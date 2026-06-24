// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.thyme


import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ThreadLocalRandom

import kse.basics.SourceLine
import kse.flow.Tidy
import kse.maths.{Est, UDDSketch}


/** Parsley — an embedded in-situ profiler for "is this thing slow, and is the alternative better?".
  *
  * Put one in a companion object and instrument the calls you care about:
  * {{{
  * object Prof { val parsley = Parsley() }
  * // …
  * val y = Prof.parsley.time{ heavyThing(x) }                              // time a single run
  * val z = Prof.parsley.timeOff("old", "new"){ slow(x) }{ fast(x) }        // race two implementations
  * }}}
  * Timings are keyed by the source location of the call (via [[kse.basics.SourceLine]]); each
  * distinct alternative at a site accumulates its own statistics ([[kse.maths.Est]] moments and a
  * [[kse.maths.UDDSketch]] for quantiles).  Unlike a tight microbenchmark, this measures the real
  * program with real inputs in real context — ideal for larger work units where extracting the
  * subtask into a harness is impractical.
  *
  * '''Lifetime.'''  So results are never silently lost, a Parsley registers a [[kse.flow.Tidy.Later]]
  * backstop: its `onClose` action (print by default) runs at [[close]], at JVM shutdown, or — held off
  * by a strong self-reference so it never fires early — never merely from GC while still in use.
  */
final class Parsley(val onClose: Parsley => Unit = Parsley.printReport) {
  private val data = new ConcurrentHashMap[String, Parsley.Entry]()
  @volatile private var shut = false

  // A strong reference to our own cleanup backstop: keeps the Later reachable so its GC-triggered
  // cleanup can't fire while we're still recording; the shutdown hook and close() still can.
  private var keepAlive: Tidy.Later[Parsley] = null
  locally:
    val clean = new Tidy.Clean[Parsley] { def apply(p: Parsley): Unit = p.runClose() }
    keepAlive = Tidy.Later(this)(using clean)

  private def runClose(): Unit = this.synchronized:
    if !shut then
      shut = true
      try onClose(this)
      catch case e: Throwable => System.err.println(s"Parsley onClose failed: $e")

  /** Run the `onClose` action now (idempotent) and release the shutdown backstop. */
  def close(): Unit =
    val k = keepAlive
    if k ne null then k.close()   // triggers runClose via the cleanup, and unregisters the backstop
    else runClose()

  /** Record one timing (seconds) for `label` at source location `site`.  Internal — used by the
    * inline timing shims; prefer [[time]] / [[timeOff]]. */
  def record(site: String, label: String, seconds: Double): Unit =
    if !shut then data.computeIfAbsent(site, _ => new Parsley.Entry()).accum(label).add(seconds)

  /** Time a single run of `f`, recording it under this call site, and return its value. */
  inline def time[A](inline f: A)(using sl: SourceLine): A =
    val t0 = System.nanoTime
    val a = f
    record(sl.toString, "", (System.nanoTime - t0) * 1e-9)
    a

  /** Race two implementations at this call site.
    *
    * In `"both"` mode (default) both run, in randomized order, each timed, and the first's value is
    * returned — use when the two are interchangeable and side-effect-free (or idempotent).  In
    * `"pick"` mode exactly one is chosen at random, timed, and returned — use when only one may run.
    * The random order/choice decorrelates the measurement from program phase.
    */
  inline def timeOff[A](aLabel: String, bLabel: String, inline mode: "both" | "pick" = "both")(inline f1: A)(inline f2: A)(using sl: SourceLine): A =
    val site = sl.toString
    inline mode match
      case "both" =>
        if ThreadLocalRandom.current().nextBoolean() then
          val t0 = System.nanoTime; val a = f1; record(site, aLabel, (System.nanoTime - t0) * 1e-9)
          val t1 = System.nanoTime; val b = f2; record(site, bLabel, (System.nanoTime - t1) * 1e-9)
          a
        else
          val t1 = System.nanoTime; val b = f2; record(site, bLabel, (System.nanoTime - t1) * 1e-9)
          val t0 = System.nanoTime; val a = f1; record(site, aLabel, (System.nanoTime - t0) * 1e-9)
          a
      case "pick" =>
        if ThreadLocalRandom.current().nextBoolean() then
          val t0 = System.nanoTime; val a = f1; record(site, aLabel, (System.nanoTime - t0) * 1e-9); a
        else
          val t0 = System.nanoTime; val b = f2; record(site, bLabel, (System.nanoTime - t0) * 1e-9); b

  /** A snapshot of all measurements: each source site, with its alternatives in first-seen order. */
  def results: Vector[(String, Vector[(String, Parsley.Stat)])] =
    val sites = new java.util.ArrayList[String](data.keySet)
    sites.sort(null)
    val out = Vector.newBuilder[(String, Vector[(String, Parsley.Stat)])]
    sites.forEach(s => out += s -> data.get(s).snapshot)
    out.result()
}
object Parsley {
  /** Summary statistics for one alternative at one site. */
  case class Stat(n: Long, mean: Double, sd: Double, median: Double, q90: Double, q99: Double)

  private final class Accum {
    private val est = new Est.M(0, 0, 0)
    private val sketch = UDDSketch(0.01)
    def add(seconds: Double): Unit = this.synchronized:
      est += seconds
      sketch += seconds
    def snapshot: Stat = this.synchronized:
      Stat(math.round(est.n), est.mean, est.sd, sketch.median, sketch.quantile(0.9), sketch.quantile(0.99))
  }

  private final class Entry {
    private val alts = new java.util.LinkedHashMap[String, Accum]()   // first-seen order
    def accum(label: String): Accum = this.synchronized:
      var a = alts.get(label)
      if a eq null then { a = new Accum(); alts.put(label, a): Unit }
      a
    def snapshot: Vector[(String, Stat)] = this.synchronized:
      val b = Vector.newBuilder[(String, Stat)]
      alts.forEach((k, v) => b += k -> v.snapshot)
      b.result()
  }

  /** Default `onClose`: print a per-site report to stdout. */
  def printReport(p: Parsley): Unit =
    val rs = p.results
    val sb = new StringBuilder
    sb ++= s"Parsley: ${rs.length} site(s)\n"
    for (site, alts) <- rs do
      sb ++= s"  $site\n"
      for (label, st) <- alts do
        val tag = if label.isEmpty then "" else s"[$label] "
        sb ++= f"    $tag${st.n}%d calls   median ${Thyme.humanTime(st.median)}%s   mean ${Thyme.humanTime(st.mean)}%s   p90 ${Thyme.humanTime(st.q90)}%s\n"
      if alts.length == 2 then
        val (la, sa) = alts(0)
        val (lb, sbb) = alts(1)
        if sa.median > 0 then
          val ratio = sbb.median / sa.median
          val verdict = if ratio < 1 then f"$lb faster (${1.0 / ratio}%.2f×)" else f"$la faster ($ratio%.2f×)"
          sb ++= f"      → $verdict by median\n"
    System.out.print(sb.result())
}
