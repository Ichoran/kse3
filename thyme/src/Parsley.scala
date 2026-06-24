// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.thyme


import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable.ArrayBuffer

import kse.basics.SourceLine
import kse.flow.Tidy
import kse.maths.{Est, UDDSketch, Pradwin}


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
  * '''Regimes.'''  Real timings drift — JIT warmup, GC, contention.  A per-alternative
  * [[kse.maths.Pradwin]] watches each timing stream and, on a significant changepoint, closes the
  * current segment into a ledger and starts a fresh one, so a report distinguishes (say) a slow
  * warmup regime from steady state instead of blending them.  An `overall` summary is always kept too.
  *
  * '''Lifetime.'''  So results are never silently lost, a Parsley registers a [[kse.flow.Tidy.Later]]
  * backstop: its `onClose` action (print by default) runs at [[close]], at JVM shutdown, or — held off
  * by a strong self-reference so it never fires early — never merely from GC while still in use.
  */
final class Parsley(
  val onClose: Parsley => Unit = Parsley.printReport,
  segAlpha: Double = 0.001,
  segCadence: Int = 128,
  segCapacity: Int = 1024,
  segMinSeg: Int = 20
) {
  private val cfg = Parsley.Config(segAlpha, jm_max(segCadence, 1), jm_max(segCapacity, 8), jm_max(segMinSeg, 2))
  private inline def jm_max(a: Int, b: Int): Int = if a > b then a else b

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
    if !shut then data.computeIfAbsent(site, _ => new Parsley.Entry(cfg)).accum(label).add(seconds)

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

  /** A snapshot of all measurements: each source site, with its alternatives (first-seen order),
    * each carrying its per-regime segments and an overall summary. */
  def results: Vector[(String, Vector[(String, Parsley.Track)])] =
    val sites = new java.util.ArrayList[String](data.keySet)
    sites.sort(null)
    val out = Vector.newBuilder[(String, Vector[(String, Parsley.Track)])]
    sites.forEach(s => out += s -> data.get(s).snapshot)
    out.result()
}
object Parsley {
  private final case class Config(alpha: Double, cadence: Int, capacity: Int, minSeg: Int)

  /** Summary statistics for one segment (or the overall) of one alternative at one site. */
  case class Stat(n: Long, mean: Double, sd: Double, median: Double, q90: Double, q99: Double)

  /** One alternative's record: per-regime `segments` (the last is the still-open one) and `overall`. */
  case class Track(segments: Vector[Stat], overall: Stat)

  private def statOf(e: Est, s: UDDSketch): Stat =
    Stat(math.round(e.n), e.mean, e.sd, s.median, s.quantile(0.9), s.quantile(0.99))

  private final class Accum(cfg: Config) {
    private var segEst = new Est.M(0, 0, 0)                                 // current (open) segment
    private var segSketch = UDDSketch(0.01, maxBuckets = 512, sparseBuckets = 32)
    private val allEst = new Est.M(0, 0, 0)                                 // overall (never reset)
    private val allSketch = UDDSketch(0.01, maxBuckets = 512, sparseBuckets = 32)
    private val pradwin = Pradwin(cfg.capacity, cfg.minSeg, cfg.alpha)
    private val segments = ArrayBuffer.empty[Stat]                          // closed regimes
    private var sinceCheck = 0

    def add(seconds: Double): Unit = this.synchronized:
      segEst += seconds; segSketch += seconds
      allEst += seconds; allSketch += seconds
      pradwin.add(seconds)
      sinceCheck += 1
      if sinceCheck >= cfg.cadence then
        sinceCheck = 0
        if pradwin.size >= 2 * cfg.minSeg && pradwin.locate().significant then
          segments += statOf(segEst, segSketch)
          segEst = new Est.M(0, 0, 0)
          segSketch = UDDSketch(0.01, maxBuckets = 512, sparseBuckets = 32)
          pradwin.clear()

    def track: Track = this.synchronized:
      Track((segments.toVector :+ statOf(segEst, segSketch)), statOf(allEst, allSketch))
  }

  private final class Entry(cfg: Config) {
    private val alts = new java.util.LinkedHashMap[String, Accum]()   // first-seen order
    def accum(label: String): Accum = this.synchronized:
      var a = alts.get(label)
      if a eq null then { a = new Accum(cfg); alts.put(label, a): Unit }
      a
    def snapshot: Vector[(String, Track)] = this.synchronized:
      val b = Vector.newBuilder[(String, Track)]
      alts.forEach((k, v) => b += k -> v.track)
      b.result()
  }

  /** Default `onClose`: print a per-site, per-regime report to stdout. */
  def printReport(p: Parsley): Unit =
    val rs = p.results
    val sb = new StringBuilder
    sb ++= s"Parsley: ${rs.length} site(s)\n"
    for (site, alts) <- rs do
      sb ++= s"  $site\n"
      for (label, tr) <- alts do
        val tag = if label.isEmpty then "" else s"[$label] "
        val o = tr.overall
        sb ++= f"    $tag${o.n}%d calls   median ${Thyme.humanTime(o.median)}%s   mean ${Thyme.humanTime(o.mean)}%s   p90 ${Thyme.humanTime(o.q90)}%s\n"
        if tr.segments.length > 1 then
          val regimes = tr.segments.map(s => s"${s.n}×${Thyme.humanTime(s.median)}").mkString(" → ")
          sb ++= s"      regimes: $regimes\n"
      if alts.length == 2 then
        val (la, ta) = alts(0)
        val (lb, tb) = alts(1)
        if ta.overall.median > 0 then
          val ratio = tb.overall.median / ta.overall.median
          val verdict = if ratio < 1 then f"$lb faster (${1.0 / ratio}%.2f×)" else f"$la faster ($ratio%.2f×)"
          sb ++= f"      → $verdict by median\n"
    System.out.print(sb.result())
}
