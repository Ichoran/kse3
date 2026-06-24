// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.thyme


import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ThreadLocalRandom

import kse.basics.SourceLine
import kse.flow.Tidy
import kse.maths.MultiPradwin


/** Parsley — an embedded in-situ profiler for "is this thing slow, and is the alternative better?".
  *
  * Put one in a companion object and instrument the calls you care about:
  * {{{
  * object Prof { val parsley = Parsley() }
  * val y = Prof.parsley.time{ heavyThing(x) }                              // time a single run
  * val z = Prof.parsley.timeOff("old", "new"){ slow(x) }{ fast(x) }        // race two implementations
  * }}}
  * Timings are keyed by the source location of the call (via [[kse.basics.SourceLine]]) and summarized
  * per regime by a [[kse.maths.MultiPradwin]] — moments and quantiles for each alternative, split into
  * the regimes (warmup, steady state, …) the timing stream actually passes through.
  *
  * For `timeOff` in `"both"` mode the regime boundaries are detected on the '''ratio''' of the two
  * contemporaneous timings, so common-mode shifts (core migration, throttling, background load) that
  * move both alternatives together cancel out — only a change in their *relative* performance splits a
  * regime.  Unlike a tight microbenchmark, this measures the real program with real inputs in context.
  *
  * '''Lifetime.'''  So results are never silently lost, a Parsley registers a [[kse.flow.Tidy.Later]]
  * backstop: its `onClose` action (print by default) runs at [[close]], at JVM shutdown, or — held off
  * by a strong self-reference so it never fires early — never merely from GC while still in use.
  */
final class Parsley(
  val onClose: Parsley => Unit = Parsley.printReport,
  segAlpha: Double = 0.001,
  segEffect: Double = 0.03,
  segCadence: Int = 128,
  segCapacity: Int = 1024,
  segMinSeg: Int = 20
) {
  private val cfg = Parsley.SegCfg(segAlpha, segEffect, segCadence, segCapacity, segMinSeg)
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

  /** Record one timing (seconds) for `label` at source `site`.  Internal — used by the inline shims. */
  def recordOne(site: String, label: String, seconds: Double): Unit =
    if !shut then
      data.computeIfAbsent(site, _ => new Parsley.SinglesEntry(cfg)) match
        case e: Parsley.SinglesEntry => e.add(label, seconds)
        case _                       => ()      // site used inconsistently; ignore

  /** Record one contemporaneous pair of timings at `site`.  Internal — used by the inline shims. */
  def recordPair(site: String, aLabel: String, bLabel: String, aSeconds: Double, bSeconds: Double): Unit =
    if !shut then
      data.computeIfAbsent(site, _ => new Parsley.PairEntry(aLabel, bLabel, cfg)) match
        case e: Parsley.PairEntry => e.add(aSeconds, bSeconds)
        case _                    => ()

  /** Time a single run of `f`, recording it under this call site, and return its value. */
  inline def time[A](inline f: A)(using sl: SourceLine): A =
    val t0 = System.nanoTime
    val a = f
    recordOne(sl.toString, "", (System.nanoTime - t0) * 1e-9)
    a

  /** Race two implementations at this call site.
    *
    * In `"both"` mode (default) both run, in randomized order, each timed, and the first's value is
    * returned — use when the two are interchangeable and side-effect-free (or idempotent); regimes are
    * detected on the ratio (common-mode-immune).  In `"pick"` mode exactly one is chosen at random,
    * timed, and returned — use when only one may run.
    */
  inline def timeOff[A](aLabel: String, bLabel: String, inline mode: "both" | "pick" = "both")(inline f1: A)(inline f2: A)(using sl: SourceLine): A =
    val site = sl.toString
    inline mode match
      case "both" =>
        if ThreadLocalRandom.current().nextBoolean() then
          val t0 = System.nanoTime; val a = f1; val da = (System.nanoTime - t0) * 1e-9
          val t1 = System.nanoTime; val b = f2; val db = (System.nanoTime - t1) * 1e-9
          recordPair(site, aLabel, bLabel, da, db)
          a
        else
          val t1 = System.nanoTime; val b = f2; val db = (System.nanoTime - t1) * 1e-9
          val t0 = System.nanoTime; val a = f1; val da = (System.nanoTime - t0) * 1e-9
          recordPair(site, aLabel, bLabel, da, db)
          a
      case "pick" =>
        if ThreadLocalRandom.current().nextBoolean() then
          val t0 = System.nanoTime; val a = f1; recordOne(site, aLabel, (System.nanoTime - t0) * 1e-9); a
        else
          val t0 = System.nanoTime; val b = f2; recordOne(site, bLabel, (System.nanoTime - t0) * 1e-9); b

  /** A snapshot of all measurements: each source site, with one [[kse.maths.MultiPradwin.Track]] per
    * alternative (and, for a `"both"` site, the `"ratio"` channel carrying the relative regimes). */
  def results: Vector[(String, Vector[MultiPradwin.Track])] =
    val sites = new java.util.ArrayList[String](data.keySet)
    sites.sort(null)
    val out = Vector.newBuilder[(String, Vector[MultiPradwin.Track])]
    sites.forEach(s => out += s -> data.get(s).tracks)
    out.result()
}
object Parsley {
  private case class SegCfg(alpha: Double, effect: Double, cadence: Int, capacity: Int, minSeg: Int)

  private sealed abstract class Entry { def tracks: Vector[MultiPradwin.Track] }

  // `time` / `"pick"`: each label is an independent single-channel stream.
  private final class SinglesEntry(cfg: SegCfg) extends Entry {
    private val mps = new java.util.LinkedHashMap[String, MultiPradwin]()   // first-seen order
    def add(label: String, t: Double): Unit = this.synchronized:
      var mp = mps.get(label)
      if mp eq null then
        mp = MultiPradwin(Array(label))(label, _(0), cfg.alpha, cfg.effect, cfg.cadence, cfg.capacity, cfg.minSeg)
        mps.put(label, mp): Unit
      mp.add(Array(t))
    def tracks: Vector[MultiPradwin.Track] = this.synchronized:
      val b = Vector.newBuilder[MultiPradwin.Track]
      mps.forEach((_, mp) => b += mp.tracks.head)   // the input channel (score is the identity duplicate)
      b.result()
  }

  // `timeOff "both"`: two contemporaneous channels with regimes detected on their ratio.
  private final class PairEntry(aLabel: String, bLabel: String, cfg: SegCfg) extends Entry {
    private val mp = MultiPradwin(Array(aLabel, bLabel))(
      "ratio", xs => xs(1) / math.max(xs(0), 1e-9),
      cfg.alpha, cfg.effect, cfg.cadence, cfg.capacity, cfg.minSeg
    )
    def add(a: Double, b: Double): Unit = mp.add(Array(a, b))
    def tracks: Vector[MultiPradwin.Track] = mp.tracks
  }

  /** A Parsley whose formatted closing report is handed to `sink` — e.g. `Parsley.onClose(println)`
    * prints it at [[Parsley.close]] or JVM shutdown; `Parsley.onClose(s => Files.writeString(path, s))`
    * saves it. */
  def onClose(sink: String => Unit): Parsley = new Parsley(p => sink(formatReport(p)))

  /** Default `onClose`: print a per-site, per-regime report to stdout. */
  def printReport(p: Parsley): Unit = System.out.print(formatReport(p))

  /** The per-site, per-regime report as a string. */
  def formatReport(p: Parsley): String =
    val rs = p.results
    val sb = new StringBuilder
    sb ++= s"Parsley: ${rs.length} site(s)\n"
    for (site, tracks) <- rs do
      sb ++= s"  $site\n"
      for tr <- tracks do
        val tag = if tr.label.isEmpty then "" else s"[${tr.label}] "
        val o = tr.overall
        if tr.label == "ratio" then
          sb ++= f"    [ratio = ${tracks(1).label}/${tracks(0).label}] median ${o.median}%.3f×   p90 ${o.q90}%.3f×\n"
        else
          sb ++= f"    $tag${o.n}%d calls   median ${Thyme.humanTime(o.median)}%s   mean ${Thyme.humanTime(o.mean)}%s   p90 ${Thyme.humanTime(o.q90)}%s\n"
        if tr.segments.length > 1 then
          val fmt: MultiPradwin.Stat => String =
            if tr.label == "ratio" then s => f"${s.n}×${s.median}%.3f×" else s => s"${s.n}×${Thyme.humanTime(s.median)}"
          sb ++= s"      regimes: ${tr.segments.map(fmt).mkString(" → ")}\n"
    sb.result()
}
