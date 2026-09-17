// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.guides


import java.nio.file.Path

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}

import kse.basics.{given, *}
import kse.flow.{given, *}
import kse.eio.{given, *}


/** Verifies that the GUIDE files tell the truth about the code they quote.
  *
  * A guide names its example files near the top,
  * {{{
  * <!-- guide examples: basics/test/src/GuideExamples.scala -->
  * }}}
  * and labels each fenced block it wants checked:
  * {{{
  * <!-- guide: arrays.plain -->
  * ```scala
  * ...
  * ```
  * }}}
  * The Scala side brackets the same lines between `// guide: arrays.plain` and `// guide: end`; the
  * common leading indentation is stripped from the Scala side before the two are compared.  Beyond
  * the examples, every guide's "What's here" list must name its `## ` sections in order, and every
  * `GUIDE-x.md § Section` reference must name a section that exists.
  *
  * Differences are reported, never repaired: when the two sides disagree, read both and decide which
  * is right.  Run as `mill guides.run` from the repository root; it takes no arguments, a root
  * directory, or the guide files to check, and exits with status 1 if there is any problem.
  */
object CheckGuides {

  /** What the check found: problems fail the run, notes are only printed. */
  final class Findings {
    val problems = ArrayBuffer.empty[String]
    val notes = ArrayBuffer.empty[String]
    def problem(s: String): Unit = problems.addOne(s) __ Unit
    def note(s: String): Unit = notes.addOne(s) __ Unit
  }

  /** One example: the line it starts on in its file, and its text. */
  final case class Example(at: Int, text: Vector[String])

  private val Summary = "What's here"

  private val Ref = """(GUIDE[-_][\w-]+\.md) § ([^|`\n]+?)\s*(?=\||$)""".r


  def main(args: Array[String]): Unit =
    val named = args.map(_.path)
    val guides =
      if named.length == 0 then guidesIn(".".path)
      else if named.length == 1 && named(0).isDirectory then guidesIn(named(0))
      else named
    if check(guides) > 0 then System.exit(1)

  /** The guide files under `root`, core ones first, in name order. */
  def guidesIn(root: Path): Array[Path] =
    root.paths.select(p => p.ext == "md" && (p.name.startsWith("GUIDE-") || p.name.startsWith("GUIDE_"))).sortBy(_.name)

  /** Checks every guide, prints what it found, and answers the number of problems. */
  def check(guides: Array[Path]): Int =
    val f = Findings()
    val unused = ArrayBuffer.empty[String]
    guides.visit(): (md, _) =>
      md.slurp.fold{ lines =>
        summaryMatches(md, lines, f)
        sectionRefs(md, lines, f)
        val (files, wanted) = mdExamples(md, lines, f)
        if files.isEmpty then
          if wanted.nonEmpty then f.problem(s"$md: has labeled examples but no '<!-- guide examples: ... -->' line")
        else
          val root = md.parent.getOrElse(_ => ".".path)
          val have = LinkedHashMap.empty[String, (Path, Example)]
          files.visit(): (file, _) =>
            val p = root / file
            p.slurp.fold{ ls =>
              for (name, ex) <- scalaExamples(p, ls, f) do
                if have.contains(name) then f.problem(s"$p: example '$name' also defined in an earlier file")
                else have(name) = (p, ex)
            }{ e => f.problem(s"$md: examples file $file cannot be read: ${e.toString.linesIterator.next()}") }
          for (name, ex) <- wanted do
            have.get(name) match
              case Some((p, sx)) => if ex.text != sx.text then f.problem(differ(name, md, ex, p, sx))
              case None => f.problem(s"$md:${ex.at}: example '$name' has no Scala twin")
          for (name, (p, sx)) <- have if !wanted.contains(name) do
            unused.addOne(s"$p:${sx.at}: example '$name' is not shown in ${md.name}") __ Unit
      }{ e => f.problem(s"$md: cannot be read: ${e.toString.linesIterator.next()}") }
    unused.foreach(u => println(s"note: $u"))
    f.notes.foreach(n => println(s"note: $n"))
    f.problems.foreach(p => println(s"PROBLEM: $p"))
    println(say"${guides.length}# guide//s# checked, ${f.problems.length}# problem//s#")
    f.problems.length


  // === The three checks ===

  /** Every bold entry under "What's here" must be a `## ` section, every section must be listed, and in the same order. */
  private def summaryMatches(md: Path, lines: Array[String], f: Findings): Unit =
    val heads = ArrayBuffer.empty[(String, Int)]
    val entries = ArrayBuffer.empty[(String, Int)]
    var inSummary = false
    var hasSummary = false
    lines.visit(): (line, i) =>
      if line.startsWith("## ") then
        val h = line.drop(3).trim
        inSummary = h == Summary
        if inSummary then hasSummary = true
        else heads.addOne((h, i + 1)) __ Unit
      else if inSummary && line.startsWith("- **") then
        val j = line.indexOf("**", 4)
        if j > 4 then entries.addOne((line.substring(4, j), i + 1)) __ Unit
    if heads.nonEmpty && hasSummary then
      val hs = heads.map(_._1).toSet
      val es = entries.map(_._1).toSet
      for (e, n) <- entries if !hs(e) do f.problem(s"$md:$n: summary entry '$e' has no matching '## $e' section")
      for (h, n) <- heads if !es(h) do f.problem(s"$md:$n: section '$h' is not listed under '## $Summary'")
      if entries.map(_._1).filter(hs) != heads.map(_._1).filter(es) then
        f.problem(s"$md: summary lists the sections in a different order than they appear")

  /** A `GUIDE-x.md § Section` reference must name a section that exists, once that guide exists. */
  private def sectionRefs(md: Path, lines: Array[String], f: Findings): Unit =
    val root = md.parent.getOrElse(_ => ".".path)
    lines.visit(): (line, i) =>
      for m <- Ref.findAllMatchIn(line) do
        val guide = m.group(1)
        val section = m.group(2)
        val target = root / guide
        if !target.exists then f.note(s"$md:${i + 1}: refers to $guide, which does not exist yet")
        else target.slurp.fold{ ls =>
          val heads = ls.select(_.startsWith("## ")).map(_.drop(3).trim)
          if !heads.contains(section) then
            f.problem(s"$md:${i + 1}: refers to '$guide § $section', but that guide has no '## $section'")
        }{ e => f.problem(s"$md:${i + 1}: refers to $guide, which cannot be read: ${e.toString.linesIterator.next()}") }

  /** The example files a guide names, and its labeled fenced blocks. */
  private def mdExamples(md: Path, lines: Array[String], f: Findings): (Array[String], LinkedHashMap[String, Example]) =
    val files = ArrayBuffer.empty[String]
    val out = LinkedHashMap.empty[String, Example]
    val buf = ArrayBuffer.empty[String]
    var pendingName = ""
    var pendingAt = 0
    var fence = false
    var start = 0
    lines.visit(): (line, i) =>
      val n = i + 1
      if !fence then
        val file = examplesFileIn(line)
        val name = if file.isEmpty then labelIn(line) else ""
        if file.nonEmpty then files.addOne(file) __ Unit
        else if name.nonEmpty then
          pendingName = name
          pendingAt = n
        else if line.startsWith("```") then
          fence = true
          buf.clear()
          start = n
          if pendingName.nonEmpty && pendingAt != n - 1 then
            f.problem(s"$md:$pendingAt: label '$pendingName' is not directly above a code fence")
        else if pendingName.nonEmpty then
          f.problem(s"$md:$pendingAt: label '$pendingName' is not directly above a code fence")
          pendingName = ""
      else if line.startsWith("```") then
        fence = false
        if pendingName.nonEmpty then
          if out.contains(pendingName) then f.problem(s"$md:$start: example '$pendingName' labeled twice")
          out(pendingName) = Example(start, buf.toArray.map(rstrip).toVector)
          pendingName = ""
      else buf.addOne(line) __ Unit
    (files.toArray, out)

  /** The bracketed examples in a Scala file, dedented. */
  private def scalaExamples(p: Path, lines: Array[String], f: Findings): LinkedHashMap[String, Example] =
    val out = LinkedHashMap.empty[String, Example]
    val buf = ArrayBuffer.empty[String]
    var name = ""
    var start = 0
    lines.visit(): (line, i) =>
      val n = i + 1
      val m = markerIn(line)
      if m == "end" then
        if name.isEmpty then f.problem(s"$p:$n: 'guide: end' with no open example")
        else
          if out.contains(name) then f.problem(s"$p:$n: example '$name' defined twice")
          out(name) = Example(start, dedent(buf.toArray))
          name = ""
      else if m.nonEmpty then
        if name.nonEmpty then f.problem(s"$p:$n: example '$m' opened inside '$name'")
        name = m
        start = n
        buf.clear()
      else if name.nonEmpty then buf.addOne(rstrip(line)) __ Unit
    if name.nonEmpty then f.problem(s"$p:$start: example '$name' never closed")
    out


  // === Line-level helpers ===

  /** The path in a `<!-- guide examples: path -->` line, or "". */
  private def examplesFileIn(line: String): String =
    val i = line.indexOf("<!-- guide examples:")
    val j = if i < 0 then -1 else line.indexOf("-->", i)
    if j < 0 then "" else line.substring(i + 20, j).trim

  /** The name in a `<!-- guide: name -->` line, or "". */
  private def labelIn(line: String): String =
    val i = line.indexOf("<!-- guide:")
    val j = if i < 0 then -1 else line.indexOf("-->", i)
    if j < 0 then "" else line.substring(i + 11, j).trim.fixIf(!isName(_))(_ => "")

  /** The name in a `// guide: name` line ("end" for the closer), or "". */
  private def markerIn(line: String): String =
    val t = line.trim
    if !t.startsWith("//") then ""
    else
      val u = t.drop(2).trim
      if !u.startsWith("guide:") then "" else u.drop(6).trim.fixIf(!isName(_))(_ => "")

  private def isName(s: String): Boolean =
    s.nonEmpty && s.forall(c => c.isLetterOrDigit || c == '.' || c == '_' || c == '-')

  private def rstrip(s: String): String =
    var e = s.length
    while e > 0 && s.charAt(e - 1).isWhitespace do e -= 1
    s.substring(0, e)

  private def indentOf(s: String): Int =
    var i = 0
    while i < s.length && s.charAt(i).isWhitespace do i += 1
    i

  /** Strips the common leading whitespace of the non-blank lines; blank lines become empty. */
  private def dedent(ls: Array[String]): Vector[String] =
    val filled = ls.select(_.trim.nonEmpty)
    val k = if filled.length == 0 then 0 else filled.map(indentOf).min
    ls.map(l => if l.trim.isEmpty then "" else l.drop(k)).toVector

  /** Where two versions of an example first diverge, with the lines that differ. */
  private def differ(name: String, md: Path, ex: Example, p: Path, sx: Example): String = MkStr: m =>
    m += s"example '$name' differs (read both, decide which is right): $md:${ex.at} vs $p:${sx.at}"
    if ex.text.length != sx.text.length then
      m += say"\n  ${ex.text.length}# line//s# in the guide, ${sx.text.length} in the code"
    val n = ex.text.length min sx.text.length
    var k = 0
    var shown = 0
    while k < n && shown < 3 do
      if ex.text(k) != sx.text(k) then
        m += s"\n  line ${k + 1}\n    guide: ${ex.text(k)}\n    scala: ${sx.text(k)}"
        shown += 1
      k += 1
    if shown == 0 && ex.text.length != sx.text.length then
      m += (if ex.text.length > n then s"\n  guide continues: ${ex.text(n)}" else s"\n  scala continues: ${sx.text(n)}")
}
