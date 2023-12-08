// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

//> using scala 3.4.0-RC1-bin-20231114-18ada51-NIGHTLY
//> using mainClass kse.generators.LabelledTuples

package kse.generators

object LabelledTuples {
  val output = collection.mutable.ArrayBuffer.empty[String]

  def genP(params: Int, seek: Array[Int], got: Array[Int], indents: Int, onlyTypes: Boolean = false): Unit =
    val i = got.length
    val indent = "  "*indents
    val nt = "L" + ('z' - i).toChar
    val nv = "l" + ('z' - i).toChar
    if params <= i then
        if seek.nonEmpty then
          got.zipWithIndex.foreach{ (j, k) =>
            val relevant = seek.filter(_ > j)
            if relevant.length > 0 then
              val msg = if onlyTypes then "" else s"\" with \" + codeOf(l${('z'-k).toChar})"
              val tps = (("L"+('z'-k).toChar) +: relevant.map(x => "L" + ('a'+x).toChar)).mkString(", ")
              val ixs = (j +: relevant).map(x => s"\"_${x+1}\"".toString).mkString(", ")
              if relevant.length == 1 then
                output += s"${indent}LabelConflicts.diff[$tps]($ixs)($msg)"
              else
                output += s"${indent}LabelConflicts.head${relevant.length+1}[$tps]($ixs)($msg)"
          }
        val result =
          if seek.isEmpty && got.toList == List.range(0, got.length) then
            got.map{ i =>
              val tp = ('A' + i).toChar.toString
              val lb = "L" + ('z' - i).toChar
              s"$tp \\^ $lb"
            }.mkString("q.asInstanceOf[(", ", ", ")]")
          else if got.length > 1 then
            val lbs = got.indices.map(k => "L" + ('z' - k).toChar).mkString(".label[", ", ", "]")
            val tpl = got.map{ case j => s"q._${j+1}.unlabel" }.mkString("(", ", ", ")")
            tpl + lbs
          else s"q._${got(0)+1}.asInstanceOf[(${('A' + got(0)).toChar.toString} \\^ Lz)]"
        output += s"$indent$result"
    else
      output += s"${indent}summonFrom:"
      seek.zipWithIndex.foreach{ case (j, h) =>
        val order = got :+ j
        val ot = "L" + ('a'+j).toChar
        output += s"$indent  case _: ($nt =:= $ot) =>"
        genP(params, seek.patch(h, Nil, 1), order, indents + 2, onlyTypes)
      }
      val errmsg =
        if onlyTypes then s"No matching label for result position _${i+1}\"}"
        else s"\"No label found matching \" + codeOf($nv)"
      output += s"$indent  case _ => compiletime.error($errmsg)"

  def picks(rg: Range, arity: Int, tooBig: (Int, Int) => Boolean): Unit = for i <- rg do
    val vts = Array.tabulate(arity)(k => ('A' + k).toChar.toString)
    val lts = Array.tabulate(arity)(k => "L" + ('a' + k).toChar)
    val nlts = Array.tabulate(i)(j => "L" + ('z' - j).toChar)
    val nlvs = Array.tabulate(i)(j => "l" + ('z' - j).toChar)
    val typearg = nlts.map(_ + " <: LabelVal").mkString(", ")
    val args = (nlvs zip nlts).map{ case (lv, lt) => s"inline $lv: $lt" }.mkString(", ")
    val orts = vts.mkString(" | ")
    val rets = nlts.map{ nlt => s"($orts) \\^ $nlt" }.mkString(", ")
    output += s"  transparent inline def pick[$typearg]($args): ($rets) ="
    if !tooBig(arity, i) then
      if i > 1 then
        output += s"    LabelConflicts.unik$i(${nlvs.mkString(", ")})"
      genP(i, Array.range(0, rg.last), Array.empty[Int], 2)
    else
      output += s"    compiletime.error(\"Implementation restrictions prevent ${i}-way picking by name\")"
    output += ""


  def genU(params: Int, pending: Array[Int], targets: Array[Int], indents: Int): Unit =
    val indent = "  "*indents
    if pending.isEmpty then
      for i <- targets.indices do
        if targets(i) == 0 then targets(i) = i+1
    val i = targets.indexOf(0)
    if i < 0 then
      if targets.toList == List.range(1, targets.length+1) then
        output += s"${indent}compiletime.error(\"No matching labels so no update possible\")"
      else
        val replaced = targets.zipWithIndex.filter(_._1 < 0)
        if replaced.length > 1 then
          val tps = replaced.map{ case (_, j) => "L" + ('a' + j).toChar }.mkString(", ")
          val ixs = replaced.map{ case (_, j) => s"\"_${j+1}\"" }.mkString(", ")
          output += s"${indent}LabelConflicts.unyq${replaced.length}[$tps]($ixs)"
        for (t, i) <- replaced do
          val untested = targets.drop(i).filter(_ > 0)
          if untested.nonEmpty then
            val tps = ((i+1) +: untested).map(j => "L" + ('a' + j - 1).toChar).mkString(", ")
            val ixs = ((i+1) +: untested).map(j => s"\"_$j\"").mkString(", ")
            val method = if untested.length == 1 then "diff" else "head" + (1+untested.length).toString
            output += s"${indent}LabelConflicts.$method[$tps]($ixs)(\"\")"
        val tup = targets.map(i => if i > 0 then s"q._$i" else if params == 1 then "source" else s"source._${-i}").mkString("(", ", ", ")")
        output += s"$indent$tup"
    else
      output += s"${indent}summonFrom:"
      if pending.length > 1 || i+1 == targets.length then
        val ot = "L" + ('a' + i).toChar
        for (j, k) <- pending.zipWithIndex do
          val pt = "T" + ('z' - j).toChar
          output += s"$indent  case _: ((Nothing \\^ $ot) <:< $pt) =>"
          genU(params, pending.patch(k, Nil, 1), targets.updated(i, -j-1), indents+2)
        output += s"$indent  case _ =>"
        genU(params, pending, targets.updated(i, i+1), indents+2)
      else
        for ii <- (i until targets.length) do
          val ot = "L" + ('a' + ii).toChar
          val pt = "T" + ('z' - pending(0)).toChar
          output += s"$indent  case _: ((Nothing \\^ $ot) <:< $pt) => "
          genU(params, pending.take(0), targets.updated(ii, -pending(0)-1), indents+2)
          targets(ii) = ii+1
        output += s"$indent  case _ =>"
        genU(params, pending, targets, indents+2)


  def updas(rg: Range, arity: Int, tooBig: (Int, Int) => Boolean): Unit = for i <- rg do
    val vts = Array.tabulate(arity)(k => ('A' + k).toChar.toString)
    val lts = Array.tabulate(arity)(k => "L" + ('a' + k).toChar)
    val nlts = Array.tabulate(i)(j => "T" + ('z' - j).toChar)
    val typearg = nlts.mkString(", ")
    val typeors = nlts.mkString(" | ")
    val args = "source: " + (if i == 1 then typearg else s"($typearg)")
    val rets = (vts zip lts).map{ case (vt, lt) => s"$vt \\^ $lt | $typeors" }.mkString(", ")
    output += s"  transparent inline def updatedBy[$typearg]($args): ($rets) ="
    if !tooBig(arity, i) then
      genU(i, Array.range(0, i), Array.fill(arity)(0), 2)
    else
      output += s"  compiletime.error(\"Implementation restrictions prevent name-update of ${arity}-tuple by ${i}-tuple\")"
    output += ""


  def pickset(arity: Int, label: String, tooBig: (Int, Int) => Boolean = (a, i) => a*i > 21): Unit =
    val n = output.length
    picks(1 to arity, arity, tooBig)
    println(s"Created ${output.length - n} lines of source for arity $label of pick")

  def updaset(arity: Int, label: String, tooBig: (Int, Int) => Boolean = (a, i) => a*i > 18): Unit =
    val n = output.length
    updas(1 to 9, arity, tooBig)
    println(s"Created ${output.length - n} lines of source for arity $label of updatedBy")

  def allset(arity: Int, label: String): Unit =
    output += ""
    output += ""
    output += s"===== $label ====="
    output += ""
    if arity > 2 then pickset(arity, label)
    if arity > 1 then updaset(arity, label)

  def main(args: Array[String]): Unit =
    allset(2, "TWO")
    allset(3, "THREE")
    allset(4, "FOUR")
    allset(5, "FIVE")
    allset(6, "SIX")
    allset(7, "SEVEN")
    allset(8, "EIGHT")
    allset(9, "NINE")
    args.length match
      case 0 =>
        println(s"Created ${output.length} lines of source total.")
        println( "To save the output, run again with a filename as the only argument.")
      case 1 =>
        val p = java.nio.file.Paths.get(args(0))
        java.nio.file.Files.writeString(p, output.mkString("\n"))
      case 2 =>
        println(s"Created ${output.length} lines of source total.")
        println( "To save the output, run again with a filename as the only argument.")
        println(s"Too many arguments specified (${args.length}):\n${args.mkString("  ", "\n  ", "\n")}")

}
