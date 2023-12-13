// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

//> using scala 3.4.0-RC1-bin-20231114-18ada51-NIGHTLY
//> using mainClass kse.generators.LabelledTuples

package kse.generators

object LabelledTuples {
  val output = collection.mutable.ArrayBuffer.empty[String]

  def pickOne(params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    output += s"  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (${tps.mkString(" | ")}) \\^ Lz ="
    output += s"    summonFrom:"
    for ((t, l), i) <- (tps zip lbs).zipWithIndex do
      val pre = s"      case _: (Lz =:= $l) => "
      val ind = s"                             "
      val ans = s"q._${i+1}.asInstanceOf[$t \\^ Lz]"
      if i+1 == params then
        output += pre + ans
      else
        output += pre + s"LabelConflicts.head${params-i}[Lz, ${lbs.drop(i+1).mkString(", ")}](codeOf(lz))"
        output += ind + ans
    output += s"      case _              => compiletime.error(\"No label found matching \" + codeOf(lz))"

  def pickMany(n: Int, params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    val lbz = (0 until n).map(j => "L" + ('z' - j).toChar)
    val vrz = (0 until n).map(j => "l" + ('z' - j).toChar)
    val typeargs = lbz.map(_ + " <: LabelVal").mkString(", ")
    val args = (vrz zip lbz).map{ case (v, l) => s"inline $v: $l"}.mkString(", ")
    val ortypes = tps.mkString(" | ")
    val qtype = if n == params then "q.type | " else ""
    val restype = lbz.map(l => s"($ortypes) \\^ $l").mkString("(", ", ", ")")
    output += s"  transparent inline def pick[$typeargs]($args): $qtype$restype ="
    output += s"    LabelConflicts.uniq${lbz.length}[${lbz.mkString(", ")}](${vrz.dropRight(1).map("codeOf(" + _ + ")").mkString(", ")})"
    val in =
      if n == params then
        output += s"    summonFrom:"
        output += s"      case _: ((${lbz.mkString(", ")}) =:= (${lbs.mkString(", ")})) => q"
        output +=  "      case _ => ("
                   "        "
      else
        output +=  "    ("
                   "      "
    for (m, v) <- (lbz zip vrz) do
      output += s"${in}summonFrom:"
      for ((t, l), i) <- (tps zip lbs).zipWithIndex do
        val pre = s"$in  case _: ($m =:= $l) => "
        val ind = s"$in                         "
        val ans = s"q._${i+1}.asInstanceOf[$t \\^ $m]"
        if i+1 == params then
          output += pre + ans
        else
          output += pre + s"LabelConflicts.head${params-i}[$m, ${lbs.drop(i+1).mkString(", ")}](codeOf($v))"
          output += ind + ans
      output   += s"$in  case _              => compiletime.error(\"Label \" + codeOf($v) + \" not found\"),"
    output += in.drop(2) + ")"

  def updaOne(params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    val restype = (tps zip lbs).map{ case (t, l) =>  s"$t \\^ $l" }.mkString("(", ", ", ")")
    val misserr = "compiletime.error(\"No matching labels found\")"
    output += s"  inline def updatedBy[Z, Lz <: LabelVal](source: Z \\^ Lz): $restype ="
    output += s"    inline if !LabelConflicts.has${params}1[${(lbs :+ "Z \\^ Lz").mkString(", ")}] then $misserr"
    output +=  "    ("
    for ((t, l), i) <- (tps zip lbs).zipWithIndex do
      output +=  "      summonFrom:"
      val pre = s"        case _: (Lz =:= $l) => "
      val ind = s"                               "
      val ans = s"summonInline[Z <:< $t](source.unlabel).labelled[$l]"
      if i+1 == params then
        output += pre + ans
      else
        output += pre + s"LabelConflicts.head${params-i}[Lz, ${lbs.drop(i+1).mkString(", ")}](\"at _${i+1}\")"
        output += ind + ans
      output += s"        case _ => q._${i+1}${if i+1 == params then "" else ","}"
    output +=  "    )"

  def updaMany(n: Int, params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    val nts = (0 until n).map(j => "N" + ('z' - j).toChar)
    val typeargs = nts.mkString(", ")
    val args = nts.mkString("source: (", ",", ")")
    val restype = (tps zip lbs).map{ case (t, l) =>  s"$t \\^ $l" }.mkString("(", ", ", ")")
    val misserr = "compiletime.error(\"No matching labels found\")"
    output += s"  inline def updatedBy[$typeargs]($args): $restype ="
    output += s"    inline if !LabelConflicts.has${params}${n}[${lbs.mkString(", ")}, ${nts.mkString(", ")}] then $misserr"
    output +=  "    ("
    for ((t, l), i) <- (tps zip lbs).zipWithIndex do
      output +=  "      summonFrom:"
      for (m, j) <- nts.zipWithIndex do
        val p = s"        case _ : ((Nothing \\^ $l) <:< $m) => "
        val s =  "                                             "
        var ans = s"summonInline[($m <:< ($t \\^ $l))](source._${j+1})" :: Nil
        if j+1 < n then
          val errdup = s"compiletime.error(\"Multple labels found matching label on _${i+1}\")"
          ans = s"LabelConflicts.miss${n-j-1}[$l, ${nts.drop(j+1).mkString(", ")}](\" in update corresponding to _${i+1}\")" :: ans
        if i+1 < params then
          ans = s"LabelConflicts.head${params-i}[${lbs.drop(i).mkString(", ")}](\"at _${i+1}\")" :: ans
        for (x, k) <- ans.zipWithIndex do
          output += (if k == 0 then p else s) + x
      output += s"        case _ => q._${i+1}${if i+1 == params then "" else ","}"
    output +=  "     )"

  def revaOne(params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    val tli = (tps zip lbs).zipWithIndex
    output += s"  transparent inline def revalue[Z](inline name: ${lbs.mkString(" | ")})(to: Z):"
    for j <- 0 until params do
      output += tli.map{ case ((t, l), i) => if i == j then s"Z \\^ $l" else s"$t \\^ $l" }.mkString("    (", ", ", if j+1 == params then ")" else ") | ")
    output +=  "    ="
    output +=  "    inline name match"
    for ((t, l), i) <- tli do
      output += s"      case _: $l =>"
      if i+1 < params then
          output += s"        LabelConflicts.head${params-i}[${lbs.drop(i).mkString(", ")}](codeOf(name))"
      output += s"        q.copy(_${i+1} = to.labelled[$l])"

  def relaOne(params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    val tli = (tps zip lbs).zipWithIndex
    output += s"  transparent inline def relabel[Lz <: LabelVal](inline name: ${lbs.mkString(" | ")})(inline lz: Lz):"
    for j <- 0 until params do
      output += tli.map{ case ((t, l), i) => if i == j then s"$t \\^ Lz" else s"$t \\^ $l" }.mkString("    (", ", ", if j+1 == params then ")" else ") | ")
    output +=  "    ="
    output +=  "    inline name match"
    for ((t, l), i) <- tli do
      output += s"      case _: $l =>"
      if i+1 < params then
          output += s"        LabelConflicts.head${params-i}[${lbs.drop(i).mkString(", ")}](codeOf(name))"
      output += s"        LabelConflicts.head${params}[Lz, ${lbs.patch(i, Nil, 1).mkString(", ")}](codeOf(name) + \" to \" + codeOf(lz) + \" creates a labelling which\")"
      output += tli.map{ case ((t, l), j) => if i == j then s"$t \\^ Lz" else s"$t \\^ $l" }.mkString("        q.asInstanceOf[(", ", ", ")]")

  def redoOne(params: Int): Unit =
    val tps = (0 until params).map(i => ('A' + i).toChar.toString)
    val lbs = (0 until params).map(i => "L" + ('a' + i).toChar)
    val tli = (tps zip lbs).zipWithIndex
    output += s"  transparent inline def redo[Z, Lz <: LabelVal](inline name: ${lbs.mkString(" | ")})(inline to: Z \\^ Lz):"
    for j <- 0 until params do
      output += tli.map{ case ((t, l), i) => if i == j then s"Z \\^ Lz" else s"$t \\^ $l" }.mkString("    (", ", ", if j+1 == params then ")" else ") | ")
    output +=  "    ="
    output +=  "    inline name match"
    for ((t, l), i) <- tli do
      output += s"      case _: $l =>"
      if i+1 < params then
          output += s"        LabelConflicts.head${params-i}[${lbs.drop(i).mkString(", ")}](codeOf(name))"
      output += s"        LabelConflicts.head${params}[Lz, ${lbs.patch(i, Nil, 1).mkString(", ")}](codeOf(name) + \" changed, creating a labelling which\")"
      output += s"        q.copy(_${i+1} = to)"

  def pickset(arity: Int, label: String): Unit =
    val n = output.length
    pickOne(arity)
    output += ""
    for n <- 2 to arity do
      pickMany(n, arity)
      output += ""
    println(s"Created ${output.length - n} lines of source for arity $label of pick")

  def updaset(arity: Int, label: String): Unit =
    val n = output.length
    updaOne(arity)
    output += ""
    for n <- 2 to 9 do
      updaMany(n, arity)
      output += ""
    println(s"Created ${output.length - n} lines of source for arity $label of updatedBy")

  def revlset(arity: Int, label: String): Unit =
    val n = output.length
    revaOne(arity)
    output += ""
    relaOne(arity)
    output += ""
    redoOne(arity)
    output += ""
    println(s"Created ${output.length - n} lines of source for arity $label of updatedBy")

  def allset(arity: Int, label: String): Unit =
    output += ""
    output += ""
    output += s"===== $label ====="
    output += ""
    if arity > 1 then pickset(arity, label)
    if arity > 1 then updaset(arity, label)
    if arity > 1 then revlset(arity, label)

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
