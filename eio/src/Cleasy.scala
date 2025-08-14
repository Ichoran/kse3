// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.eio.cleasy


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.compiletime.{constValue, summonFrom}
import scala.compiletime.ops.any.ToString

import java.nio.file.Path

import scala.util.boundary

import kse.basics.*
import kse.basics.intervals.{Iv, End}
import kse.flow.*
import kse.maths.*
import kse.eio.*


type CharVal = Char & Singleton

trait Parse[A] {
  def apply(s: String): Ask[A]
  def argument: Parse.ArgStyle = Parse.ArgStyle.Always
  def userString: String = "value"
  final def maybe: Parse[Option[A]] = new Parse.Optional[A](this)
  final def |[B](that: Parse[B]): Parse[Either[B, A]] = new Parse.SumType[A, B](this, that)
  final def desc(s: String): Parse[A] = new Parse.Desc[A](this, s)
}
object Parse {
  enum ArgStyle:
    case Never
    case Always
    case Optional

  def choosy(xs: Seq[String]): String = choosy(xs.toArray)
  def choosy(xs: Array[String]): String =
    xs.length match
      case 2 => xs.mkString("Choose ", " or ", "")
      case 1 => s"Use ${xs.head}"
      case x if x < 1 => s"Do not supply a value."
      case _ => "Choose ".make: sb =>
        xs.visit(): (x, i) =>
          if i == xs.length - 1 then sb += ", or "
          else if i > 0 then sb += ", "
          sb += x

  def listy(xs: Seq[String]): String = listy(xs.toArray)
  def listy(xs: Array[String]): String =
    MkStr: sb =>
      xs.visit(): (x, i) =>
        if i > 0 then
          if i+1 == xs.length then
            if i > 1 then sb += ", and "
            else sb += " and "
          else sb += ", "
        sb += x

  final class Optional[A](p: Parse[A]) extends Parse[Option[A]] {
    def apply(s: String) =
      if s.isEmpty then Is(None)
      else p(s).map(x => Some(x))
    override val argument = ArgStyle.Optional
    override val toString =
      val s = p.toString
      if s.startsWith("Parse[") && s.endsWith("]") then s"Parse[Option[${s.select(6 to End-1)}]]"
      else s + ".maybe"
    override lazy val userString =
      if p.userString.startsWith("[") && p.userString.endsWith("]") then p.userString
      else s"[${p.userString}]"
  }

  final class SumType[A, B](p: Parse[A], q: Parse[B]) extends Parse[Either[B, A]] {
    def apply(s: String): Ask[Either[B, A]] =
      p(s).fold{ a => Is(Right(a)) }{ e1 =>
        q(s).fold{ b => Is(Left(b)) }{ e2 =>
          Alt(Err(e1, e2)("Cannot parse either alternative"))
        }
      }
    override val argument = (p.argument, q.argument) match
      case (ArgStyle.Always, ArgStyle.Always) => ArgStyle.Always
      case (ArgStyle.Never, ArgStyle.Never) => ArgStyle.Never
      case _ => ArgStyle.Optional
    override val toString =
      val s = p.toString
      val t = q.toString
      if s.startsWith("Parse[") && s.endsWith("]") && t.startsWith("Parse[") && t.endsWith("]") then s"Parse[${s.select(6 to End-1)} | ${t.select(6 to End-1)}]"
      else s"$s | $t"
    override lazy val userString =
      val pStrip = p.userString.fn(s => if s.startsWith("[") && s.endsWith("]") then s.select(1 to End-1) else s)
      val qStrip = q.userString.fn(s => if s.startsWith("[") && s.endsWith("]") then s.select(1 to End-1) else s)
      argument match
        case ArgStyle.Always => s"$pStrip/$qStrip"
        case _               => s"[$pStrip/$qStrip]"
  }

  final class Desc[A](p: Parse[A], message: String) extends Parse[A] {
    def apply(s: String) = p(s)
    override def argument = p.argument
    override def toString = p.toString
    override def userString = message
  }

  final class The[L <: LabelStr](ls: Vector[L]) extends Parse[L] {
    def apply(s: String): Ask[L] =
      ls.find(_ == s) match
        case Some(l) => Is(l)
        case _ => Err.or(s"$s is not a valid option.  ${choosy(ls)}")
    transparent inline def |[M <: LabelStr](m: M) = summonFrom{
      case have: (M <:< L) => this
      case _               => new The[L | M](ls :+ m)
    }
    override val argument = if ls contains "" then ArgStyle.Optional else ArgStyle.Always
    override val toString = ls.mkString("|")
    override lazy val userString = argument match
      case ArgStyle.Optional  => s"[${ls.filter(_.nonEmpty).mkString("/")}]"
      case _                  => ls.mkString("/")
  }

  def keyMatch(key: String)(test: String): Int =
    if !test.startsWith("--") then -1
    else
      val i = test.indexOf('=')
      if i < 0 then
        if key.length == test.length - 2 && test.startsWith(key, 2) then test.length
        else -1
      else if key.length == i - 2 && test.startsWith(key, 2) then i+1
      else -1

  def whereKey(key: String)(args: Array[String]): Array[(Int, Int)] =
    var stop = false
    args.flex.copyOp: (test, i) =>
      shortcut.quit(stop).?
      stop = test.startsWith("--") && (test.length == 2 || (test.length==3 && test.charAt(2) == '='))
      val j = keyMatch(key)(test)
      shortcut.skip(j < 0).?
      (i, j)

  def shortMatch(key: Char)(test: String): Option[Array[Int]] =
    if test.isEmpty || !test.startsWith("-") || test.startsWith("--") then None
    else Some(test.where(_ == key))

  def whereShort(key: Char)(args: Array[String]): Array[(Int, Int)] =
    val iib = Array.newBuilder[(Int, Int)]
    shortcut.quittable:
      args.visit(): (test, i) =>
        shortMatch(key)(test) match
          case Some(js) => js.use()(j => iib += ((i, j)))
          case _ => shortcut.quit(test.startsWith("--") && (test.length == 2 || (test.length==3 && test.charAt(2) == '='))).?
    iib.result

  def whereEither(key: String, short: Char)(args: Array[String]): Array[(Int, Boolean, Int)] =
    val izib = Array.newBuilder[(Int, Boolean, Int)]
    shortcut.quittable:
      args.visit(): (test, i) =>
        val j = keyMatch(key)(test)
        if j >= 0 then izib += ((i, true, j))
        else shortMatch(short)(test) match
          case Some(js) => js.use()(j => izib += ((i, false, j)))
          case _ => shortcut.quit(test.startsWith("--") && (test.length == 2 || (test.length==3 && test.charAt(2) == '='))).?
    izib.result
}

val _u: Parse[Unit] = new:
  def apply(s: String) = if s.isEmpty then Is.unit else Err.or("Do not supply a value.")
  override def argument = Parse.ArgStyle.Never
  override def toString = "Parse[Unit]"
  override def userString = ""

val _tf: Parse[Boolean] = new:
  def apply(s: String) =
    if s.isEmpty then Is(true)
    else if Cleasy.goodCaps(s, "true",  "t", "T", "yes", "y", "Y", "on" ) then Is(true)
    else if Cleasy.goodCaps(s, "false", "f", "F", "no",  "n", "N", "off") then Is(false)
    else Err.or(s"$s is not a valid boolean specifier")
  override def argument = Parse.ArgStyle.Optional
  override def userString = "[T/F]"
  override def toString = "Parse[Boolean]"

val _str: Parse[String] = new:
  def apply(s: String) = s.orAlt[Err]
  override def userString = "text"
  override def toString = "Parse[String]"

val _int: Parse[Int] = new:
  def apply(s: String) = nice{ s.toInt }
  override def toString = "Parse[Int]"

val _uint: Parse[UInt] = new:
  def apply(s: String) = nice{ java.lang.Integer.parseUnsignedInt(s).u }
  override def toString = "Parse[UInt]"

val _long: Parse[Long] = new:
  def apply(s: String) = nice{ s.toLong }
  override def toString = "Parse[Long]"

val _ulong: Parse[ULong] = new:
  def apply(s: String) = nice{ java.lang.Long.parseUnsignedLong(s).u }
  override def toString = "Parse[ULong]"

val _double: Parse[Double] = new:
  def apply(s: String): Ask[Double] = Ask:
    if s.length > 0 then
      val c = s.charAt(0)
      if c == 'i' || c == 'I' then
        if s.equalsIgnoreCase("infinity") || s.equalsIgnoreCase("inf") then Is.break(Double.PositiveInfinity)
      else if c == 'n' || c == 'N' then
        if s.equalsIgnoreCase("NaN") then Is.break(Double.NaN)
      else if c == '-' && s.length > 1 then
        val c2 = s.charAt(1)
        if c2 == 'i' || c2 == 'I' then
          if s.drop(1).fn(t => t.equalsIgnoreCase("infinity") || t.equalsIgnoreCase("inf")) then Is.break(Double.NegativeInfinity)
        else if c2 == 'n' || c2 == 'N' then
          if s.drop(1).equalsIgnoreCase("NaN") then Is.break(-Double.NaN)
    s.toDouble
  override def toString = "Parse[Double]"

val _path: Parse[Path] = new:
  def apply(s: String) = nice{ s.path }
  override def userString = "path"
  override def toString = "Parse[Path]"

val _dir: Parse[Path] = new:
  def apply(s: String): Ask[Path] = Ask:
    val p = s.path
    if !p.exists then Err ?# s"Folder does not exist: $p"
    if !p.isDirectory then Err ?# s"Not a folder: $p"
    p
  override def userString = "folder"
  override def toString = "Parse[Path]"

val _file: Parse[Path] = new:
  def apply(s: String): Ask[Path] = Ask:
    val p = s.path
    if !p.exists then Err ?# s"File does not exist: $p"
    if p.isDirectory then Err ?# s"Folder instead of file: $p"
    p
  override def userString = "file"
  override def toString = "Parse[Path]"

def the[L <: LabelStr](label: L): Parse.The[L] = Parse.The[L](Vector(label))


extension [L <: LabelStr](label: L) {
  transparent inline def |[M <: LabelStr](m: M) = Parse.The[L](Vector(label)).|(m)

  inline def %(about: String) = Opt.of[Unit, '\u002D', L](label, '\u002D', _u) % about

  inline def x: OptNLabel[L] = OptNLabel(label)

  inline def ~[C <: CharVal](short: C): OptLabelChar[C, L] = OptLabelChar(label, short)

  inline def ~[A](parse: Parse[A]): Opt[A, '\u002D', L] = Opt.of(label, '\u002D', parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptD[A, '\u002D', L] = Opt.withDefault(label, '\u002D', pd._1, pd._2)
}

extension [C <: CharVal](short: C) {
  inline def %(about: String): Opt[Unit, C, Singleton & ToString[C]] = Opt.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u) % about

  inline def x: OptNChar[C] = OptNChar(short)

  inline def ~[L <: LabelStr](label: L): OptLabelChar[C, L] =
    compiletime.error(s"Place long-form label first")

  inline def ~[A](parse: Parse[A]): Opt[A, C, Singleton & ToString[C]] =  Opt.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptD[A, C, Singleton & ToString[C]] = Opt.withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, pd._1, pd._2)
}

final class OptNLabel[L <: LabelStr](val label: L) {
  inline def build: OptN[Unit, '\u002D', L] = OptN.of(label, '\u002D', _u)

  inline def %(about: String): OptN[Unit, '\u002D', L] = OptN.of(label, '\u002D', _u) % about

  inline def ~[C <: CharVal](short: C): OptNLabelChar[C, L] = OptNLabelChar(label, short)

  inline def ~[C <: CharVal](onc: OptNChar[C]): OptNLabelChar[C, L] =
    compiletime.error("Place .x on long-form label only when giving both long and short forms")

  inline def ~[A](parse: Parse[A]): OptN[A, '\u002D', L] = OptN.of(label, '\u002D', parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptN[A, '\u002D', L] = OptN.withDefault(label, '\u002D', pd._1, pd._2)
}

final class OptNChar[C <: CharVal](val short: C) {
  inline def build: OptN[Unit, C, Singleton & ToString[C]] = OptN.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)

  inline def %(about: String): OptN[Unit, C, Singleton & ToString[C]] = OptN.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u) % about

  inline def ~[A](parse: Parse[A]): OptN[A, C, Singleton & ToString[C]] = OptN.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptN[A, C, Singleton & ToString[C]] = OptN.withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, pd._1, pd._2)
}

final class OptLabelChar[C <: CharVal, L <: LabelStr](val label: L, val short: C) {
  inline def build: Opt[Unit, C, L] = Opt.of(label, short, _u)

  inline def %(about: String): Opt[Unit, C, L] = Opt.of(label, short, _u) % about

  inline def x: OptNLabelChar[C, L] = compiletime.error(s"Place `.x` on long-form option")

  inline def ~[A](parse: Parse[A]): Opt[A, C, L] = Opt.of(label, short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptD[A, C, L] = Opt.withDefault(label, short, pd._1, pd._2)
}

final class OptNLabelChar[C <: CharVal, L <: LabelStr](val label: L, val short: C) {
  def build: OptN[Unit, C, L] = OptN.of(label, short, _u)

  inline def %(about: String): OptN[Unit, C, L] = OptN.of(label, short, _u) % about

  inline def ~[A](parse: Parse[A]): OptN[A, C, L] = OptN.of(label, short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptN[A, C, L] = OptN.withDefault(label, short, pd._1, pd._2)
}


final case class Opt[A, C <: CharVal, L <: LabelStr] private[cleasy] (label: L, short: C, parse: Parse[A], about: String) {
  inline def parse_?[E >: Alt[Err]](args: Array[String], consumed: Int => Unit = _ => {})(using boundary.Label[E]): (Option[(A, Int)] \ L) =
    if short == '\u002D' then
      Parse.whereKey(label)(args) match
        case Array() => \.wrap[Option[(A, Int)]](None)[L]
        case Array(ij) =>
          val (i, j) = ij
          val arg = args(i)
          val v = arg.select(j to End)
          val a = parse(v).fold(__){ e => boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $arg")) }
          consumed(i)
          \.wrap[Option[(A, Int)]](Some(a, i))[L]
        case idxs => 
          val msg = s"--$label may only be given once but was found ${idxs.length} times\n  at arguments ${Parse.listy(idxs.map{ case (i, _) => s"#${i+1}" })}"
          boundary.break(Err.or(msg))
    else
      Parse.whereEither(label, short)(args) match
        case Array() => \.wrap[Option[(A, Int)]](None)[L]
        case Array(izj) =>
          val (i, isLabel, j) = izj
          val arg = args(i)
          val v =
            if isLabel then arg.select(j to End)
            else if parse.argument == Parse.ArgStyle.Always then
              if j != End.of(arg) then boundary.break(Err.or(s"-$short must be last in $arg because it needs an argument"))
              if i+1 >= args.length then boundary.break(Err.or(s"-$short needs an argument but there are no more arguments"))
              if args(i+1).startsWith("-") then boundary.break(Err.or(s"-$short needs an argument but ${args(i+1)} is also an option"))
              consumed(i+1)
              args(i+1)
            else ""
          val a = parse(v).fold(__){ e => 
            val argmsg =
              if isLabel then arg
              else s"$short in $arg (equivalent to --$label)"
            boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $argmsg"))
          }
          consumed(i)
          \.wrap[Option[(A, Int)]](Some(a, i))[L]
        case idxs =>
          boundary.break(Err.or(Opt.moreThanOneOptionError(label, short, args, idxs)))

  inline def comment(moreAbout: String) = this % moreAbout

  def %(moreAbout: String) =
    new Opt[A, C, L](label, short, parse, if about.isEmpty || about(End) == '\n' then about + moreAbout else about + "\n" + moreAbout)

  def x =
    new OptN[A, C, L](label, short, parse, None, about)

  def withDefault(default: () => A): OptD[A, C, L] =
    new OptD[A, C, L](label, short, parse, default, about)

  override lazy val toString = Opt.customToString(label, if short == '-' then "" else short.toString, if parse eq _u then "" else (if parse eq _u then "" else "?") + parse.toString, about)()
}
object Opt {
  val done = new Opt[Unit, '\u002D', ""]("", '\u002D', _u, "Stop parsing options.")

  inline def apply[L <: LabelStr](label: L): Opt[Unit, '\u002D', L] = of(label, '\u002D', _u)
  inline def apply[C <: CharVal](short: C): Opt[Unit, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)
  inline def apply[C <: CharVal, L <: LabelStr](label: L, short: C): Opt[Unit, C, L] = of(label, short, _u)
  inline def apply[A, L <: LabelStr](label: L, parse: Parse[A]): Opt[A, '\u002D', L] = of(label, '\u002D', parse)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A]): Opt[A, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  inline def apply[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A]): Opt[A, C, L] = of(label, short, parse)
  def of[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A]): Opt[A, C, L] = new Opt[A, C, L](label, short, parse, "")

  inline def apply[A, L <: LabelStr](label: L, parse: Parse[A], default: () => A): OptD[A, '\u002D', L] = withDefault(label, '\u002D', parse, default)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A], default: () => A): OptD[A, C, Singleton & ToString[C]] = withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse, default)
  inline def apply[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A], default: () => A): OptD[A, C, L] = withDefault(label, short, parse, default)
  def withDefault[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A], default: () => A): OptD[A, C, L] = new OptD[A, C, L](label, short, parse, default, "")
  
  private[cleasy] def moreThanOneOptionError(label: String, short: Char, args: Array[String], idxs: Array[(Int, Boolean, Int)]): String =
    val lbb = Array.newBuilder[Int]
    val shb = Array.newBuilder[Int]
    idxs.visit(): (ixj, _) =>
      if ixj._2 then lbb += ixj._1 else shb += ixj._1
    val labelIdx = lbb.result
    val shortIdx = shb.result
    MkStr: sb =>
      if shortIdx.length == 0 then sb += s"--$label may only be given once but was found ${labelIdx.length} times\n"
      else if labelIdx.length == 0 then sb += s"-$short (--$label) may only be given once but was found ${shortIdx.length} times\n"
      else sb += s"--$label (-$short) may only be given once but were found ${idxs.length} times\n"
      if labelIdx.length > 0 then
        val msgIfShort = if shortIdx.length > 0 then s"as --$label " else ""
        sb += s"  ${msgIfShort}at arguments ${Parse.listy(labelIdx.map(i => s"#${i+1}"))}"
      if shortIdx.length > 0 then
        val shortArgs = shortIdx.diced(shortIdx.flex.copyOp{ (ix, j) => shortcut.skip(j == 0 || shortIdx(j-1) == ix).?; j } , "[)")
        val argsMsg = shortArgs.copyWith: ixs =>
          if ixs.length == 1 then
            if args(ixs(0)).length == 2 then "#" + (ixs(0)+1)
            else s"#${ixs(0)+1} (in ${args(ixs(0))})"
          else s"#${ixs(0)+1} (${ixs.length}x in ${args(ixs(0))})"
        val msgIfLong = if labelIdx.length > 0 then s"as -$short " else ""
        val plural = if shortArgs.length == 1 then "" else "s"
        sb += s"  ${msgIfLong}at argument$plural ${Parse.listy(argsMsg)}"

  private[cleasy] def customToString[A](label: String, short: String, parse: String, about: String)(indent: Int = 2, labelW: Int = -1, shortW: Int = -1, parseW: Int = -1, aboutW: Int = -1, margin: Int = 80, clip: Boolean = false, wrap: Int = 0, sep: String = " ") =
    MkStr: sb =>
      val start = sb.length
      sb.repeat(' ', indent)
      var overage = 0
      var useOverage = labelW >= 0
      inline def pad(n: Int)(f: => Unit): Unit =
        val i0 = sb.length
        f
        if clip && n >= 0 && (sb.length - i0) > (n max 1) then
          sb.length = i0 + n - 1
          sb += '\u2026'
        val iN = sb.length
        if useOverage then
          if n > iN - i0 then
            (n - (iN - i0)).times:
              if overage > 0 then overage -= 1
              else sb += ' '
          else if n > 0 then overage += (iN - i0) - n
          else useOverage = n == 0
      pad(labelW):
        sb += "--"
        sb += label
      pad(shortW + sep.length - 1):
        if short.nonEmpty then
          sb += sep
          sb += '-'
          sb += short
      pad(parseW + sep.length - 1):
        if parse.nonEmpty then
          sb += sep
          sb += parse
      if about.length == 1 || margin <= 0 || margin - sb.length > sep.length + about.length then
        sb += sep
        sb += about
      else if about.length > 0 then
        sb += sep
        val in = sb.length - start
        if margin - in < 3 then sb += '\u2026'
        else
          var w = wrap
          val idxs = about.where(_.isWhitespace)
          var i = 0
          var ii = 0
          while i < about.length && w > 0 && about.length - i >= (margin - in) do
            while ii+1 < idxs.length && idxs(ii+1) - i < (margin - in) do ii += 1
            if ii < idxs.length && idxs(ii) > i then
              sb.add(about, i, idxs(ii))
              i = idxs(ii) + 1
              while ii+1 < idxs.length && i == idxs(ii+1) do
                i += 1
                ii += 1
              if i < about.length then
              sb += '\n'
              sb.repeat(' ', in)
            else
              sb.add(about, i, i + (margin - in - 2))
              sb += '\u21b5'
              sb += '\n'
              sb.repeat(' ', in)
              i += (margin - in - 2)
            w -= 1
          if about.length - i < margin - in then sb.add(about, i, about.length)
          else
            sb.add(about, i, i + (margin - in - 2))
            sb += '\u2026'
      if sb(End) != '\n' then sb += '\n'
}

final case class OptN[A, C <: CharVal, L <: LabelStr] private[cleasy] (val label: L, short: C, parse: Parse[A], default: Option[() => A], about: String) {
  inline def parse_?[E >: Alt[Err]](args: Array[String], consumed: Int => Unit = _ => {})(using boundary.Label[E]): (List[(A, Int)] \ L) =
    val lb = List.newBuilder[(A, Int)]
    if short == '\u002D' then
      val idxs = Parse.whereKey(label)(args)
      if idxs.isEmpty then
        default match
          case Some(a) => (lb += ((a(), -1))) __ Unit
          case _ =>
      else
        idxs.use(): ij =>
          val (i, j) = ij
          val arg = args(i)
          val v = arg.select(j to End)
          val a = parse(v).fold(__){ e => boundary.break(Alt(e +# s"Error parsing option at argument ${i+1}, $arg")) }
          consumed(i)
          (lb += ((a, i))) __ Unit
    else
      val idxs = Parse.whereEither(label, short)(args)
      idxs.visit(): (izj, _) =>
        val (i, isLabel, j) = izj
        val arg = args(i)
          val v =
            if isLabel then arg.select(j to End)
            else if parse.argument == Parse.ArgStyle.Always then
              if j != End.of(arg) then boundary.break(Err.or(s"-$short must be last in $arg because it needs an argument"))
              if i+1 >= args.length then boundary.break(Err.or(s"-$short needs an argument but there are no more arguments"))
              if args(i+1).startsWith("-") then boundary.break(Err.or(s"-$short needs an argument but ${args(i+1)} is also an option"))
              consumed(i+1)
              args(i+1)
            else ""
        val a = parse(v).fold(__){ e =>
          val argmsg =
            if isLabel then arg
            else s"$short in $arg (equivalent to --$label)"
          boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $argmsg"))
        }
        consumed(i)
        (lb += ((a, i))) __ Unit
    \.wrap[List[(A, Int)]](lb.result)[L]

  inline def getDefault: Option[A] = default.map(_())

  inline def comment(moreAbout: String) = this % moreAbout

  def %(moreAbout: String) =
    new OptN[A, C, L](label, short, parse, default, if about.isEmpty || about(End) == '\n' then about + moreAbout else about + "\n" + moreAbout)

  override lazy val toString = Opt.customToString(label, if short == '-' then "" else short.toString, if parse eq _u then "" else "*" + parse.toString, about)()
}
object OptN {
  inline def apply[L <: LabelStr](label: L): OptN[Unit, '\u002D', L] = of(label, '\u002D', _u)
  inline def apply[C <: CharVal](short: C): OptN[Unit, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)
  inline def apply[C <: CharVal, L <: LabelStr](label: L, short: C): OptN[Unit, C, L] = of(label, short, _u)
  inline def apply[A, L <: LabelStr](label: L, parse: Parse[A]): OptN[A, '\u002D', L] = of(label, '\u002D', parse)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A]): OptN[A, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  inline def apply[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A]): OptN[A, C, L] = of(label, short, parse)
  def of[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A]): OptN[A, C, L] = new OptN(label, short, parse, None, "")

  inline def apply[A, L <: LabelStr](label: L, parse: Parse[A], default: () => A): OptN[A, '\u002D', L] = withDefault(label, '\u002D', parse, default)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A], default: () => A): OptN[A, C, Singleton & ToString[C]] = withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse, default)
  inline def apply[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A], default: () => A): OptN[A, C, L] = withDefault(label, short, parse, default)
  def withDefault[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A], default: () => A): OptN[A, C, L] = new OptN(label, short, parse, Some(default), "")
}

final case class OptD[A, C <: CharVal, L <: LabelStr] private[cleasy] (val label: L, short: C, parse: Parse[A], default: () => A, about: String) {
  inline def parse_?[E >: Alt[Err]](args: Array[String], consumed: Int => Unit = _ => {})(using boundary.Label[E]): ((A, Option[Int]) \ L) =
    if short == '\u002D' then
      Parse.whereKey(label)(args) match
        case Array() => \.wrap[(A, Option[Int])]((default(), None))[L]
        case Array(ij) =>
          val (i, j) = ij
          val arg = args(i)
          val v = arg.select(j to End)
          val a = parse(v).fold(__){ e => boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $arg")) }
          consumed(i)
          \.wrap[(A, Option[Int])]((a, Some(i)))[L]
        case idxs => 
          val msg = s"--$label may only be given once but was found ${idxs.length} times\n  at arguments ${Parse.listy(idxs.map{ case (i, _) => s"#${i+1}" })}"
          boundary.break(Err.or(msg))
    else
      Parse.whereEither(label, short)(args) match
        case Array() => \.wrap[(A, Option[Int])]((default(), None))[L]
        case Array(izj) =>
          val (i, isLabel, j) = izj
          val arg = args(i)
          val v =
            if isLabel then arg.select(j to End)
            else if parse.argument == Parse.ArgStyle.Always then
              if j != End.of(arg) then boundary.break(Err.or(s"-$short must be last in $arg because it needs an argument"))
              if i+1 >= args.length then boundary.break(Err.or(s"-$short needs an argument but there are no more arguments"))
              if args(i+1).startsWith("-") then boundary.break(Err.or(s"-$short needs an argument but ${args(i+1)} is also an option"))
              consumed(i+1)
              args(i+1)
            else ""
          val a = parse(v).fold(__){ e => 
            val argmsg =
              if isLabel then arg
              else s"$short in $arg (equivalent to --$label)"
            boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $argmsg"))
          }
          consumed(i)
          \.wrap[(A, Option[Int])]((a, Some(i)))[L]
        case idxs =>
          boundary.break(Err.or(Opt.moreThanOneOptionError(label, short, args, idxs)))

  inline def comment(moreAbout: String) = this % moreAbout

  def %(moreAbout: String) =
    new OptD[A, C, L](label, short, parse, default, if about.isEmpty || about(End) == '\n' then about + moreAbout else about + "\n" + moreAbout)

  def x =
    new OptN[A, C, L](label, short, parse, Some(default), about)

  override lazy val toString = Opt.customToString(label, if short == '-' then "" else short.toString, if parse eq _u then "" else "!" + parse.toString, about)()
}
object OptD {
  inline def apply[A, L <: LabelStr](label: L, parse: Parse[A], default: () => A): OptD[A, '\u002D', L] = of(label, '\u002D', parse, default)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A], default: () => A): OptD[A, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse, default)
  inline def apply[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A], default: () => A): OptD[A, C, L] = of(label, short, parse, default)
  def of[A, C <: CharVal, L <: LabelStr](label: L, short: C, parse: Parse[A], default: () => A): OptD[A, C, L] = new OptD(label, short, parse, default, "")
}


final class Args[N <: LabelStr, T <: Tuple](val original: Array[String], used: Array[Int], labels: Vector[N], parsed: T) {
  val indexedArgs =
    var stopped = false
    original.flex.copyOp: (arg, i) =>
      shortcut.skip(used(i) > 0).?
      (arg, i)

  val args = indexedArgs.copyWith(_._1)

  lazy val options: NamedTuple.NamedTuple[Args.TupleNames[T], Args.TupleTypes[T]] = Args.asNamedTuple(parsed)
  lazy val labeled = Args.asLabeledTuple(parsed)

  transparent inline def indexed[L <: LabelStr](label: L): Args.Extract[L, T] = summonFrom {
    case have: (L <:< N) => Args.extract[L, T](label, parsed)
    case _ => compiletime.error("Option by that name does not exist.")
  }
  transparent inline def apply[L <: LabelStr](label: L) = summonFrom {
    case have: (L <:< N) => inline indexed[L](label) match
      case ao: (a, Option[Int]) => ao._1
      case oa: Option[(a, Int)] => oa.map(_._1)
      case os: List[(a, Int)] => os.map(_._1)
      case _ => compiletime.error("Cannot simplify this type.")
    case _ => compiletime.error("Option by that name does not exist.")
  }
  inline def found[L <: LabelStr](label: L) = summonFrom {
    case have: (L <:< N) => inline indexed[L](label) match
      case (_, oi: Option[Int]) => oi.isDefined
      case oa: Option[(?, Int)] => oa.isDefined
      case os: List[(?, Int)] => os match
        case Nil => false
        case (_, i) :: Nil if i < 0 => false
        case _ => true
      case _ => compiletime.error("Unexpected type among parsed options")
  }
  inline def require[L <: LabelStr](label: L): Unit Or Err = summonFrom {
    case have: (L <:< N) => inline indexed[L](label) match
      case (_, oi: Option[Int]) => if oi.isDefined then Is(()) else Err.or(s"Option --${label} is required")
      case oa: Option[(?, Int)] => if oa.isDefined then Is(()) else Err.or(s"Option --${label} is required")
      case os: List[(?, Int)] => os match
        case Nil => Err.or(s"Option --${label} is required")
        case (_, i) :: Nil if i < 0 => Err.or(s"Option --${label} is required")
        case _ => Is(())
      case _ => compiletime.error("Unexpected type among parsed options")
  }
}
object Args {
  final class Elt[L <: LabelStr, A](val value: A, val isDoneMarker: Boolean = false) {}
  inline def elt[L <: LabelStr, A](x: \[A, L]): Elt[L, A] = new Elt[L, A](x.unlabel)
  inline def elt[L <: LabelStr, A](x: \[A, L], m: Boolean): Elt[L, A] = new Elt[L, A](x.unlabel, isDoneMarker = m)

  type Extract[L <: LabelStr, T <: Tuple] = T match
    case EmptyTuple => Nothing
    case Elt[L, a] *: ts => a
    case t *: ts => Extract[L, ts]

  type AsLabeledTuple[T <: Tuple] = T match
    case EmptyTuple => EmptyTuple
    case Elt[l, a] *: ts => \[a, l] *: AsLabeledTuple[ts]

  type TupleTypes[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Elt[l, a] *: ts => l match
      case "" => TupleTypes[ts]
      case _ => a match
        case (b, _) => b *: TupleTypes[ts]
        case Option[(b, _)] => Option[b] *: TupleTypes[ts]
        case List[(b, _)] => List[b] *: TupleTypes[ts]

  type TupleNames[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Elt[l, _] *: ts => l match
      case "" => TupleNames[ts]
      case _ => l *: TupleNames[ts]

  transparent inline def extractUncertainTypeByLabel[L <: LabelStr, T <: Tuple](label: L, tuple: T): Any =
    inline compiletime.erasedValue[T] match
      case _: EmptyTuple => compiletime.error("Label not found")
      case _: (Elt[L, a] *: tp) => tuple.asInstanceOf[Elt[L, a] *: tp].head.value
      case _: (t *: tp) => extractUncertainTypeByLabel[L, tp](label, tuple.drop(1).asInstanceOf[tp])

  inline def extract[L <: LabelStr, T <: Tuple](label: L, tuple: T): Extract[L, T] = 
    extractUncertainTypeByLabel[L, T](label, tuple).asInstanceOf[Extract[L, T]]

  inline def asNamedTuple[T <: Tuple](tuple: T): NamedTuple.NamedTuple[TupleNames[T], TupleTypes[T]] =
    var work: Tuple = tuple
    var result: Tuple = EmptyTuple
    loop:
      work match
        case t *: rest => t match
          case e: Elt[?, ?] =>
            if !e.isDoneMarker then
              val value = e.value match
                case ao: (?, ?) => ao._1
                case oa: Option[?] => oa match
                  case Some(soa) => soa match
                    case ai: (?, ?) => Some(ai._1)
                    case _ => Err("Internal error: unknown option data packed in Option").toss
                  case _ => None
                case os: List[?] => os.map(_ match {
                  case ai: (?, ?) => ai._1
                  case _ =>  Err("Internal error: unknown option data packed in List").toss
                })
                case _ => Err("Internal error: trying to read options but unknown type").toss
              result = result :* value
            work = rest
          case _ => Err("Internal error: trying to read options but did not put them in Elt").toss
        case _ => loop.break()
    result.asInstanceOf[NamedTuple.NamedTuple[TupleNames[T], TupleTypes[T]]]

  inline def asLabeledTuple[T <: Tuple](tuple: T): AsLabeledTuple[T] =
    var work: Tuple = tuple
    var result: Tuple = EmptyTuple
    loop:
      work match
        case t *: rest => t match
          case e: Elt[?, ?] =>
            result = result :* e.value
            work = rest
          case _ => Err("Internal error: trying to read options but did not put them in Elt").toss
        case _ => loop.break()
    result.asInstanceOf[AsLabeledTuple[T]]
}


final class Cleasy[N <: LabelStr, H <: CharVal, T <: Tuple](title: String, postfix: String, shorts: Vector[H], labels: Vector[N], options: T) {
  transparent inline def +[A, C <: CharVal, L <: LabelStr](op: Opt[A, C, L]) = summonFrom {
    case have: (L <:< N) => compiletime.error("Option with that name already exists.")
    case okay: (C <:< '\u002D') => new Cleasy(title, postfix, shorts, (labels :+ op.label): Vector[N | L], options :* op)
    case have: (C <:< H) => compiletime.error("Option with that short name already exists.")
    case _ => new Cleasy(title, postfix, (shorts :+ op.short): Vector[H | C], (labels :+ op.label): Vector[N | L], options :* op)
  }
  transparent inline def +[A, C <: CharVal, L <: LabelStr](op: OptN[A, C, L]) = summonFrom {
    case have: (L <:< N) => compiletime.error("Option with that name already exists.")
    case okay: (C <:< '\u002D') => new Cleasy(title, postfix, shorts, (labels :+ op.label): Vector[N | L], options :* op)
    case have: (C <:< H) => compiletime.error("Option with that short name already exists.")
    case _ => new Cleasy(title, postfix, (shorts :+ op.short): Vector[H | C], (labels :+ op.label): Vector[N | L], options :* op)
  }
  transparent inline def +[A, C <: CharVal, L <: LabelStr](op: OptD[A, C, L]) = summonFrom {
    case have: (L <:< N) => compiletime.error("Option with that name already exists.")
    case okay: (C <:< '\u002D') => new Cleasy(title, postfix, shorts, (labels :+ op.label): Vector[N | L], options :* op)
    case have: (C <:< H) => compiletime.error("Option with that short name already exists.")
    case _ => new Cleasy(title, postfix, (shorts :+ op.short): Vector[H | C], (labels :+ op.label): Vector[N | L], options :* op)
  }

  transparent inline def --[L <: LabelStr](label: L) = this + Opt.of[Unit, '\u002D', L](label, '\u002D', _u)
  transparent inline def --[C <: CharVal](short: C) = this + Opt.of[Unit, C, Singleton & ToString[C]](constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)
  
  transparent inline def --[L <: LabelStr](onl: OptNLabel[L]) = this + onl.build
  transparent inline def --[C <: CharVal](onc: OptNChar[C]) = this + onc.build
  transparent inline def --[C <: CharVal, L <: LabelStr](olc: OptLabelChar[C, L]) = this + olc.build
  transparent inline def --[C <: CharVal, L <: LabelStr](onlc: OptNLabelChar[C, L]) = this + onlc.build

  transparent inline def --[A, C <: CharVal, L <: LabelStr](o: Opt[A, C, L]) = this + o
  transparent inline def --[A, C <: CharVal, L <: LabelStr](o: OptN[A, C, L]) = this + o
  transparent inline def --[A, C <: CharVal, L <: LabelStr](o: OptD[A, C, L]) = this + o

  transparent inline def parse[E >: Alt[Err]](args: Array[String]) = Ask:
    escape:
      args.visit(): (arg, i) =>
        escape.when(arg == "--" || arg == "--=").?
        if arg.startsWith("--") then
          val i = arg.indexOf('=')
          val argname = if i < 0 then arg.select(2 to End) else arg.select(2, i)
          if !labels.contains(argname) then
            Err ?# s"No such option: argname\n  in argument ${i+1}, $arg"
        else if arg.startsWith("-") then
          val unknown = arg.drop(1).filterNot(shorts contains _)
          if unknown.nonEmpty then
            Err ?# s"No such option${if unknown.length > 1 then "s" else ""}: ${Parse.listy(unknown.map(_.toString))}\n  in argument ${i+1}, $arg"
    val used = args.copyWith(_ => 0)
    Args(args, used, labels, Cleasy.parse_?(options, args, i => used(i) += 1))

  def userString(margin: Int = 80, debug: Boolean = false, paramMax: Int = 16) =
    title.make: sb =>
      if sb.length > 0 && sb(End) != '\n' then sb += '\n'
      var ops: Tuple = options
      var labelW = 0
      var shortW = 0
      var parseW = 0
      var aboutW = 0
      loop:
        ops match
          case EmptyTuple => loop.break()
          case t *: tp =>
            ops = tp
            t match
              case o: Opt[?, ?, ?] =>
                if debug then
                  labelW = labelW max o.label.length + 2
                  if o.short != '-' then shortW = shortW max 4
                  if o.parse ne _u then parseW = parseW max o.parse.toString.length + 2
                else
                  val n = o.parse.userString.length.fn(x => x + x.sign) min paramMax
                  labelW = labelW max o.label.length + 2 + n
                  if o.short != '-' then shortW = shortW max 3 + (if o.parse.argument == Parse.ArgStyle.Always then n else 0)
                aboutW = aboutW max o.about.length
              case o: OptN[?, ?, ?] =>
                if debug then
                  labelW = labelW max o.label.length + 2
                  if o.short != '-' then shortW = shortW max 3
                  if o.parse ne _u then parseW = parseW max o.parse.toString.length + 2
                else
                  val n = o.parse.userString.length.fn(x => x + x.sign) min paramMax
                  labelW = labelW max o.label.length + 2 + n
                  if o.short != '-' then shortW = shortW max 3 + (if o.parse.argument == Parse.ArgStyle.Always then n else 0)
                aboutW = aboutW max o.about.length
              case o: OptD[?, ?, ?] =>
                if debug then
                  labelW = labelW max o.label.length + 2
                  if o.short != '-' then shortW = shortW max 3
                  if o.parse ne _u then parseW = parseW max o.parse.toString.length + 1
                else
                  val n = o.parse.userString.length.fn(x => x + x.sign) min paramMax
                  labelW = labelW max o.label.length + 2 + n
                  if o.short != '-' then shortW = shortW max 3 + (if o.parse.argument == Parse.ArgStyle.Always then n else 0)
                aboutW = aboutW max o.about.length
      if !debug then
        if sb.length > 1 && sb(End-1) != '\n' then sb += '\n'
        if labelW < 4 then labelW = 4
        if aboutW < 4 then aboutW = 4
        val lower = MkStr: s2 =>
          if labelW < 6 then
            sb += "Name"
            s2 += "----"
            if labelW == 5 then
              sb += ' '
              s2 += '-'
          else
            sb += "Option"
            s2 += "------"
            (labelW - 6).times:
              sb += ' '
              s2 += '-'
          if shortW > 0 then
            if shortW < 6 then
              sb += "  ch"
              s2 += "  --"
              (6 - shortW).times:
                sb += ' '
                s2 += '-'
            else
              sb += "  Short"
              s2 += "  -----"
              (shortW - 6).times:
                sb += ' '
                s2 += '-'
          if aboutW > 0 then
            if aboutW < 5 then aboutW = 5
            if aboutW < 15 then
              sb += "  Desc"
              s2 += "  ----"
              aboutW -= 5
            else
              sb += "  Description"
              s2 += "  -----------"
              aboutW -= 12
            if aboutW > margin - 1 - s2.length then aboutW = margin - 1 - s2.length
            aboutW.times:
              sb += ' '
              s2 += '-'
          sb += '\n'
          s2 += '\n'
        sb += lower
      ops = options
      loop:
        ops match
          case EmptyTuple => loop.break()
          case t *: tp =>
            ops = tp
            if t.asInstanceOf[AnyRef] ne Opt.done then
              t match
                case o: Opt[?, ?, ?]  =>
                  val usr = o.parse.userString.fixIf(_.length >= paramMax - 1)(_.take(paramMax - 2) + "\u2026")
                  val lb = if debug || o.parse.userString.isEmpty then o.label else if usr.startsWith("[") then s"${o.label}[=${usr.select(1 to End)}" else s"${o.label}=$usr"
                  val sh = if o.short == '-' then "" else if debug then o.short.toString else if o.parse.userString.isEmpty || o.parse.argument != Parse.ArgStyle.Always then o.short.toString else s"${o.short} $usr"
                  val ps = if debug then "?" + o.parse.toString else ""
                  val in = if debug then 2 else 0
                  val wr = if debug then 0 else 5
                  sb += Opt.customToString(lb, sh, ps, o.about)(indent = in, labelW = labelW, shortW = shortW, parseW = parseW, wrap = wr, sep = "  ")
                case o: OptN[?, ?, ?] =>
                  val usr = o.parse.userString.fixIf(_.length >= paramMax - 1)(_.take(paramMax - 2) + "\u2026")
                  val lb = if debug || o.parse.userString.isEmpty then o.label else if usr.startsWith("[") then s"${o.label}[=${usr.select(1 to End)}" else s"${o.label}=$usr"
                  val sh = if o.short == '-' then "" else if debug then o.short.toString else if o.parse.userString.isEmpty || o.parse.argument != Parse.ArgStyle.Always then o.short.toString else s"${o.short} $usr"
                  val ps = if debug then "*" + o.parse.toString else ""
                  val in = if debug then 2 else 0
                  val wr = if debug then 0 else 5
                  sb += Opt.customToString(lb, sh, ps, o.about)(indent = in, labelW = labelW, shortW = shortW, parseW = parseW, wrap = wr, sep = "  ")
                case o: OptD[?, ?, ?] =>
                  val usr = o.parse.userString.fixIf(_.length >= paramMax - 1)(_.take(paramMax - 2) + "\u2026")
                  val lb = if debug || o.parse.userString.isEmpty then o.label else if usr.startsWith("[") then s"${o.label}[=${usr.select(1 to End)}" else s"${o.label}=$usr"
                  val sh = if o.short == '-' then "" else if debug then o.short.toString else if o.parse.userString.isEmpty || o.parse.argument != Parse.ArgStyle.Always then o.short.toString else s"${o.short} $usr"
                  val ps = if debug then o.parse.toString else ""
                  val in = if debug then 2 else 0
                  val wr = if debug then 0 else 5
                  sb += Opt.customToString(lb, sh, ps, o.about)(indent = in, labelW = labelW, shortW = shortW, parseW = parseW, wrap = wr, sep = "  ")
                case _ => sb += t.toString
            if sb.length > 0 && sb(End) != '\n' then sb += '\n'
      sb += postfix
      if sb(End) != '\n' then sb += '\n'

  override lazy val toString = userString(debug = true)
}
object Cleasy {
  type Parsed[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Opt[a, ?, l] *: ts => Args.Elt[l, Option[(a, Int)]] *: Parsed[ts]
    case OptN[a, ?, l] *: ts => Args.Elt[l, List[(a, Int)]] *: Parsed[ts]
    case OptD[a, ?, l] *: ts => Args.Elt[l, (a, Option[Int])] *: Parsed[ts]
    case t *: ts => t *: Parsed[ts]

  def parse_?[T <: Tuple, E >: Alt[Err]](specification: T, arguments: Array[String], consumed: Int => Unit = _ => {})(using boundary.Label[E]): Parsed[T] =
    var work: Tuple = specification
    var result: Tuple = EmptyTuple
    loop:
      work match
        case t *: rest => t match
          case o: Opt[?, ?, ?] =>
            result = result :* Args.elt(o.parse_?(arguments, consumed), o eq Opt.done)
            work = rest
          case o: OptN[?, ?, ?] =>
            result = result :* Args.elt(o.parse_?(arguments, consumed))
            work = rest
          case o: OptD[?, ?, ?] =>
            result = result :* Args.elt(o.parse_?(arguments, consumed))
            work = rest
          case _ => boundary.break(Err.or("Internal error: trying to parse options but not using Opt"))
        case _ => loop.break()
    result.asInstanceOf[Parsed[T]]

  def goodCaps(s: String, ts: String*) =
    ts.exists(t => s.equalsIgnoreCase(t)) &&
    (s.forall(_.isLower) || s.forall(_.isUpper) || (s.length > 1 && s.head.isUpper && s.tail.forall(_.isLower)))

  def apply(title: String = "", conclusion: String => String = s => s) =
    val postfix = conclusion("Use -- to end option parsing. Short options may be combined as -abc.\n")
    new Cleasy(title, postfix, Vector('\u002D'), Vector(""), EmptyTuple :* Opt.done)
}
