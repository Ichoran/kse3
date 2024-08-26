// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.eio.cleasy

import scala.compiletime.{constValue, summonFrom}
import scala.compiletime.ops.any.ToString

import scala.util.boundary

import kse.basics.*
import kse.flow.*
import kse.maths.*

type CharVal = Char & Singleton

trait Parse[A] {
  def apply(s: String): A Or Err
  def userString: Option[String] = None
  def takesArgument: Boolean = false
  def maybe: Parse[Option[A]] = new Parse.Optional[A](this)
  def |[B](that: Parse[B]): Parse[Either[B, A]] = new Parse.SumType[A, B](this, that)
  def user(s: String): Parse[A] = new Parse.User[A](this, s)
}
object Parse {
  def choosy(xs: Seq[String]): String = choosy(xs.toArray)
  def choosy(xs: Array[String]): String =
    xs.length match
      case 2 => xs.mkString("Choose ", " or ", "")
      case 1 => s"Use ${xs.head}"
      case x if x < 1 => s"Do not supply a value."
      case _ => "Choose ".build: sb =>
        xs.visit(): (x, i) =>
          if i == xs.length - 1 then sb append ", or "
          else if i > 0 then sb append ", "
          sb append x

  def listy(xs: Seq[String]): String = listy(xs.toArray)
  def listy(xs: Array[String]): String =
    "".build: sb =>
      xs.visit(): (x, i) =>
        if i > 0 then
          if i+1 == xs.length then
            if i > 1 then sb append ", and "
            else sb append " and "
          else sb append ", "
        sb append x

  final class Optional[A](p: Parse[A]) extends Parse[Option[A]] {
    def apply(s: String) =
      if s.isEmpty then Is(None)
      else p(s).map(x => Some(x))
    override def takesArgument = false
    override def toString =
      val s = p.toString
      if s.startsWith("Parse[") && s.endsWith("]") then s"Parse[Option[${s.select(6 to End-1)}]]"
      else s + ".maybe"
    override def userString = p.userString.map(s => s"[$s]")
  }

  final class SumType[A, B](p: Parse[A], q: Parse[B]) extends Parse[Either[B, A]] {
    def apply(s: String): Either[B, A] Or Err =
      p(s).fold{ a => Is(Right(a)) }{ e1 =>
        q(s).fold{ b => Is(Left(b)) }{ e2 =>
          Alt(Err(e1, e2)("Cannot parse either alternative"))
        }
      }
    override def takesArgument = p.takesArgument && q.takesArgument
    override def toString =
      val s = p.toString
      val t = q.toString
      if s.startsWith("Parse[") && s.endsWith("]") && t.startsWith("Parse[") && t.endsWith("]") then s"Parse[${s.select(6 to End-1)} | ${t.select(6 to End-1)}]"
      else s"$s | $t"
  }

  final class User[A](p: Parse[A], message: String) extends Parse[A] {
    def apply(s: String) = p(s)
    override def takesArgument = true
    override def userString = Some(message)
  }

  final class The[L <: LabelVal](ls: Vector[L]) extends Parse[L] {
    def apply(s: String): L Or Err =
      ls.find(_ == s) match
        case Some(l) => Is(l)
        case _ => Err.or(s"$s is not a valid option.  ${choosy(ls)}")
    transparent inline def |[M <: LabelVal](m: M) = summonFrom{
      case have: (M <:< L) => this
      case _               => new The[L | M](ls :+ m)
    }
    override def takesArgument = !ls.contains("")
    override val userString = Some(ls.mkString(" | "))
    override def toString = ls.mkString("|")
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
    args.breakable.copyOp: (test, i) =>
      shortcut.quitIf(stop)
      stop = test.startsWith("--") && (test.length == 2 || (test.length==3 && test.charAt(2) == '='))
      val j = keyMatch(key)(test)
      shortcut.skipIf(j < 0)
      (i, j)

  def shortMatch(key: Char)(test: String): Option[Array[Int]] =
    if test.isEmpty || !test.startsWith("-") || test.startsWith("--") then None
    else Some(test.where(_ == key))

  def whereShort(key: Char)(args: Array[String]): Array[(Int, Int)] =
    val iib = Array.newBuilder[(Int, Int)]
    shortcut.quittable:
      args.visit(): (test, i) =>
        shortMatch(key)(test) match
          case Some(js) => js.peek()(j => iib += ((i, j)))
          case _ => shortcut.quitIf(test.startsWith("--") && (test.length == 2 || (test.length==3 && test.charAt(2) == '=')))
    iib.result

  def whereEither(key: String, short: Char)(args: Array[String]): Array[(Int, Boolean, Int)] =
    val izib = Array.newBuilder[(Int, Boolean, Int)]
    shortcut.quittable:
      args.visit(): (test, i) =>
        val j = keyMatch(key)(test)
        if j >= 0 then izib += ((i, true, j))
        else shortMatch(short)(test) match
          case Some(js) => js.peek()(j => izib += ((i, false, j)))
          case _ => shortcut.quitIf(test.startsWith("--") && (test.length == 2 || (test.length==3 && test.charAt(2) == '=')))
    izib.result
}

val _u: Parse[Unit] = new:
  def apply(s: String) = if s.isEmpty then Is.unit else Err.or("Do not supply a value.")
  override def toString = "Parse[Unit]"

val _tf: Parse[Boolean] = new:
  def apply(s: String) =
    if s.isEmpty then Is(true)
    else if Cleasy.goodCaps(s, "true",  "t", "yes", "y", "on" ) then Is(true)
    else if Cleasy.goodCaps(s, "false", "f", "no",  "n", "off") then Is(false)
    else Err.or(s"$s is not a valid boolean specifier")
  override def toString = "Parse[Boolean]"

val _str: Parse[String] = new:
  def apply(s: String) = s.orAlt[Err]
  override def toString = "Parse[String]"

val _int: Parse[Int] = new:
  def apply(s: String) = nice{ s.toInt }
  override def takesArgument = true
  override def toString = "Parse[Int]"

val _uint: Parse[UInt] = new:
  def apply(s: String) = nice{ java.lang.Integer.parseUnsignedInt(s).u }
  override def toString = "Parse[UInt]"

val _long: Parse[Long] = new:
  def apply(s: String) = nice{ s.toLong }
  override def takesArgument = true
  override def toString = "Parse[Long]"

val _ulong: Parse[ULong] = new:
  def apply(s: String) = nice{ java.lang.Long.parseUnsignedLong(s).u }
  override def takesArgument = true
  override def toString = "Parse[ULong]"

val _double: Parse[Double] = new:
  def apply(s: String) = nice{ s.toDouble }
  override def takesArgument = true
  override def toString = "Parse[Double]"

def the[L <: LabelVal](label: L): Parse.The[L] = Parse.The[L](Vector(label))


extension [L <: LabelVal](label: L) {
  transparent inline def |[M <: LabelVal](m: M) = Parse.The[L](Vector(label)).|(m)

  inline def %(about: String) = Opt.of[Unit, '\u002D', L](label, '\u002D', _u) % about

  inline def x: OptNLabel[L] = OptNLabel(label)

  inline def ~[C <: CharVal](short: C): OptLabelChar[C, L] = OptLabelChar(label, short)

  inline def ~[A](parse: Parse[A]): Opt[A, '\u002D', L] = Opt.of(label, '\u002D', parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptD[A, '\u002D', L] = Opt.withDefault(label, '\u002D', pd._1, pd._2)
}

extension [C <: CharVal](short: C) {
  inline def %(about: String): Opt[Unit, C, Singleton & ToString[C]] = Opt.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u) % about

  inline def x: OptNChar[C] = OptNChar(short)

  inline def ~[L <: LabelVal](label: L): OptLabelChar[C, L] =
    compiletime.error(s"Place long-form label first")

  inline def ~[A](parse: Parse[A]): Opt[A, C, Singleton & ToString[C]] =  Opt.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptD[A, C, Singleton & ToString[C]] = Opt.withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, pd._1, pd._2)
}

final class OptNLabel[L <: LabelVal](val label: L) {
  inline def build: OptN[Unit, '\u002D', L] = OptN.of(label, '\u002D', _u)

  inline def %(about: String): OptN[Unit, '\u002D', L] = OptN.of(label, '\u002D', _u) % about

  inline def ~[C <: CharVal](short: C): OptNLabelChar[C, L] = OptNLabelChar(label, short)

  inline def ~[C <: CharVal](onc: OptNChar[C]): OptNLabelChar[C, L] =
    compiletime.error(s"Place .x on long-form label only when giving both long and short forms")

  inline def ~[A](parse: Parse[A]): OptN[A, '\u002D', L] = OptN.of(label, '\u002D', parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptN[A, '\u002D', L] = OptN.withDefault(label, '\u002D', pd._1, pd._2)
}

final class OptNChar[C <: CharVal](val short: C) {
  inline def build: OptN[Unit, C, Singleton & ToString[C]] = OptN.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)

  inline def %(about: String): OptN[Unit, C, Singleton & ToString[C]] = OptN.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u) % about

  inline def ~[A](parse: Parse[A]): OptN[A, C, Singleton & ToString[C]] = OptN.of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptN[A, C, Singleton & ToString[C]] = OptN.withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, pd._1, pd._2)
}

final class OptLabelChar[C <: CharVal, L <: LabelVal](val label: L, val short: C) {
  inline def build: Opt[Unit, C, L] = Opt.of(label, short, _u)

  inline def %(about: String): Opt[Unit, C, L] = Opt.of(label, short, _u) % about

  inline def x: OptNLabelChar[C, L] = compiletime.error(s"Place `.x` on long-form option")

  inline def ~[A](parse: Parse[A]): Opt[A, C, L] = Opt.of(label, short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptD[A, C, L] = Opt.withDefault(label, short, pd._1, pd._2)
}

final class OptNLabelChar[C <: CharVal, L <: LabelVal](val label: L, val short: C) {
  def build: OptN[Unit, C, L] = OptN.of(label, short, _u)

  inline def %(about: String): OptN[Unit, C, L] = OptN.of(label, short, _u) % about

  inline def ~[A](parse: Parse[A]): OptN[A, C, L] = OptN.of(label, short, parse)
  inline def ~[A](pd: (Parse[A], () => A)): OptN[A, C, L] = OptN.withDefault(label, short, pd._1, pd._2)
}


final class Opt[A, C <: CharVal, L <: LabelVal] private[cleasy] (val label: L, val short: C, val parse: Parse[A], val about: String = "") {
  inline def parse_?[E >: Alt[Err]](args: Array[String])(using boundary.Label[E]): (Option[(A, Int)] \ L) =
    if short == '\u002D' then
      Parse.whereKey(label)(args) match
        case Array() => \[Option[(A, Int)], L](None)
        case Array(ij) =>
          val (i, j) = ij
          val arg = args(i)
          val v = arg.select(j to End)
          val a = parse(v).fold(__){ e => boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $arg")) }
          \[Option[(A, Int)], L](Some(a, i))
        case idxs => 
          val msg = s"--$label may only be given once but was found ${idxs.length} times\n  at arguments ${Parse.listy(idxs.map{ case (i, _) => s"#${i+1}" })}"
          boundary.break(Err.or(msg))
    else
      Parse.whereEither(label, short)(args) match
        case Array() => \[Option[(A, Int)], L](None)
        case Array(izj) =>
          val (i, isLabel, j) = izj
          val arg = args(i)
          val v = if isLabel then arg.select(j to End) else ""
          val a = parse(v).fold(__){ e => 
            val argmsg =
              if isLabel then arg
              else s"$short in $arg (equivalent to --$label)"
            boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $argmsg"))
          }
          \[Option[(A, Int)], L](Some(a, i))
        case idxs =>
          boundary.break(Err.or(Opt.moreThanOneOptionError(label, short, args, idxs)))

  def %(moreAbout: String) =
    new Opt[A, C, L](label, short, parse, if about.isEmpty || about(End) == '\n' then about + moreAbout else about + "\n" + moreAbout)

  def x =
    new OptN[A, C, L](label, short, parse, None, about)

  override lazy val toString = Opt.customToString(label, short, if parse eq _u then "" else parse.toString, about, if parse eq _u then "" else "?")()
}
object Opt {
  val done = new Opt[Unit, '\u002D', ""]("", '\u002D', _u, "Stop parsing options.")

  inline def apply[L <: LabelVal](label: L): Opt[Unit, '\u002D', L] = of(label, '\u002D', _u)
  inline def apply[C <: CharVal](short: C): Opt[Unit, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)
  inline def apply[C <: CharVal, L <: LabelVal](label: L, short: C): Opt[Unit, C, L] = of(label, short, _u)
  inline def apply[A, L <: LabelVal](label: L, parse: Parse[A]): Opt[A, '\u002D', L] = of(label, '\u002D', parse)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A]): Opt[A, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  def of[A, C <: CharVal, L <: LabelVal](label: L, short: C, parse: Parse[A]): Opt[A, C, L] = new Opt[A, C, L](label, short, parse, "")

  inline def apply[A, L <: LabelVal](label: L, parse: Parse[A], default: () => A): OptD[A, '\u002D', L] = withDefault(label, '\u002D', parse, default)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A], default: () => A): OptD[A, C, Singleton & ToString[C]] = withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse, default)
  def withDefault[A, C <: CharVal, L <: LabelVal](label: L, short: C, parse: Parse[A], default: () => A): OptD[A, C, L] = new OptD[A, C, L](label, short, parse, default, "")
  
  private[cleasy] def moreThanOneOptionError(label: String, short: Char, args: Array[String], idxs: Array[(Int, Boolean, Int)]): String =
    val lbb = Array.newBuilder[Int]
    val shb = Array.newBuilder[Int]
    idxs.peek(): ixj =>
      if ixj._2 then lbb += ixj._1 else shb += ixj._1
    val labelIdx = lbb.result
    val shortIdx = shb.result
    "".build: sb =>
      if shortIdx.length == 0 then sb append s"--$label may only be given once but was found ${labelIdx.length} times\n"
      else if labelIdx.length == 0 then sb append s"-$short (--$label) may only be given once but was found ${shortIdx.length} times\n"
      else sb append s"--$label (-$short) may only be given once but were found ${idxs.length} times\n"
      if labelIdx.length > 0 then
        val msgIfShort = if shortIdx.length > 0 then s"as --$label " else ""
        sb append s"  ${msgIfShort}at arguments ${Parse.listy(labelIdx.map(i => s"#${i+1}"))}"
      if shortIdx.length > 0 then
        val shortArgs = shortIdx.diced(shortIdx.breakable.copyOp{ (ix, j) => shortcut.skipIf(j == 0 || shortIdx(j-1) == ix); j } , "[)")
        val argsMsg = shortArgs.copyWith: ixs =>
          if ixs.length == 1 then
            if args(ixs(0)).length == 2 then "#" + (ixs(0)+1)
            else s"#${ixs(0)+1} (in ${args(ixs(0))})"
          else s"#${ixs(0)+1} (${ixs.length}x in ${args(ixs(0))})"
        val msgIfLong = if labelIdx.length > 0 then s"as -$short " else ""
        sb append s"  ${msgIfLong}at argument${if shortArgs.length == 1 then "" else "s"} ${Parse.listy(argsMsg)}"

  private[cleasy] def customToString[A](label: String, short: Char, parse: String, about: String, special: String)(indent: Int = 2, labelW: Int = -1, shortW: Int = -1, parseW: Int = -1, aboutW: Int = -1, specialW: Int = -1, margin: Int = 80, clip: Boolean = false, wrap: Int = 0) =
    "".build: sb =>
      val start = sb.length
      indent.times:
        sb append ' '
      var overage = 0
      var useOverage = labelW >= 0
      inline def pad(n: Int)(f: => Unit): Unit =
        val i0 = sb.length
        f
        if clip && n >= 0 && (sb.length - i0) > (n max 1) then
          sb.setLength(i0 + n - 1)
          sb append '\u2026'
        val iN = sb.length
        if useOverage then
          if n > iN - i0 then
            (n - (iN - i0)).times:
              if overage > 0 then overage -= 1
              else sb append ' '
          else if n > 0 then overage += (iN - i0) - n
          else useOverage = n == 0
      pad(labelW):
        sb append "--"
        sb append label
      pad(shortW):
        if short != '-' then
          sb append " -"
          sb append short
      pad(specialW):
        if special.nonEmpty then
          sb append ' '
          sb append special
      pad(parseW):
        if parse.nonEmpty then
          sb append ' '
          sb append parse
      if about.length == 1 || margin <= 0 || margin - sb.length > 1 + about.length then
        sb append ' '
        sb append about
      else if about.length > 0 then
        sb append ' '
        val in = sb.length - start
        if margin - in < 3 then sb append '\u2026'
        else
          var w = wrap
          val idxs = about.where(_.isWhitespace)
          var i = 0
          var ii = 0
          while i < about.length && w > 0 && about.length - i >= (margin - in) do
            while ii+1 < idxs.length && idxs(ii+1) - i < (margin - in) do ii += 1
            if ii < idxs.length && idxs(ii) > i then
              sb.append(about, i, idxs(ii))
              i = idxs(ii) + 1
              while ii+1 < idxs.length && i == idxs(ii+1) do
                i += 1
                ii += 1
              if i < about.length then
              sb append '\n'
              in.times:
                sb append ' '
            else
              sb.append(about, i, i + (margin - in - 2))
              sb append '\u21b5'
              sb append '\n'
              in.times:
                sb append ' '
              i += (margin - in - 2)
            w -= 1
          if about.length - i < margin - in then sb.append(about, i, about.length)
          else
            sb.append(about, i, i + (margin - in - 2))
            sb append '\u2026'
      if sb.charAt(sb.length - 1) != '\n' then sb append '\n'
}

final case class OptN[A, C <: CharVal, L <: LabelVal] private[cleasy] (val label: L, short: C, parse: Parse[A], default: Option[() => A], about: String) {
  inline def parse_?[E >: Alt[Err]](args: Array[String])(using boundary.Label[E]): (List[(A, Int)] \ L) =
    val lb = List.newBuilder[(A, Int)]
    if short == '\u002D' then
      val idxs = Parse.whereKey(label)(args)
      if idxs.isEmpty then
        default match
          case Some(a) => lb += ((a(), -1))
          case _ =>
      else
        idxs.peek(): ij =>
          val (i, j) = ij
          val arg = args(i)
          val v = arg.select(j to End)
          val a = parse(v).fold(__){ e => boundary.break(Alt(e +# s"Error parsing option at argument ${i+1}, $arg")) }
          lb += ((a, i))
    else
      val idxs = Parse.whereEither(label, short)(args)
      idxs.peek(): izj =>
        val (i, isLabel, j) = izj
        val arg = args(i)
        val v = if isLabel then arg.select(j to End) else ""
        val a = parse(v).fold(__){ e =>
          val argmsg =
            if isLabel then arg
            else s"$short in $arg (equivalent to --$label)"
          boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $argmsg"))
        }
        lb += ((a, i))
    \[List[(A, Int)], L](lb.result)

  def %(moreAbout: String) =
    new OptN[A, C, L](label, short, parse, default, if about.isEmpty || about(End) == '\n' then about + moreAbout else about + "\n" + moreAbout)

  override lazy val toString = Opt.customToString(label, short, if parse eq _u then "" else parse.toString, about, "*")()
}
object OptN {
  inline def apply[L <: LabelVal](label: L): OptN[Unit, '\u002D', L] = of(label, '\u002D', _u)
  inline def apply[C <: CharVal](short: C): OptN[Unit, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)
  inline def apply[C <: CharVal, L <: LabelVal](label: L, short: C): OptN[Unit, C, L] = of(label, short, _u)
  inline def apply[A, L <: LabelVal](label: L, parse: Parse[A]): OptN[A, '\u002D', L] = of(label, '\u002D', parse)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A]): OptN[A, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse)
  def of[A, C <: CharVal, L <: LabelVal](label: L, short: C, parse: Parse[A]): OptN[A, C, L] = new OptN(label, short, parse, None, "")

  inline def apply[A, L <: LabelVal](label: L, parse: Parse[A], default: () => A): OptN[A, '\u002D', L] = withDefault(label, '\u002D', parse, default)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A], default: () => A): OptN[A, C, Singleton & ToString[C]] = withDefault(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse, default)
  def withDefault[A, C <: CharVal, L <: LabelVal](label: L, short: C, parse: Parse[A], default: () => A): OptN[A, C, L] = new OptN(label, short, parse, Some(default), "")
}

final case class OptD[A, C <: CharVal, L <: LabelVal] private[cleasy] (val label: L, short: C, parse: Parse[A], default: () => A, about: String) {
  inline def parse_?[E >: Alt[Err]](args: Array[String])(using boundary.Label[E]): ((A, Option[Int]) \ L) =
    if short == '\u002D' then
      Parse.whereKey(label)(args) match
        case Array() => \[(A, Option[Int]), L]((default(), None))
        case Array(ij) =>
          val (i, j) = ij
          val arg = args(i)
          val v = arg.select(j to End)
          val a = parse(v).fold(__){ e => boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $arg")) }
          \[(A, Option[Int]), L]((a, Some(i)))
        case idxs => 
          val msg = s"--$label may only be given once but was found ${idxs.length} times\n  at arguments ${Parse.listy(idxs.map{ case (i, _) => s"#${i+1}" })}"
          boundary.break(Err.or(msg))
    else
      Parse.whereEither(label, short)(args) match
        case Array() => \[(A, Option[Int]), L]((default(), None))
        case Array(izj) =>
          val (i, isLabel, j) = izj
          val arg = args(i)
          val v = if isLabel then arg.select(j to End) else ""
          val a = parse(v).fold(__){ e => 
            val argmsg =
              if isLabel then arg
              else s"$short in $arg (equivalent to --$label)"
            boundary.break(Alt(e +# s"Error parsing option at argument ${i + 1}, $argmsg"))
          }
          \[(A, Option[Int]), L]((a, Some(i)))
        case idxs =>
          boundary.break(Err.or(Opt.moreThanOneOptionError(label, short, args, idxs)))

  def %(moreAbout: String) =
    new OptD[A, C, L](label, short, parse, default, if about.isEmpty || about(End) == '\n' then about + moreAbout else about + "\n" + moreAbout)

  def x =
    new OptN[A, C, L](label, short, parse, Some(default), about)

  override lazy val toString = Opt.customToString(label, short, if parse eq _u then "" else parse.toString, about, "!")()
}
object OptD {
  inline def apply[A, L <: LabelVal](label: L, parse: Parse[A], default: () => A): OptD[A, '\u002D', L] = of(label, '\u002D', parse, default)
  inline def apply[A, C <: CharVal](short: C, parse: Parse[A], default: () => A): OptD[A, C, Singleton & ToString[C]] = of(constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, parse, default)
  def of[A, C <: CharVal, L <: LabelVal](label: L, short: C, parse: Parse[A], default: () => A): OptD[A, C, L] = new OptD(label, short, parse, default, "")
}


final class Args[N <: LabelVal, T <: Tuple](val original: Array[String], labels: Vector[N], parsed: T) {
  val indexedArgs =
    var stopped = false
    original.breakable.copyOp: (arg, i) =>
      if !stopped then
        stopped = (arg == "--" || arg == "--=")
        shortcut.skipIf(arg.startsWith("-"))
      (arg, i)
  val args = indexedArgs.copyWith(_._1)

  lazy val options: Args.AsNamedTuple[T] = Args.asNamedTuple(parsed)

  transparent inline def option[L <: LabelVal](label: L): Args.Extract[L, T] = summonFrom {
    case have: (L <:< N) => Args.extract[L, T](label, parsed)
    case _ => compiletime.error("Option by that name does not exist.")
  }
  transparent inline def ~[L <: LabelVal](label: L) = summonFrom {
    case have: (L <:< N) => inline option[L](label) match
      case ao: (a, Option[Int]) => ao._1
      case oa: Option[(a, Int)] => oa.map(_._1)
      case os: List[(a, Int)] => os.map(_._1)
      case _ => compiletime.error("Cannot simplify this type.")
    case _ => compiletime.error("Option by that name does not exist.")
  }
  inline infix def found[L <: LabelVal](label: L) = summonFrom {
    case have: (L <:< N) => inline option[L](label) match
      case (_, oi: Option[Int]) => oi.isDefined
      case oa: Option[(?, Int)] => oa.isDefined
      case os: List[(?, Int)] => os match
        case Nil => false
        case (_, i) :: Nil if i < 0 => false
        case _ => true
      case _ => compiletime.error("Unexpected type among parsed options")
  }
}
object Args {
  import compiletime.ops.int.*

  final class Elt[L <: LabelVal, A](val value: A) {} // = kse.basics.\[A, L]
  inline def elt[L <: LabelVal, A](x: \[A, L]): Elt[L, A] = new Elt[L, A](x.unlabel)

  type Extract[L <: LabelVal, T <: Tuple] = T match
    case EmptyTuple => Nothing
    case Elt[L, a] *: ts => a
    case t *: ts => Extract[L, ts]

  type AsNamedTuple[T <: Tuple] = T match
    case EmptyTuple => EmptyTuple
    case Elt[l, a] *: ts => \[a, l] *: AsNamedTuple[ts]

  transparent inline def extractUncertainTypeByLabel[L <: LabelVal, T <: Tuple](label: L, tuple: T): Any =
    inline compiletime.erasedValue[T] match
      case _: EmptyTuple => compiletime.error("Label not found")
      case _: (Elt[L, a] *: tp) => tuple.asInstanceOf[Elt[L, a] *: tp].head.value
      case _: (t *: tp) => extractUncertainTypeByLabel[L, tp](label, tuple.drop(1).asInstanceOf[tp])

  inline def extract[L <: LabelVal, T <: Tuple](label: L, tuple: T): Extract[L, T] = 
    extractUncertainTypeByLabel[L, T](label, tuple).asInstanceOf[Extract[L, T]]

  inline def asNamedTuple[T <: Tuple](tuple: T): AsNamedTuple[T] =
    var work: Tuple = tuple
    var result: Tuple = EmptyTuple
    loop:
      work match
        case t *: rest => t match
          case e: Elt[?, ?] =>
            result = result :* e.value
            work = rest
          case _ => Err("Internal error: trying to read options but did not put them in Elt").toss
        case _ => false.?
    result.asInstanceOf[AsNamedTuple[T]]
}


final class Cleasy[N <: LabelVal, H <: CharVal, T <: Tuple](title: String, shorts: Vector[H], labels: Vector[N], options: T) {
  transparent inline def +[A, C <: CharVal, L <: LabelVal](op: Opt[A, C, L]) = summonFrom {
    case have: (L <:< N) => compiletime.error("Option with that name already exists.")
    case okay: (C <:< '\u002D') => new Cleasy(title, shorts, (labels :+ op.label): Vector[N | L], options :* op)
    case have: (C <:< H) => compiletime.error("Option with that short name already exists.")
    case _ => new Cleasy(title, (shorts :+ op.short): Vector[H | C], (labels :+ op.label): Vector[N | L], options :* op)
  }
  transparent inline def +[A, C <: CharVal, L <: LabelVal](op: OptN[A, C, L]) = summonFrom {
    case have: (L <:< N) => compiletime.error("Option with that name already exists.")
    case okay: (C <:< '\u002D') => new Cleasy(title, shorts, (labels :+ op.label): Vector[N | L], options :* op)
    case have: (C <:< H) => compiletime.error("Option with that short name already exists.")
    case _ => new Cleasy(title, (shorts :+ op.short): Vector[H | C], (labels :+ op.label): Vector[N | L], options :* op)
  }
  transparent inline def +[A, C <: CharVal, L <: LabelVal](op: OptD[A, C, L]) = summonFrom {
    case have: (L <:< N) => compiletime.error("Option with that name already exists.")
    case okay: (C <:< '\u002D') => new Cleasy(title, shorts, (labels :+ op.label): Vector[N | L], options :* op)
    case have: (C <:< H) => compiletime.error("Option with that short name already exists.")
    case _ => new Cleasy(title, (shorts :+ op.short): Vector[H | C], (labels :+ op.label): Vector[N | L], options :* op)
  }

  transparent inline def --[L <: LabelVal](label: L) = this + Opt.of[Unit, '\u002D', L](label, '\u002D', _u)
  transparent inline def --[C <: CharVal](short: C) = this + Opt.of[Unit, C, Singleton & ToString[C]](constValue[ToString[C]].asInstanceOf[Singleton & ToString[C]], short, _u)
  
  transparent inline def --[L <: LabelVal](onl: OptNLabel[L]) = this + onl.build
  transparent inline def --[C <: CharVal](onc: OptNChar[C]) = this + onc.build
  transparent inline def --[C <: CharVal, L <: LabelVal](olc: OptLabelChar[C, L]) = this + olc.build
  transparent inline def --[C <: CharVal, L <: LabelVal](onlc: OptNLabelChar[C, L]) = this + onlc.build

  transparent inline def --[A, C <: CharVal, L <: LabelVal](o: Opt[A, C, L]) = this + o
  transparent inline def --[A, C <: CharVal, L <: LabelVal](o: OptN[A, C, L]) = this + o
  transparent inline def --[A, C <: CharVal, L <: LabelVal](o: OptD[A, C, L]) = this + o

  transparent inline def parse[E >: Alt[Err]](args: Array[String]) = Err.Or:
    escape:
      args.visit(): (arg, i) =>
        (arg != "--" && arg != "--=").?
        if arg.startsWith("--") then
          val i = arg.indexOf('=')
          val argname = if i < 0 then arg.select(2 to End) else arg.select(2, i)
          if !labels.contains(argname) then
            Err ?# s"No such option: argname\n  in argument ${i+1}, $arg"
        else if arg.startsWith("-") then
          val unknown = arg.drop(1).filterNot(shorts contains _)
          if unknown.nonEmpty then
            Err ?# s"No such option${if unknown.length > 1 then "s" else ""}: ${Parse.listy(unknown.map(_.toString))}\n  in argument ${i+1}, $arg"
    Args(args, labels, Cleasy.parse_?(options, args))

  def userString(margin: Int = 80, debug: Boolean = false, paramMax: Int = 16) =
    title.build: sb =>
      if sb.length > 0 && sb.charAt(sb.length - 1) != '\n' then sb append '\n'
      var ops: Tuple = options
      var labelW = 0
      var shortW = 0
      var specialW = 0
      var parseW = 0
      var aboutW = 0
      loop:
        ops match
          case EmptyTuple => false.?
          case t *: tp =>
            ops = tp
            t match
              case o: Opt[?, ?, ?] =>
                labelW = labelW max o.label.length + 2
                if o.short != '-' then shortW = shortW max 3
                if o.parse ne _u then
                  if debug then parseW = parseW max o.parse.toString.length + 2
                  else parseW = parseW max o.parse.userString.map(_.length + 1 min paramMax).getOrElse(0)
                aboutW = aboutW max o.about.length
              case o: OptN[?, ?, ?] =>
                labelW = labelW max o.label.length + 2
                if o.short != '-' then shortW = shortW max 3
                if o.parse ne _u then
                  if debug then parseW = parseW max o.parse.toString.length + 2
                  else parseW = parseW max o.parse.userString.map(_.length + 1 min paramMax).getOrElse(0)
                aboutW = aboutW max o.about.length
              case o: OptD[?, ?, ?] =>
                labelW = labelW max o.label.length + 2
                if o.short != '-' then shortW = shortW max 3
                if o.parse ne _u then
                  if debug then parseW = parseW max o.parse.toString.length + 1
                  else parseW = parseW max o.parse.userString.map(_.length + 1 min paramMax).getOrElse(0)
                aboutW = aboutW max o.about.length
      if !debug then
        if sb.length > 1 && sb.charAt(sb.length - 2) != '\n' then sb append '\n'
        if labelW < 4 then labelW = 4
        if aboutW < 4 then aboutW = 4
        val lower = "".build: s2 =>
          if labelW < 6 then
            sb append "Name"
            s2 append "----"
            if labelW == 5 then
              sb append ' '
              s2 append '-'
          else
            sb append "Option"
            s2 append "------"
            (labelW - 6).times:
              sb append ' '
              s2 append '-'
          if shortW > 0 then
            sb append " ch"
            s2 append " --"
          if parseW > 0 then
            if parseW < 4 then parseW = 4
            if parseW < 7 then
              sb append " Arg"
              s2 append " ---"
              (parseW - 4).times:
                sb append ' '
                s2 append '-'
            else
              sb append " Value"
              s2 append " -----"
              (parseW - 7).times:
                sb append ' '
                s2 append '-'
          if aboutW > 0 then
            if aboutW < 5 then aboutW = 5
            if aboutW < 15 then
              sb append " Desc"
              s2 append " ----"
              aboutW -= 5
            else
              sb append " Description"
              s2 append " -----------"
              aboutW -= 12
            if aboutW > margin - 1 - s2.length then aboutW = margin - 1 - s2.length
            aboutW.times:
              sb append ' '
              s2 append '-'
          sb append '\n'
          s2 append '\n'
        sb append lower
      ops = options
      loop:
        ops match
          case EmptyTuple => false.?
          case t *: tp =>
            ops = tp
            if t.asInstanceOf[AnyRef] ne Opt.done then
              t match
                case o: Opt[?, ?, ?]  =>
                  val ps =
                    if debug then "?" + o.parse.toString
                    else o.parse.userString.getOrElse("").take(paramMax-1)
                  sb append Opt.customToString(o.label, o.short, ps, o.about, "")(indent = if debug then 2 else 0, labelW = labelW, shortW = shortW, parseW = parseW, specialW = specialW, wrap = if debug then 0 else 5)
                case o: OptN[?, ?, ?] =>
                  val ps =
                    if debug then "*" + o.parse.toString
                    else o.parse.userString.getOrElse("").take(paramMax-1)
                  sb append Opt.customToString(o.label, o.short, ps, o.about, "")(indent = if debug then 2 else 0, labelW = labelW, shortW = shortW, parseW = parseW, specialW = specialW, wrap = if debug then 0 else 5)
                case o: OptD[?, ?, ?] =>
                  val ps =
                    if debug then o.parse.toString
                    else o.parse.userString.getOrElse("").take(paramMax-1)
                  sb append Opt.customToString(o.label, o.short, ps, o.about, "")(indent = if debug then 2 else 0, labelW = labelW, shortW = shortW, parseW = parseW, specialW = specialW, wrap = if debug then 0 else 5)
                case _ => sb append t.toString                  
            if sb.length > 0 && sb.charAt(sb.length - 1) != '\n' then sb append '\n'
      sb append "Use -- to end option parsing. Short options may be combined as -abc.\n"

  override lazy val toString = userString(debug = true)
}
object Cleasy {
  type Parsed[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Opt[a, ?, l] *: ts => Args.Elt[l, Option[(a, Int)]] *: Parsed[ts]
    case OptN[a, ?, l] *: ts => Args.Elt[l, List[(a, Int)]] *: Parsed[ts]
    case OptD[a, ?, l] *: ts => Args.Elt[l, (a, Option[Int])] *: Parsed[ts]
    case t *: ts => t *: Parsed[ts]

  def parse_?[T <: Tuple, E >: Alt[Err]](specification: T, arguments: Array[String])(using boundary.Label[E]): Parsed[T] =
    var work: Tuple = specification
    var result: Tuple = EmptyTuple
    loop:
      work match
        case t *: rest => t match
          case o: Opt[?, ?, ?] =>
            result = result :* Args.elt(o.parse_?(arguments))
            work = rest
          case o: OptN[?, ?, ?] =>
            result = result :* Args.elt(o.parse_?(arguments))
            work = rest
          case o: OptD[?, ?, ?] =>
            result = result :* Args.elt(o.parse_?(arguments))
            work = rest
          case _ => boundary.break(Err.or("Internal error: trying to parse options but not using Opt"))
        case _ => false.?
    result.asInstanceOf[Parsed[T]]

  def goodCaps(s: String, ts: String*) =
    ts.exists(t => s.equalsIgnoreCase(t)) &&
    (s.forall(_.isLower) || s.forall(_.isUpper) || (s.length > 1 && s.head.isUpper && s.tail.forall(_.isLower)))

  def apply(title: String = "") = new Cleasy(title, Vector('\u002D'), Vector(""), EmptyTuple :* Opt.done)
}
