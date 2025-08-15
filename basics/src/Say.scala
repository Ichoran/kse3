// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-25 Rex Kerr.

package kse.basics


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{StringBuilder => StB}

import scala.annotation.targetName

import kse.basics.intervals._


opaque type CodePoint = Int
object CodePoint extends Translucent.Companion[CodePoint, Int] {
  inline def apply(i: Int): CodePoint = i
  inline def wrap(i: Int): CodePoint = i
  inline def wrap(hi: Char, lo: Char): CodePoint = java.lang.Character.toCodePoint(hi, lo)

  extension (cp: CodePoint) {
    inline def value: Int = cp
    inline def unwrap: Int = cp

    inline def isValid: Boolean = java.lang.Character.isValidCodePoint(cp: Int)
    inline def isSingle: Boolean = java.lang.Character.isBmpCodePoint(cp: Int)
    inline def charCount: Int = java.lang.Character.charCount(cp: Int)
    inline def asChar: Char = if java.lang.Character.isBmpCodePoint(cp: Int) then (cp: Int).toChar else '\uFFFD'
    inline def high: Char = if java.lang.Character.isBmpCodePoint(cp: Int) then '\uFFFD' else java.lang.Character.highSurrogate(cp: Int)
    inline def low: Char = if java.lang.Character.isBmpCodePoint(cp: Int) then '\uFFFD' else java.lang.Character.lowSurrogate(cp: Int)
    inline def chars: Array[Char] = java.lang.Character.toChars(cp: Int)
  }
}


extension (s: String) {
  inline def codept(pt: Iv.Pt): CodePoint = CodePoint(s.codePointAt(Iv.point(pt, s)))
  inline def make(inline f: MkStr => Unit): String =
    val sb = new java.lang.StringBuilder(s)
    f(sb)
    sb.toString
  inline def maker(): MkStr = MkStr.wrap(new java.lang.StringBuilder(s))
}


opaque type MkStr = StB
object MkStr {
  import collection.immutable.{Range => Rg}
  import kse.basics.labels._
  import kse.basics.intervals._

  inline def wrap(sb: StB): MkStr = sb

  inline def empty(): MkStr = new java.lang.StringBuilder()
  inline def ofSize(n: Int): MkStr = new java.lang.StringBuilder(n)

  inline def apply(inline f: MkStr => Unit): String =
    val sb = new java.lang.StringBuilder
    f(sb)
    sb.toString

  inline def apply(n: Int)(inline f: MkStr => Unit): String =
    val sb = new java.lang.StringBuilder(n)
    f(sb)
    sb.toString

  type Addable = Boolean | Char | Array[Char] | Double | Float | Int | CodePoint | Long | String | CharSequence | MkStr | AnyRef
  type RangeAddable = Array[Char] | CharSequence

  extension (sb: MkStr) {
    inline def unwrap: StB = sb

    inline def apply(pt: Iv.Pt): Char = (sb: StB).charAt(Iv.point(pt, (sb: StB).length()))
    inline def update(pt: Iv.Pt, c: Char): Unit = (sb: StB).setCharAt(Iv.point(pt, (sb: StB).length()), c)

    inline def +=(x: Addable): Unit = inline x match
      case b: Boolean       => (sb: StB).append(b): Unit
      case c: Char          => (sb: StB).append(c): Unit
      case ac: Array[Char]  => (sb: StB).append(ac): Unit
      case d: Double        => (sb: StB).append(d): Unit
      case f: Float         => (sb: StB).append(f): Unit
      case i: Int           => (sb: StB).append(i): Unit
      case c: CodePoint     => (sb: StB).appendCodePoint(c: Int): Unit
      case l: Long          => (sb: StB).append(l): Unit
      case s: String        => (sb: StB).append(s): Unit
      case ms: MkStr        => (sb: StB).append(ms: CharSequence): Unit
      case cs: CharSequence => (sb: StB).append(cs): Unit
      case ar: Array[?]     => compiletime.error("Arrays must be added element by element")
      case a: AnyRef        => (sb: StB).append(a): Unit

    inline def add(xs: RangeAddable, i0: Int, iN: Int) = inline xs match
      case ac: Array[Char]  => (sb: StB).append(ac, i0, iN - i0): Unit
      case cs: CharSequence => (sb: StB).append(cs, i0, iN): Unit
    inline def add(xs: RangeAddable, ivx: Iv.X): Unit = inline xs match
      case ac: Array[Char] =>
        val i0 = ivx.index0(ac)
        val n = ivx.indexN(ac) - i0
        (sb: StB).append(ac, i0, n): Unit
      case cs: CharSequence =>
        val n = cs.length()
        (sb: StB).append(cs, ivx.index0(n), ivx.indexN(n)): Unit
    inline def add(xs: RangeAddable, inline rg: Range): Unit =
      val iv = Iv of rg
      inline xs match
        case ac: Array[Char] =>
          val i0 = iv.i0
          (sb: StB).append(ac, i0, iv.iN - i0): Unit
        case cs: CharSequence =>
          (sb: StB).append(cs, iv.i0, iv.iN): Unit

    inline def addln(): Unit =
      (sb: StB).append('\n'): Unit
    inline infix def addln(x: Addable): Unit =
      MkStr.+=(sb)(x)
      (sb: StB).append('\n'): Unit
    inline def addln(xs: RangeAddable, i0: Int, iN: Int): Unit =
      MkStr.add(sb)(xs, i0, iN)
      (sb: StB).append('\n'): Unit
    inline def addln(xs: RangeAddable, ivx: Iv.X): Unit =
      MkStr.add(sb)(xs, ivx)
      (sb: StB).append('\n'): Unit
    inline def addln(xs: RangeAddable, inline rg: Rg): Unit =
      MkStr.add(sb)(xs, rg)
      (sb: StB).append('\n'): Unit

    inline def capacity: Int = (sb: StB).capacity()
    inline def capacity_=(i: Int): Unit = (sb: StB).ensureCapacity(i)
    inline def trimCapacity(): Unit = (sb: StB).trimToSize()

    inline def codept(i: Int): CodePoint = CodePoint((sb: StB).codePointAt(i))

    inline def codeCount(i0: Int, iN: Int): Int = (sb: StB).codePointCount(i0, iN)
    inline def codeCount(ivx: Iv.X): Int =
      val i0 = ivx.index0(sb.length)
      val iN = ivx.indexN(sb.length)
      (sb: StB).codePointCount(i0, iN)
    inline def codeCount(inline rg: Rg): Int =
      val iv = Iv of rg
      (sb: StB).codePointCount(iv.i0, iv.iN)

    inline def del(target: Iv.Pt): Unit = (sb: StB).deleteCharAt(Iv.point(target, (sb: StB).length())): Unit
    inline def del(i0: Int, iN: Int): Unit = (sb: StB).delete(i0, iN): Unit
    inline def del(ivx: Iv.X): Unit = (sb: StB).delete(ivx.index0((sb: StB).length()), ivx.indexN(((sb: StB).length()))): Unit
    inline def del(inline rg: Rg): Unit =
      val iv = Iv of rg
      (sb: StB).delete(iv.i0, iv.iN): Unit

    inline def getChars(i0: Int, iN: Int, target: Array[Char], where: Iv.Pt): Unit = (sb: StB).getChars(i0, iN, target, Iv.point(where, target))
    inline def getChars(ivx: Iv.X, target: Array[Char], where: Iv.Pt): Unit = (sb: StB).getChars(ivx.index0((sb: StB).length()), ivx.indexN((sb: StB).length()), target, Iv.point(where, target))
    inline def getChars(inline rg: Rg, target: Array[Char], where: Iv.Pt): Unit =
      val iv = Iv of rg
      (sb: StB).getChars(iv.i0, iv.iN, target, Iv.point(where, target))

    def indexOf(c: Char, i0: Int = 0): Int =
      if i0 >= (sb: StB).length() then -1
      else if c == (sb: StB).charAt(i0) then i0
      else indexOf(c, i0+1)
    inline def indexOf(s: String): Int = (sb: StB).indexOf(s)
    inline def indexOf(s: String, i0: Int): Int = (sb: StB).indexOf(s, i0)
    def lastIndexOf(c: Char, i1: Int = (sb: StB).length() - 1): Int =
      if i1 < 0 then -1
      else if c == (sb: StB).charAt(i1) then i1
      else lastIndexOf(c, i1-1)
    inline def lastIndexOf(s: String): Int = (sb: StB).lastIndexOf(s)
    inline def lastIndexOf(s: String, i1: Int): Int = (sb: StB).lastIndexOf(s, i1)

    inline def ins(target: Iv.Pt, x: Addable): Unit =
      val j = Iv.point(target, (sb: StB).length())
      inline x match
        case b: Boolean       => (sb: StB).insert(j, b): Unit
        case c: Char          => (sb: StB).insert(j, c): Unit
        case ac: Array[Char]  => (sb: StB).insert(j, ac): Unit
        case d: Double        => (sb: StB).insert(j, d): Unit
        case f: Float         => (sb: StB).insert(j, f): Unit
        case i: Int           => (sb: StB).insert(j, i): Unit
        case c: CodePoint     => if CodePoint.isSingle(c) then (sb: StB).insert(j, (c: Int).toChar): Unit else (sb: StB).insert(j, CodePoint.chars(c)): Unit
        case l: Long          => (sb: StB).insert(j, l): Unit
        case s: String        => (sb: StB).insert(j, s): Unit
        case ms: MkStr        => (sb: StB).insert(j, ms: CharSequence): Unit
        case cs: CharSequence => (sb: StB).insert(j, cs): Unit
        case ar: Array[?]     => compiletime.error("Arrays should be stringified element-by-element then inserted")
        case a: AnyRef        => (sb: StB).insert(j, a): Unit
    inline def ins(target: Iv.Pt, ra: RangeAddable, i0: Int, iN: Int): Unit =
      val j = Iv.point(target, (sb: StB).length())
      inline ra match
        case ac: Array[Char]  => (sb: StB).insert(j, ac, i0, iN - i0): Unit
        case cs: CharSequence => (sb: StB).insert(j, cs, i0, iN): Unit
    inline def ins(target: Iv.Pt, ra: RangeAddable, ivx: Iv.X): Unit =
      val n = (sb: StB).length()
      val j = Iv.point(target, n)
      val i0 = ivx.index0(n)
      val iN = ivx.indexN(n)
      inline ra match
        case ac: Array[Char]  => (sb: StB).insert(j, ac, i0, iN - i0): Unit
        case cs: CharSequence => (sb: StB).insert(j, cs, i0, iN): Unit
    inline def ins(target: Iv.Pt, ra: RangeAddable, inline rg: Rg): Unit =
      val j = Iv.point(target, (sb: StB).length())
      val iv = Iv of rg
      val i0 = iv.i0
      inline ra match
        case ac: Array[Char]  => (sb: StB).insert(j, ac, i0, iv.iN - i0): Unit
        case cs: CharSequence => (sb: StB).insert(j, cs, i0, iv.iN): Unit

    inline def length: Int = (sb: StB).length()
    inline def length_=(n: Int): Unit = (sb: StB).setLength(n)

    inline def repeat(c: Char, n: Int): Unit = (sb: StB).repeat(c, n): Unit
    inline def repeat(cp: CodePoint, n: Int): Unit = (sb: StB).repeat(CodePoint.value(cp), n): Unit
    inline def repeat(s: String, n: Int): Unit = (sb: StB).repeat(s, n): Unit
    inline def repeat(cs: CharSequence, n: Int): Unit = (sb: StB).repeat(cs, n): Unit
    inline def repeat(ms: MkStr, n: Int): Unit = (sb: StB).repeat(ms: StB, n): Unit

    inline def reverse(): Unit = (sb: StB).reverse(): Unit

    inline def str(): String = (sb: StB).toString
    inline def str(i0: Int, iN: Int): String = (sb: StB).substring(i0, iN)
    inline def str(ivx: Iv.X): String =
      val n = (sb: StB).length()
      (sb: StB).substring(ivx.index0(n), ivx.indexN(n))
    inline def str(inline rg: Rg): String = 
      val iv = Iv of rg
      (sb: StB).substring(iv.i0, iv.iN)
  }
}


trait Say[A] {
  def apply(a: A)(using fmt: Say.Format): String
  def into(a: A)(stb: StB)(using fmt: Say.Format): Say.Anchor
}
object Say {
  opaque type Anchor = Int
  object Anchor {
    inline def wrap(i: Int): Anchor = i
    extension (a: Anchor)
      inline def unwrap: Int = a
  }

  class Format(final val columns: Int = Int.MaxValue, final val rows: Int = 1, final val digits: Float = Float.NaN) {}
  object Format {
    given default: Format = new Format()
  }

  trait Plainly[A] extends Say[A] {
    final def into(a: A)(stb: StB)(using fmt: Say.Format): Say.Anchor =
      stb append apply(a)(using fmt)
      Anchor.wrap(Int.MaxValue)
  }
  trait Into[A] extends Say[A] {
    final def apply(a: A)(using fmt: Say.Format): String =
      val stb = new StB()
      into(a)(stb)(using fmt) __ Unit
      stb.toString
  }

  trait Me {
    def into(stb: StB): Unit
  }

  given Say[AnyRef] = new Plainly[AnyRef]:
    def apply(a: AnyRef)(using fmt: Format): String = a.toString
}


given instantiateSayMe[A](using fmt: Say.Format, aSay: Say[A]) : Conversion[A, Say.Me] with
  def apply(a: A): Say.Me = new:
    def into(stb: StB): Unit = aSay.into(a)(stb)(using fmt) __ Unit


extension [A](a: A)
  inline def say()(using fmt: Say.Format, aSay: Say[A]): String = aSay(a)(using fmt)
  inline def sayInto(stb: StB)(using fmt: Say.Format, aSay: Say[A]): Say.Anchor = aSay.into(a)(stb)(using fmt)

extension (stb: StB)
  inline infix def hear[A](a: A)(using fmt: Say.Format, aSay: Say[A]): Say.Anchor =
    aSay.into(a)(stb)(using fmt)

extension (sc: StringContext)
  inline def say(inline sms: Say.Me*): String =
    val stb = new StB()
    var i = 0
    while i < sms.length do
      stb append sc.parts(i)
      sms(i).into(stb)
      i += 1
    if i < sc.parts.length then stb append sc.parts(i) __ Unit
    stb.toString
