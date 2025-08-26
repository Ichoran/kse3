// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-25 Rex Kerr.

package kse.basics


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{StringBuilder => StB}

import scala.annotation.targetName
import scala.util.boundary

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

  def indentBy(pad: String): String =
    var n = 0
    s.visitLineIndices(): (i0, iN) =>
      n += 1
    val sb = new java.lang.StringBuilder(n*pad.length + s.length)
    s.visitLineIndices(): (i0, iN) =>
      sb.append(pad): Unit
      sb.append(s, i0, iN): Unit
    sb.toString

  def dedentBy(pad: String, skipEmptyStart: Boolean = false): String =
    if s.length == 0 || (skipEmptyStart && ((s.length == 1 && s.charAt(0) == '\n') || (s.length == 2 && s == "\r\n"))) then ""
    else
      val sb = new java.lang.StringBuilder(s.length)
      var x0 = 0
      if skipEmptyStart && x0 < s.length then
        if s.charAt(x0) == '\n' then x0 += 1
        else if s.charAt(x0) == '\r' && x0+1 < s.length && s.charAt(x0+1) == '\n' then x0 += 2
      s.visitLineIndices(x0, s.length): (i0, iN) =>
        var i = i0
        var j = 0
        while i < iN && j < pad.length && s.charAt(i) == pad.charAt(j) do
          i += 1
          j += 1
        if i == iN && iN < s.length then i -= 1
        sb.append(s, i, iN): Unit
      sb.toString

  def dedent(max: Int = Int.MaxValue, skipEmptyStart: Boolean = true): String =
    var n = max
    var j0 = -1
    var jN = -1
    var canExtend = true
    boundary[Unit]:
      s.visitLineIndices(): (i0, iN) =>
        var i = i0
        val k =
          if iN - i0 > n - 1 then i0 + n
          else if iN > i0 && s.charAt(iN-1) == '\n' then
            if iN > i0+1 && s.charAt(iN-2) == '\r' then iN - 2
            else iN - 1
          else iN - i0
        if j0 < 0 then
          var blank = true
          while i < k && { blank = java.lang.Character.isWhitespace(s.charAt(i)); blank } do i += 1
          if i > i0 then
            j0 = i0
            jN = i
            canExtend = blank
        else
          var j = j0
          while i < k && j < jN && s.charAt(i) == s.charAt(j) do
            i += 1
            j += 1
          if j == jN then
            if i < k && canExtend then
              var blank = true
              while i < k && { blank = java.lang.Character.isWhitespace(s.charAt(i)); blank } do i += 1
              if i - i0 > jN - j0 then
                jN = i
                j0 = i0
              canExtend = blank
          else if i < k then
            jN = j
            canExtend == canExtend && java.lang.Character.isWhitespace(s.charAt(i))
            if !canExtend && jN == j0 then boundary.break()
    dedentBy(if j0 < 0 then "" else s.substring(j0, jN), skipEmptyStart)


  def demargin(indicator: Char = '|', skipMalformedLines: Boolean = false): String =
    boundary[String]:
      var nl0 = s.indexOf('\n')
      var x0 = if nl0 == 0 || (nl0 == 1 && s.charAt(0) == '\r') then nl0 + 1 else 0
      if x0 > 0 then
        nl0 = s.indexOf('\n', x0, s.length)
      if nl0 < 0 then
        boundary.break(if s.indexOf(indicator) == s.length - 1 then "" else s)
      var ic0 = s.indexOf(indicator, x0, nl0)
      if ic0 < 0 then boundary.break(s)
      if ic0 != nl0 - 1 then
        var i = nl0 - 1
        while i > ic0 && java.lang.Character.isWhitespace(s.charAt(i)) do i -= 1
        if i > ic0 && !skipMalformedLines then
          throw new IllegalArgumentException(s"Indicator line has non-whitespace after indicator")
      if ic0 == x0 then boundary.break(s.substring(nl0+1, s.length))
      var iw0 = x0
      while iw0 < ic0 && java.lang.Character.isWhitespace(s.charAt(iw0)) do iw0 += 1
      val sb = new java.lang.StringBuilder(s.length - (nl0 + 1))
      var l = 0
      s.visitLineIndices(nl0+1, s.length): (i0, iN) =>
        l += 1
        var j = iN - 1
        if iN < s.length || (j >= i0 && s.charAt(j) == '\n') then
          if j > i0 && s.charAt(j-1) == '\r' then j -= 1
        else j += 1
        var i = i0
        var x = x0
        while i < j && x < ic0 && s.charAt(i) == s.charAt(x) do
          i += 1
          x += 1
        if x == ic0 || (i == j && iw0 == ic0) || skipMalformedLines then sb.append(s, i, iN): Unit
        else throw new IllegalArgumentException(s"Mismatching prefix on line $l")
      if sb.length > 0 then
        if sb.charAt(sb.length - 1) == '\n' then
          sb.setLength(sb.length - (if sb.length > 1 && sb.charAt(sb.length - 2) == '\r' then 2 else 1))
      sb.toString
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

    inline def use()(inline f: Char => Unit): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        f((sb: StB).charAt(i))
        i += 1
    inline def use(i0: Int, iN: Int)(inline f: Char => Unit): Unit =
      var i = i0
      if i < 0 then i = i0
      var n = (sb: StB).length()
      if iN < n then n = iN
      while i < n do
        f((sb: StB).charAt(i))
        i += 1
    inline def use(ivx: Iv.X)(inline f: Char => Unit): Unit =
      val n = (sb: StB).length()
      use(ivx.index0(n), ivx.indexN(n))(f)
    inline def use(inline rg: Rg)(inline f: Char => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: Char => Unit): Unit =
      var k = 0
      val n = (sb: StB).length()
      while k < indices.length do
        val i = indices(k)
        if i >= 0 && i < n then f((sb: StB).charAt(i))
        k += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: Char => Unit): Unit =
      var k = 0
      val n = (sb: StB).length()
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < n then f((sb: StB).charAt(i))
        k += 1
    inline def use(inline p: Char => Boolean)(inline f: Char => Unit): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        val c = (sb: StB).charAt(i)
        if p(c) then f(c)
        i += 1

    inline def alter()(inline f: Char => Char): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        (sb: StB).setCharAt(i, f((sb: StB).charAt(i)))
        i += 1
    inline def alter(i0: Int, iN: Int)(inline f: Char => Char): Unit =
      var i = i0
      if i < 0 then i = i0
      var n = (sb: StB).length()
      if iN < n then n = iN
      while i < n do
        (sb: StB).setCharAt(i, f((sb: StB).charAt(i)))
        i += 1
    inline def alter(ivx: Iv.X)(inline f: Char => Char): Unit =
      val n = (sb: StB).length()
      alter(ivx.index0(n), ivx.indexN(n))(f)
    inline def alter(inline rg: Rg)(inline f: Char => Char): Unit =
      val iv = Iv of rg
      alter(iv.i0, iv.iN)(f)
    inline def alter(indices: Array[Int])(inline f: Char => Char): Unit =
      var k = 0
      val n = (sb: StB).length()
      while k < indices.length do
        val i = indices(k)
        if i >= 0 && i < n then (sb: StB).setCharAt(i, f((sb: StB).charAt(i)))
        k += 1
    inline def alter(indices: scala.collection.IntStepper)(inline f: Char => Char): Unit =
      var k = 0
      val n = (sb: StB).length()
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < n then (sb: StB).setCharAt(i, f((sb: StB).charAt(i)))
        k += 1
    inline def alter(inline p: Char => Boolean)(inline f: Char => Char): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        val c = (sb: StB).charAt(i)
        if p(c) then (sb: StB).setCharAt(i, f(c))
        i += 1

    inline def visit()(inline f: (Char, Int) => Unit): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        f((sb: StB).charAt(i), i)
        i += 1
    inline def visit(i0: Int, iN: Int)(inline f: (Char, Int) => Unit): Unit =
      var i = i0
      if i < 0 then i = i0
      var n = (sb: StB).length()
      if iN < n then n = iN
      while i < n do
        f((sb: StB).charAt(i), i)
        i += 1
    inline def visit(ivx: Iv.X)(inline f: (Char, Int) => Unit): Unit =
      val n = (sb: StB).length()
      visit(ivx.index0(n), ivx.indexN(n))(f)
    inline def visit(inline rg: Rg)(inline f: (Char, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: (Char, Int) => Unit): Unit =
      var k = 0
      val n = (sb: StB).length()
      while k < indices.length do
        val i = indices(k)
        if i >= 0 && i < n then f((sb: StB).charAt(i), i)
        k += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: (Char, Int) => Unit): Unit =
      var k = 0
      val n = (sb: StB).length()
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < n then f((sb: StB).charAt(i), i)
        k += 1
    inline def visit(inline p: Char => Boolean)(inline f: (Char, Int) => Unit): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        val c = (sb: StB).charAt(i)
        if p(c) then f(c, i)
        i += 1

    inline def edit()(inline f: (Char, Int) => Char): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        (sb: StB).setCharAt(i, f((sb: StB).charAt(i), i))
        i += 1
    inline def edit(i0: Int, iN: Int)(inline f: (Char, Int) => Char): Unit =
      var i = i0
      if i < 0 then i = i0
      var n = (sb: StB).length()
      if iN < n then n = iN
      while i < n do
        (sb: StB).setCharAt(i, f((sb: StB).charAt(i), i))
        i += 1
    inline def edit(ivx: Iv.X)(inline f: (Char, Int) => Char): Unit =
      val n = (sb: StB).length()
      edit(ivx.index0(n), ivx.indexN(n))(f)
    inline def edit(inline rg: Rg)(inline f: (Char, Int) => Char): Unit =
      val iv = Iv of rg
      edit(iv.i0, iv.iN)(f)
    inline def edit(indices: Array[Int])(inline f: (Char, Int) => Char): Unit =
      var k = 0
      val n = (sb: StB).length()
      while k < indices.length do
        val i = indices(k)
        if i >= 0 && i < n then (sb: StB).setCharAt(i, f((sb: StB).charAt(i), i))
        k += 1
    inline def edit(indices: scala.collection.IntStepper)(inline f: (Char, Int) => Char): Unit =
      var k = 0
      val n = (sb: StB).length()
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < n then (sb: StB).setCharAt(i, f((sb: StB).charAt(i), i))
        k += 1
    inline def edit(inline p: Char => Boolean)(inline f: (Char, Int) => Char): Unit =
      var i = 0
      val n = (sb: StB).length()
      while i < n do
        val c = (sb: StB).charAt(i)
        if p(c) then (sb: StB).setCharAt(i, f(c, i))
        i += 1

    inline def visitLineIndices(inline f: (Int, Int) => Unit): Unit =
      var i = 0
      var j = -1
      val n = (sb: StB).length()
      while i < n do
        j = i + 1
        while j < n && (sb: StB).charAt(j) != '\n' do j += 1
        val k = if j < n then j + 1 else n
        if j < n then j = j + 1
        f(i, j)
        i = k
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
