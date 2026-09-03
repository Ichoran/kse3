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
            canExtend = canExtend && java.lang.Character.isWhitespace(s.charAt(i))
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
    inline def add[R <: Iv.X | Rg](xs: RangeAddable, inline r: R): Unit = inline xs match
      case ac: Array[Char] =>
        Iv.dispatch(r, ac)((i0, iN) => (sb: StB).append(ac, i0, iN - i0): Unit)
      case cs: CharSequence =>
        val n = cs.length()
        Iv.dispatch(r, n)((i0, iN) => (sb: StB).append(cs, i0, iN): Unit)

    inline def addln(): Unit =
      (sb: StB).append('\n'): Unit
    inline infix def addln(x: Addable): Unit =
      MkStr.+=(sb)(x)
      (sb: StB).append('\n'): Unit
    inline def addln(xs: RangeAddable, i0: Int, iN: Int): Unit =
      MkStr.add(sb)(xs, i0, iN)
      (sb: StB).append('\n'): Unit
    inline def addln[R <: Iv.X | Rg](xs: RangeAddable, inline r: R): Unit =
      MkStr.add(sb)(xs, r)
      (sb: StB).append('\n'): Unit

    inline def capacity: Int = (sb: StB).capacity()
    inline def capacity_=(i: Int): Unit = (sb: StB).ensureCapacity(i)
    inline def trimCapacity(): Unit = (sb: StB).trimToSize()

    inline def codept(i: Int): CodePoint = CodePoint((sb: StB).codePointAt(i))

    inline def codeCount(i0: Int, iN: Int): Int = (sb: StB).codePointCount(i0, iN)
    inline def codeCount[R <: Iv.X | Rg](inline r: R): Int =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => (sb: StB).codePointCount(i0, iN))

    inline def del(target: Iv.Pt): Unit = (sb: StB).deleteCharAt(Iv.point(target, (sb: StB).length())): Unit
    inline def del(i0: Int, iN: Int): Unit = (sb: StB).delete(i0, iN): Unit
    inline def del[R <: Iv.X | Rg](inline r: R): Unit =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => (sb: StB).delete(i0, iN): Unit)

    inline def getChars(i0: Int, iN: Int, target: Array[Char], where: Iv.Pt): Unit = (sb: StB).getChars(i0, iN, target, Iv.point(where, target))
    inline def getChars[R <: Iv.X | Rg](inline r: R, target: Array[Char], where: Iv.Pt): Unit =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => (sb: StB).getChars(i0, iN, target, Iv.point(where, target)))

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
    inline def ins[R <: Iv.X | Rg](target: Iv.Pt, ra: RangeAddable, inline r: R): Unit =
      val n = (sb: StB).length()
      val j = Iv.point(target, n)
      inline ra match
        case ac: Array[Char]  => Iv.dispatch(r, n)((i0, iN) => (sb: StB).insert(j, ac, i0, iN - i0): Unit)
        case cs: CharSequence => Iv.dispatch(r, n)((i0, iN) => (sb: StB).insert(j, cs, i0, iN): Unit)

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
    inline def str[R <: Iv.X | Rg](inline r: R): String =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => (sb: StB).substring(i0, iN))

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
    inline def use[R <: Iv.X | Rg](inline r: R)(inline f: Char => Unit): Unit =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => use(i0, iN)(f))
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
        val i = indices.nextStep()
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
    inline def alter[R <: Iv.X | Rg](inline r: R)(inline f: Char => Char): Unit =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => alter(i0, iN)(f))
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
        val i = indices.nextStep()
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
    inline def visit[R <: Iv.X | Rg](inline r: R)(inline f: (Char, Int) => Unit): Unit =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => visit(i0, iN)(f))
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
        val i = indices.nextStep()
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
    inline def edit[R <: Iv.X | Rg](inline r: R)(inline f: (Char, Int) => Char): Unit =
      val n = (sb: StB).length()
      Iv.dispatch(r, n)((i0, iN) => edit(i0, iN)(f))
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
        val i = indices.nextStep()
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


//////////////////////////////////////////////////
/// Sayable typeclass and the say interpolator ///
//////////////////////////////////////////////////


/** Namespace for customization of `Sayable` rendering. */
object Say {
  /** Customization hooks consulted by `Sayable` instances.
    *
    * The `say` interpolator (and `.say()` extension) summon the `Style` in scope at each
    * use site and hand it to every instance, so output can be customized locally by
    * providing a given that overrides whichever hooks are relevant.  More hooks are
    * expected to appear here as needed; instances unaware of a hook simply ignore it.
    */
  class Style() {
    /** Separator between successive elements of an array or collection. */
    def sep: String = ", "

    /** Text that opens an array or collection. */
    def open: String = "["

    /** Text that closes an array or collection. */
    def close: String = "]"
  }
  object Style {
    given default: Style = new Style()
  }
}


/** Typeclass to render a value of type `A` into a `MkStr`; powers the `say` interpolator.
  *
  * Unlike `toString`-based interpolation, the instance is chosen at compile time from the
  * static type of each argument, so arrays and opaque types can print sensibly.  The
  * `Say.Style` in scope at the use site is passed to every instance as a customization hook.
  */
trait Sayable[A] {
  def say(a: A, m: MkStr, style: Say.Style): Unit
}

trait SayableLowPriority {
  /** Anything without a more specific instance renders as `String.valueOf` would. */
  given sayAnything: [A] => Sayable[A] = (a, m, _) => m += String.valueOf(a.asInstanceOf[AnyRef])
}

trait SayableGenericPriority extends SayableLowPriority {
  /** Arrays render element by element from the element's instance, e.g. `[1, 2, 3]`. */
  given sayArray: [A] => (sy: Sayable[A]) => Sayable[Array[A]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      sy.say(xs(i), m, st)
      i += 1
    m += st.close
}

object Sayable extends SayableGenericPriority {
  // JDK floating-point text uses 'E'; house style is lowercase, so fix up appended text
  private def lowerE(sb: StB, n0: Int): Unit =
    var i = sb.length - 1
    while i > n0 && sb.charAt(i) != 'E' do i -= 1
    if i > n0 then sb.setCharAt(i, 'e')

  given sayBoolean: Sayable[Boolean] = (b, m, _) => m += b
  given sayByte:    Sayable[Byte]    = (b, m, _) => m += b.toInt
  given sayShort:   Sayable[Short]   = (s, m, _) => m += s.toInt
  given sayChar:    Sayable[Char]    = (c, m, _) => m += c
  given sayInt:     Sayable[Int]     = (i, m, _) => m += i
  given sayLong:    Sayable[Long]    = (l, m, _) => m += l
  given sayString:  Sayable[String]  = (s, m, _) => m += s

  // NOT `m += c`: within this file CodePoint is transparently Int, so the Addable
  // inline match would take the Int branch and print digits instead of the character
  given sayCodePoint: Sayable[CodePoint] = (c, m, _) => m.unwrap.appendCodePoint(c.value): Unit

  given sayFloat: Sayable[Float] = (f, m, _) =>
    val sb = m.unwrap
    val n = sb.length
    sb.append(f): Unit
    lowerE(sb, n)

  given sayDouble: Sayable[Double] = (d, m, _) =>
    val sb = m.unwrap
    val n = sb.length
    sb.append(d): Unit
    lowerE(sb, n)

  given sayArrayBoolean: Sayable[Array[Boolean]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      m += xs(i)
      i += 1
    m += st.close

  given sayArrayByte: Sayable[Array[Byte]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      m += xs(i).toInt
      i += 1
    m += st.close

  given sayArrayShort: Sayable[Array[Short]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      m += xs(i).toInt
      i += 1
    m += st.close

  given sayArrayChar: Sayable[Array[Char]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      m += xs(i)
      i += 1
    m += st.close

  given sayArrayInt: Sayable[Array[Int]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      m += xs(i)
      i += 1
    m += st.close

  given sayArrayLong: Sayable[Array[Long]] = (xs, m, st) =>
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      m += xs(i)
      i += 1
    m += st.close

  given sayArrayFloat: Sayable[Array[Float]] = (xs, m, st) =>
    val sb = m.unwrap
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      val n = sb.length
      sb.append(xs(i)): Unit
      lowerE(sb, n)
      i += 1
    m += st.close

  given sayArrayDouble: Sayable[Array[Double]] = (xs, m, st) =>
    val sb = m.unwrap
    m += st.open
    var i = 0
    while i < xs.length do
      if i > 0 then m += st.sep
      val n = sb.length
      sb.append(xs(i)): Unit
      lowerE(sb, n)
      i += 1
    m += st.close
}


/** Typeclass to decide whether a count of type `N` is plural; powers the `say` interpolator's
  * `#prefix/singular/plural#` and `#singular/plural/suffix<#` pluralization forms.
  *
  * The English convention is followed: exactly one is singular, everything else (including
  * zero and negative counts) is plural.  Instances for the standard integer types are
  * provided here; other numeric types can supply their own instance.
  */
trait PluralizeBy[N] {
  def isPlural(n: N): Boolean
}
object PluralizeBy {
  given byByte:   PluralizeBy[Byte]   = b => b != 1
  given byShort:  PluralizeBy[Short]  = s => s != 1
  given byInt:    PluralizeBy[Int]    = i => i != 1
  given byLong:   PluralizeBy[Long]   = l => l != 1L
  given byBigInt: PluralizeBy[BigInt] = b => b != BigInt(1)
}


/** Typeclass to render a value of type `A` in spoken words; powers the `say` interpolator's
  * `spoken`/`Spoken` wrappers, which print e.g. `2` as `two`.  Instances should produce
  * entirely lowercase text (`Spoken` capitalizes the first character itself); kse.maths
  * provides instances for the standard integer types.
  */
trait Speakable[A] {
  def speak(a: A, m: MkStr, style: Say.Style): Unit
}


/** A pluralization count that renders as nothing: `silently(n)` drives singular/plural
  * choices in the `say` interpolator exactly as `n` would, but prints no number, so
  * `say"#I am/We are/<#${silently(n)} doing great!"` mentions no count.
  */
opaque type Silently = Boolean
object Silently {
  inline def wrap(plural: Boolean): Silently = plural
  extension (s: Silently)
    inline def unwrap: Boolean = s
  given pluralizeSilently: PluralizeBy[Silently] = s => s
  given saySilently: Sayable[Silently] = (_, _, _) => ()
}

/** Wraps a count so that it pluralizes like `a` but prints nothing in `say` interpolation. */
inline def silently[A](a: A)(using pb: PluralizeBy[A]): Silently = Silently.wrap(pb.isPlural(a))


/** A count that prints in spoken words via its `Speakable` instance but pluralizes like the
  * underlying number, so `say"There #is/are/ <#${spoken(n)}"` can give `There are two`.
  * `spoken(n)` is lowercase; `Spoken(n)` capitalizes the first letter for sentence starts.
  */
opaque type Spoken[A] = (A, Boolean)
object Spoken {
  /** A spoken form with its first letter capitalized, e.g. to start a sentence. */
  inline def apply[A](a: A)(using Speakable[A]): Spoken[A] = (a, true)
  inline def wrap[A](a: A, capitalized: Boolean): Spoken[A] = (a, capitalized)
  extension [A](s: Spoken[A])
    inline def value: A = s._1
    inline def capitalized: Boolean = s._2
  given pluralizeSpoken: [A] => (pb: PluralizeBy[A]) => PluralizeBy[Spoken[A]] = s => pb.isPlural(s._1)
  given saySpoken: [A] => (sp: Speakable[A]) => Sayable[Spoken[A]] = (s, m, st) =>
    val sb = m.unwrap
    val n0 = sb.length
    sp.speak(s._1, m, st)
    if s._2 && sb.length > n0 then sb.setCharAt(n0, java.lang.Character.toUpperCase(sb.charAt(n0)))
}

/** Wraps a number to print as lowercase spoken words in `say` interpolation. */
inline def spoken[A](a: A)(using Speakable[A]): Spoken[A] = Spoken.wrap(a, false)


extension [A](a: A)
  /** Renders this value to a `String` via its `Sayable` instance and the ambient `Say.Style`. */
  inline def say()(using sy: Sayable[A], st: Say.Style): String =
    MkStr: m =>
      sy.say(a, m, st)

  /** Appends this value to a `MkStr` via its `Sayable` instance and the ambient `Say.Style`. */
  inline def sayInto(m: MkStr)(using sy: Sayable[A], st: Say.Style): Unit =
    sy.say(a, m, st)


extension (inline sc: StringContext)
  /** Say-interpolator: like `s"..."`, but each argument is rendered by the `Sayable`
    * instance for its static type into a single `MkStr`, so arrays and opaque types
    * print sensibly.  Customize output by providing a `Say.Style` given at the use site.
    *
    * If the text immediately after an argument starts with `#`, it is a pluralization
    * directive `#prefix/singular/plural#`: the prefix plus the singular or plural form is
    * appended, chosen by the argument's `PluralizeBy` instance (all standard integer types
    * including `BigInt` have one; it is a compile error if the argument's type does not).
    * For example, `say"You have $n# tr/y/ies# left"` gives `You have 1 try left` or
    * `You have 2 tries left`.  Text immediately before an argument may likewise end with
    * `#singular/plural/suffix<#`, chosen by the argument that follows, so
    * `say"There #is/are/ <#$n# egg//s#"` gives `There is 1 egg` or `There are 15 eggs`.
    * The degenerate forms `#//#` and `#//<#` emit a literal `#` and `<#` respectively; a
    * `#` anywhere else is always literal (a part must actually end with `<#` to close the
    * before-argument form, so a bare trailing `#` is safe).  One part may carry directives
    * at both ends: the closing `...<#` form is recognized first and the opening form is
    * parsed from what remains.  The fields themselves cannot contain `/` or `#`.
    * `silently(n)` drives such choices while printing nothing, and `spoken(n)` / `Spoken(n)`
    * print the count in lowercase / Capitalized words via its `Speakable` instance
    * (kse.maths provides these for the standard integer types).
    *
    * The result is assembled by straight-line code generated at compile time: no varargs
    * `Seq`, no wrapper objects, and typeclass instances are resolved statically.
    */
  inline def say(inline args: Any*): String =
    ${ basicsMacroImpl.sayInterpolationExpr('sc, 'args) }
