// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC

package kse.eio

import java.lang.{StringBuilder => StB}

import kse.flow._
import kse.maths._
import kse.maths.packed._


trait Display[-A] {
  def displayFmt(opts: Display.Opts Or Unit)(target: StB, a: A): Int
  def displayFmt(opts: Display.Opts Or Unit)(a: A): String = (new StB).tap(sb => displayFmt(opts)(sb, a)).toString
  def display(target: StB, a: A): Int = displayFmt(Alt.unit)(target, a)
  def display(a: A): String = displayFmt(Alt.unit)(a)
}
object Display {
  opaque type Flags = Int
  object Flags {
    def apply(i: Int): Flags = i
    extension (f: Flags)
      def unwrap: Int = f
    extension (f: kse.eio.Display.Flags) {
      def &(g: kse.eio.Display.Flags): kse.eio.Display.Flags = apply(unwrap(f) | unwrap(g))
      def hasAny(g: kse.eio.Display.Flags): Boolean = (unwrap(f) & unwrap(g)) != 0
      def has(g: kse.eio.Display.Flags): Boolean = (unwrap(f) & unwrap(g)) == unwrap(g)
      def mask(g: kse.eio.Display.Flags): kse.eio.Display.Flags = (unwrap(f) & unwrap(g))
      def isEmpty: Boolean = unwrap(f) == 0
    }
  }
  inline def PadLeft: kse.eio.Display.Flags    = Flags( 0x1)
  inline def PadRight: kse.eio.Display.Flags   = Flags( 0x2)
  inline def Pad: kse.eio.Display.Flags        = Flags( 0x3)
  inline def StrictSize: kse.eio.Display.Flags = Flags( 0x4)
  inline def OneLine: kse.eio.Display.Flags    = Flags( 0x8)
  inline def ShowSign: kse.eio.Display.Flags   = Flags(0x10)
  inline def SixSig: kse.eio.Display.Flags     = Flags(0x20)  // If set alone, use 6 sig figs
  inline def ClipSig: kse.eio.Display.Flags    = Flags(0x40)  // If set alone, use 12 sig figs; with SixSig, cut to 3
  inline def SigFigs: kse.eio.Display.Flags    = Flags(0x60)

  case class Opts(maxSize: Int = 0, anchor: Int = 0, margin: Int = Int.MaxValue, indent: String = "", flags: Flags = Flags(0)) {}
  object Opts {
    val default = Opts()

    def padded(maxSize: Int, anchor: Int = 0, margin: Int = Int.MaxValue, indent: String = ""): Opts =
      new Opts(maxSize, anchor, margin, indent, Pad)
    def strict(maxSize: Int = 0, anchor: Int = 0, margin: Int = Int.MaxValue, indent: String = ""): Opts =
      new Opts(maxSize, anchor, margin, indent, StrictSize)
    def strictpad(maxSize: Int, anchor: Int = 0, margin: Int = Int.MaxValue, indent: String = ""): Opts =
      new Opts(maxSize, anchor, margin, indent, Pad & StrictSize)
    def padleft(size: Int, margin: Int = Int.MaxValue, indent: String = ""): Opts =
      new Opts(size, size, margin, indent, PadLeft)
    def padright(size: Int, margin: Int = Int.MaxValue, indent: String = ""): Opts =
      new Opts(size, 0, margin, indent, PadRight)
  }

  val fixedPadding = Array.fill(200)(' ')
  val lotsOfHashes = Array.fill(21)('#')

  def addPadding(n: Int)(target: StB, index: Int = Int.MaxValue): Unit =
    if n > 0 then
      if index < target.length then
        var i = index
        var m = n
        if i < 0 then
          m += i
          i = 0
        if m > 0 then
          if m <= fixedPadding.length then target.insert(i, fixedPadding, 0, m)
          else
            val cs = new Array[Char](m)
            cs.fill(' ')
            target.insert(i, cs)
      else
        if n < fixedPadding.length then target.append(fixedPadding, 0, n)
        else
          var m = n
          while m > fixedPadding.length do
            target append fixedPadding
            m -= fixedPadding.length
          target.append(fixedPadding, 0, m)

  def booleanFmt(target: StB, maxSize: Int, anchor: Int, flags: Flags = Flags(0))(value: Boolean): Int =
    import Flags._
    val n = if value then 4 else 5
    val l = target.length
    if maxSize > 0 && n > maxSize then target append (if value then 'T' else 'F')
    else target append value
    val a = target.length - l
    val anch = if maxSize > 0 then anchor min maxSize else anchor
    if a < anch && flags.has(PadLeft) then addPadding(anch - a)(target, l)
    val b = target.length - l
    if maxSize > 0 && b < maxSize && flags.has(PadRight) then addPadding(maxSize - b)(target)
    b

  def numberFmt(target: StB, maxSize: Int, anchor: Int, unsigned: Boolean = false, flags: Flags = Flags(0))(number: Long): Int =
    import Flags._
    val l = target.length
    if unsigned then
      if number >= 0 then target append number
      else target append java.lang.Long.toUnsignedString(number)
    else
      if number > 0 && flags.has(ShowSign) then target append '+'
      target append number
    val a = target.length - l
    val anch = if maxSize > 0 then anchor min maxSize else anchor
    if a < anch && flags.has(PadLeft) then addPadding(anch - a)(target, l)
    val b = target.length - l
    if b < maxSize && flags.has(PadRight) then addPadding(maxSize - b)(target)
    if flags.has(StrictSize) && target.length - l > maxSize && maxSize > 0 then
      target.setLength(l)
      if maxSize == 1 then target append (if number > 0 || unsigned then '+' else if number < 0 then '-' else '0')
      else if maxSize == 2 then
        target append (if number < 0 && !unsigned then "-#" else if flags.has(ShowSign) && !unsigned then "+#" else "##")
      else
        if number < 0 && !unsigned then target append "-"
        else if flags.has(ShowSign) && !unsigned then target append "+"
        target.append(lotsOfHashes, 0, maxSize - (target.length - l))
      maxSize
    else
      b

  private def decimalCleanup(target: StB, zero: Int)(maxSize: Int, anchor: Int, flags: Flags = Flags(0)): Int = ???
    /*
    var dotn = zero + 1
    while dotn < target.length && target.charAt(dotn) != '.' do dotn += 1
    var expn = dotn
    while expn < target.length && target.charAt(expn) != 'E' do expn += 1
    val pow =
      if expn < target.length then
        target.setCharAt(expn, 'e')
        var i = expn + 1
        val neg = target.charAt(i) match
          case '-' => i += 1; true
          case '+' => i += 1; false
          case _   => false
        var value = 0
        while i < target.length do
          value = 10*value + (target.charAt(i) - '0')
          i += 1
        if neg then -value else value
      else 0
    val figs = (expn - zero) - (if dotn < expn then 1 else 0)
    val anch = if dotn < target.length then dotn + pow else expn + pow
    */

  given Display[Boolean] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Boolean): Int =
      opts.fold{
        o => booleanFmt(target, o.maxSize, o.anchor, o.flags)(a)
      }{
        _ => val l = target.length; target append a; target.length - l
      }

  given Display[Char] with
    def displayFmt(opts: Opts Or Unit)(target: StB, c: Char): Int =
      import Flags._
      opts.fold{
        o =>
          if o.maxSize > 1 then
            val l = target.length
            if o.anchor > 1 && o.flags.has(PadLeft) then addPadding((o.anchor min o.maxSize) - 1)(target)
            target append c
            val b = target.length - l
            if b < o.maxSize && o.flags.has(PadRight) then addPadding(o.maxSize - b)(target)
            b
          else
            target append c; 1
      }{
        _ => target append c; 1
      }

  given Display[Byte] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Byte): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, false, o.flags)(a.toLong)
      }{
        _ => val l = target.length; target append a; target.length - l
      }

  given Display[UByte] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: UByte): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, true, o.flags)(a.toLong)
      }{
        _ => val l = target.length; target append a.toInt; target.length - l
      }

  given Display[Short] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Short): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, false, o.flags)(a.toLong)
      }{
        _ => val l = target.length; target append a; target.length - l
      }

  given Display[Int] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Int): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, false, o.flags)(a.toLong)
      }{
        _ => val l = target.length; target append a; target.length - l
      }

  given Display[UInt] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: UInt): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, true, o.flags)(a.toLong)
      }{
        _ => val l = target.length; target append a.toLong; target.length - l
      }

  given Display[Long] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Long): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, false, o.flags)(a)
      }{
        _ => val l = target.length; target append a; target.length - l
      }

  given Display[ULong] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: ULong): Int =
      opts.fold{
        o => numberFmt(target, o.maxSize, o.anchor, true, o.flags)(a.signed)
      }{
        _ =>
          val l = target.length
          if a.signed < 0 then target append java.lang.Long.toUnsignedString(a.signed)
          else target append a.signed
          target.length - l
      }

  given Display[Float] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Float): Int =
      val l = target.length
      target append a
      opts.fold{
        o => decimalCleanup(target, l)(o.maxSize, o.anchor, o.flags)
      }{
        _ => decimalCleanup(target, l)(0, 0)
      }

  given Display[Double] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: Double): Int =
      val l = target.length
      target append a
      opts.fold{
        o => decimalCleanup(target, l)(o.maxSize, o.anchor, o.flags)
      }{
        _ => decimalCleanup(target, l)(0, 0)
      }

  given Display[String] with
    def displayFmt(opts: Opts Or Unit)(target: StB, a: String): Int =
      import Flags._
      opts.fold{
        o =>
          if o.maxSize <= 0 then
            target append a
            0
          else
            if a.length >= o.maxSize then
              if o.flags.has(StrictSize) && a.length > o.maxSize then
                if o.maxSize <= 3 then target.append(a, 0, o.maxSize)
                if o.anchor >= o.maxSize then
                  val n = (o.maxSize - 3) max 3
                  target.append("...", 0, (o.maxSize - 3) min 3)
                  target.append(a, a.length - n, a.length)
                else
                  target.append(a, 0, (o.maxSize - 3) max 3)
                  target.append("...", 0, (o.maxSize - 3) min 3)
              else target append a
              if o.anchor >= o.maxSize then o.maxSize else 0
            else if o.anchor <= 0 then
              target append a
              if o.flags.has(PadRight) then addPadding(o.maxSize - a.length)(target)
              0
            else if o.anchor >= o.maxSize then
              if o.flags.has(PadLeft) then addPadding(o.maxSize - a.length)(target)
              target append a
              if o.flags.has(PadLeft) then o.maxSize else a.length
            else
              var r = o.anchor + a.length/2
              var l = r - a.length
              if l < 0 then
                r -= l
                l = 0
              else if r > o.maxSize then
                l -= r - o.maxSize
                r = o.maxSize
              val i = target.length
              var n = 0
              if l > 0 && o.flags.has(PadLeft) then
                addPadding(l)(target)
                n += l
              target append a
              n += a.length / 2
              val b = target.length - i
              if b < o.maxSize && o.flags.has(PadRight) then addPadding(o.maxSize - b)(target)
              n
      }{
        _ => target append a; 0
      }


  /*
  given Display[Float] with
    def displayFmtOn(target: StB, maxSize: Int, anchor: Int, pad: Boolean, margin: Int, indent: String)(a: Float): Int =
      target append a
      0

  given displayPlusMinus(using dsp: Display[Float]): Display[PlusMinus] with
    def displayFmtOn(target: StB, maxSize: Int, anchor: Int, pad: Boolean, margin: Int, indent: String)(a: PlusMinus): Int =
      target append a.value
      target append " +- "
      target append a.error
      0

  given Display[Frac] with
    def displayFmtOn(target: StB, maxSize: Int, anchor: Int, pad: Boolean, margin: Int, indent: String)(a: Frac): Int =
      target append a.numer
      target append " over "
      target append a.denom
      0

  given displayArray[A](using dsp: Display[A]): Display[Array[A]] with
    override def displayOn(target: StB)(a: Array[A]): Int =
      target append "[["
      target append a.length
      target append "]: "
      aFor(a){ (ai, i) =>
        if i > 0 then target append ", "
        dsp.displayOn(target)(ai)
      }
      target append "]"
      0
    def displayFmtOn(target: StB, maxSize: Int, anchor: Int, pad: Boolean, margin: Int, indent: String)(a: Array[A]): Int =
      displayOn(target)(a)

  /*
  given displayIterable[A](using dsp: Display[A]): Display[Iterable[A]] with
    override def displayOn(target: StB)(a: Iterable[A])
  */

  given Display[AnyRef] with
    def displayFmtOn(target: StB, preferredSize: Int, anchorPoint: Int, pad: Boolean, margin: Int, indent: String)(a: AnyRef): Int =
      target append a.toString
      0
  */
}


extension [A](a: A)(using Display[A]) {
  inline def displayFmt(target: StB, opts: Display.Opts): Int = summon[Display[A]].displayFmt(Is(opts))(target, a)
  inline def displayFmt(opts: Display.Opts): String = summon[Display[A]].displayFmt(Is(opts))(a)
  inline def display(target: StB): Int = summon[Display[A]].display(target, a)
  inline def display: String = summon[Display[A]].display(a)
}

