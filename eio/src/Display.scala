// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC

package kse.eio

import java.lang.{StringBuilder => StB}

import kse.flow._
import kse.maths._
import kse.maths.packed._

trait Dysplay[-A] {
  protected def nesting: Boolean = false
  protected def limitFrom(limit: Int, ctx: Dysplay.Context) =
    if limit > 0 then limit min (ctx.width max 1) else ctx.width max 1
  def append(target: StB, a: A, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info
  def dysplay(a: A)(using ctx: Dysplay.Context): String =
    val stb = new StB
    append(stb, a, ctx.width)
    stb.toString
}
object Dysplay {
  opaque type Info = Unit
  object Info {
    def default: Info = ()
    def wrap(u: Unit): Info = u
    extension (info: Info)
      def unwrap: Unit = info
  }

  trait Context {
    def tabular: Boolean = false
    def humanized: Boolean = false
    def hashover: Boolean = false
    def width: Int
    def sub(count: Int): Context = this
  }
  object Context {
    /*
    final case class Static(tabular: Boolean, width: Int) extends Context {}
    final case class Proxy(underlying: Context, tab: Boolean Or Unit = Alt.unit, wid: Int Or Unit = Alt.unit)
    extends Context {
      def tabular = tab.getOrElse(_ => underlying.tabular)
      def width = wid.getOrElse(_ => underlying.width)
    }
    */
    given standard: Context with
      def width = 65535
  }

  private val powersOfTen = Array(
    1L,                 10L,                     100L,                     1_000L,
                    10_000L,                 100_000L,                 1_000_000L,
                10_000_000L,             100_000_000L,             1_000_000_000L,
            10_000_000_000L,         100_000_000_000L,         1_000_000_000_000L,
        10_000_000_000_000L,     100_000_000_000_000L,     1_000_000_000_000_000L,
    10_000_000_000_000_000L, 100_000_000_000_000_000L, 1_000_000_000_000_000_000L)

  private val lotsOfHashes = Array.fill(300)('#')

  private def needsSpecialHandling(c: Char): Int = 
    import java.lang.Character._
    getType(c) match
      case UPPERCASE_LETTER |
           LOWERCASE_LETTER |
           TITLECASE_LETTER |
           MODIFIER_LETTER  |
           OTHER_LETTER     |
           DECIMAL_DIGIT_NUMBER |
           LETTER_NUMBER        |
           OTHER_NUMBER         |
           CONNECTOR_PUNCTUATION |
           DASH_PUNCTUATION      |
           START_PUNCTUATION     |
           END_PUNCTUATION       |
           MATH_SYMBOL     |
           CURRENCY_SYMBOL |
           MODIFIER_SYMBOL |
           OTHER_SYMBOL
        => 0
      case INITIAL_QUOTE_PUNCTUATION |
           FINAL_QUOTE_PUNCTUATION   |
           OTHER_PUNCTUATION
        => if c == '\'' then 1 else if c == '"' then 2 else 3
      case SPACE_SEPARATOR => 3
      case _ => -1

  private val lowEscapeStrings =
    Array.tabulate(32)(i => s"\\x${UByte.wrap(i.toByte).hexString}").fn{ a =>
      a('\n') = "\n"
      a('\r') = "\r"
      a('\t') = "\t"

    }

  given Dysplay[Char] with
    def append(target: StB, a: Char, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      needsSpecialHandling(a) match
        case 0 =>
          target append a
        case x if x > 0 =>
          val space = limitFrom(limit, ctx)
          if space < 3 then target append '\u2026'
          else if space > 3 && a == '\'' then target append "'\\''"
          else
            target append '\''
            target append a
            target append '\''
        case _ =>
          val space = limitFrom(limit, ctx)
          if space < 3 then target append '\u2026'
          else if space == 3 then target append "'\u2026\'"
          else if a < ' ' then
            if a == '\n' then target append "'\\n'"
            else if a == '\r' then target append "'\\r'"
            else if a == '\t' then target append "'\\t'"
            else if a == '\b' then target append "'\\b'"
            else if a == '\f' then target append "'\\f'"
            else
              target append '\''
              target append '^'
              target append (a + '@').toChar
              target append '\''
          else
            if space < 8 then target append "'\\u\u2026'"
            else
              target append "'\\u"
              target append a.hexString
              target append '\''
      Info.default

  given Dysplay[Boolean] with
    def append(target: StB, a: Boolean, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      val space = limitFrom(limit, ctx)
      if space > 4 || (space == 4 && !ctx.tabular) then target append a
      else target append (if a then 'T' else 'F')
      Info.default

  given Dysplay[ULong] with
    def append(target: StB, a: ULong, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      var space = limitFrom(limit, ctx)
      val l = target.length
      target append java.lang.Long.toUnsignedString(a.unwrap)
      if target.length - l > space then
        if ctx.hashover then
          target.setLength(l)
          while space > 0 do
            val n = space min 300
            target.append(lotsOfHashes, 0, n)
            space -= n
      Info.default

  given Dysplay[UByte] with
    def append(target: StB, a: UByte, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      summon[Dysplay[ULong]].append(target, a.toULong, limit)

  given Dysplay[Long] with
    def append(target: StB, a: Long, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      var space = limitFrom(limit, ctx)
      val l = target.length
      if ctx.tabular && a >= 0 then target append ' '
      target append a
      if target.length - l > space then
        if ctx.hashover then
          target.setLength(l)
          while space > 0 do
            val n = space min 300
            target.append(lotsOfHashes, 0, n)
            space -= n
      Info.default

  given Dysplay[Byte] with
    def append(target: StB, a: Byte, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      summon[Dysplay[Long]].append(target, a.toLong, limit)

  given Dysplay[Short] with
    def append(target: StB, a: Short, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      summon[Dysplay[Long]].append(target, a.toLong, limit)

  given Dysplay[Int] with
    def append(target: StB, a: Int, limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      summon[Dysplay[Long]].append(target, a.toLong, limit)

  given [A](using db: Dysplay[A]): Dysplay[Array[A]] with
    protected override def nesting = true
    def append(target: StB, a: Array[A], limit: Int)(using ctx: Dysplay.Context): Dysplay.Info =
      val l = target.length
      val m = limitFrom(limit, ctx) +# l
      target append '['
      target append a.length
      target append ':'
      var spaci = target.length
      if spaci > m-2 then
        if m-l <= 2 then
          target.setLength(l)
          if m-l == 2 && a.isEmpty then target append "[]"
          else target append '\u2026'
        else target append "[\u2026]"
      else
        attempt:
          given Context = ctx.sub(a.length)
          aFor(a){ (b, _) =>
            target append ' '
            ensure(target.length < m-1)
            spaci = target.length
            db.append(target, b, if db.nesting then limit - (spaci - l) - 3 max 1 else limit)
            ensure(target.length <= m-1)
          }
          target append ']'
        .default:
          if m-4 > spaci then
            target.setLength(m-4)
            target append "\u2026 \u2026]"
          else
            target.setLength(spaci)
            target append "\u2026]"
      Info.default
}


/*
trait Displey[-A] {
  def coordinate(coord: Displey.Coordinator Or Unit): Displey.Coordinator Or Unit = coord
  def append(target: StB, a: A, limit: Int, anchor: Int, coord: Displey.Coordinator Or Unit): Displey.Info
  def displey(a: A): String = (new StB).tap(stb => append(stb, a, Int.MaxValue, -1, Alt.unit)).toString
}
object Displey {
  opaque type Info = Long
  object Info {
    inline def default: kse.eio.Displey.Info = wrap(0L)
    inline def wrap(l: Long): Info = l

    extension (info: Info)
      inline def unwrap: Long = info
  }

  trait Coordinator {
    def seek[A](pf: PartialFunction[Coordinator, A]): A Or Unit = pf.applyOr(this)
  }
  object Coordinator {
    final class Mixture(val accounts: List[Coordinator]) extends Coordinator {
      inline def ::(that: Coordinator) = Mixture(that :: accounts)
      inline def :+(that: Coordinator) = Mixture(accounts :+ that)
      inline def +:(that: Coordinator) = Mixture(that +: accounts)

      override def seek[A](pf: PartialFunction[Coordinator, A]): A Or Unit =
        Or.FlatRet:
          iFor(accounts.iterator){ (a, _) => a.seek(pf).breakOnIs }
          Alt.unit
    }
  }

  given Displey[Boolean] with
    def append(target: StB, a: Boolean, limit: Int, anchor: Int, account: Coordinator Or Unit): Info =
      target append (if limit >= 5 then a else if a then "T" else "F")
      Info.default

  given Displey[String] with
    def append(target: StB, a: String, limit: Int, anchor: Int, account: Coordinator Or Unit): Info =
      val l = target.length
      var m = l +# (limit max 0)
      var i = 0
      while i < a.length do
      if a.length <= limit then target append a
      else
        if limit > 1 then target.append(a, 0, limit - 1)
        target append '\u2026'
      Info.default
}

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


extension [A](a: A)(using disp: Display[A]) {
  inline def displayFmt(target: StB, opts: Display.Opts): Int = disp.displayFmt(Is(opts))(target, a)
  inline def displayFmt(opts: Display.Opts): String = disp.displayFmt(Is(opts))(a)
  inline def display(target: StB): Int = disp.display(target, a)
  inline def display: String = disp.display(a)
}

extension [A](a: A)(using disp: Displey[A]) {
  inline def displey(lim: Int): String = (new StB).tap(stb => disp.append(stb, a, lim, -1, Alt.unit)).toString
}
*/

extension [A](a: A)(using disp: Dysplay[A], ctx: Dysplay.Context) {
  inline def dysplay: String = disp.dysplay(a)(using ctx)
}

/*
abstract class Displayable extends (StB => Unit) {}
object Displayable {
  given displayer[A](using disp: Display[A]): Conversion[A, Displayable] with
    def apply(a: A): Displayable = (stb: StB) => { disp.display(stb, a); () }
}

extension (sc: StringContext)
  def disp(items: Displayable*): String =
    val ts = sc.parts.iterator
    val is = items.iterator
    val stb = new StB()
    if ts.hasNext then stb append ts.next
    while is.hasNext do
      is.next.apply(stb)
      stb append ts.next
    stb.toString
*/