// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC

package kse.eio

import java.lang.{StringBuilder => StB}

import scala.util.{Try, Success, Failure, boundary}

import kse.flow._
import kse.maths._
import kse.maths.packed._

/** Provides custom serialization to a StringBuilder. */
trait Display[-A] {
  /** True if it has displayable subcomponents */
  protected def nesting: Boolean = false    

  /** Implementation details for how to append things and report on alignment points.
    */
  protected def appendImpl(target: StB, a: A, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info

  /** Place the displayable a into a target `java.lang.StringBuilder`.
    * 
    * If limit is positive, it is the maximum target width.
    * 
    * If strict is true, do everything possible to meet the width.  If false, it's advisory only.
    * Numbers, for example, should generally clip (or fail to print) only in strict mode.
    */
  def append(target: StB, a: A, limit: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
    val space = if limit > 0 then limit min (ctx.width max 1) else ctx.width max 1
    appendImpl(target, a, space, strict)

  /** Place the displayable into a new `String`. */
  def display(a: A)(using ctx: Display.Context): String =
    val stb = new StB
    append(stb, a, ctx.width, false)
    stb.toString
}
object Display {
  trait Auto {
    final type Me = this.type
    def myDisplay: Display[this.type]
  }
  object Auto {
    private class Impl[A](value: A)(disp: Display[value.type]) extends Auto {
      val myDisplay: Display[Me] = new Display[Me] {
        def appendImpl(target: StB, a: Me, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
          disp.appendImpl(target, value, space, strict)
      }
    }
    def apply[A](a: A)(using disp: Display[A]): Auto =
      new Impl(a)(disp)
  }

  /** Encapsulates formatting information regarding a displayed item so a supervisor can make adjustments. */
  opaque type Info = Unit
  object Info {
    def default: Info = ()
    def wrap(u: Unit): Info = u
    extension (info: Info)
      def unwrap: Unit = info
  }

  /** Standard style settings for displaying objects. */
  trait Context {
    def tabular: Boolean = false
    def humanized: Boolean = false
    def hashover: Boolean = false
    def width: Int
    def sub(count: Int): Context = this
  }
  object Context {
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
        => if c == '\'' then 1 else if c == '"' then 2 else if c == '\\' then -1 else if c < 127 then 0 else 3
      case SPACE_SEPARATOR => 3
      case _ => -1

  def decimalDigitsOf(value: ULong): Int =
    val l = value.signed
    if l == 0 then 1
    else if l > 0 then
      var i = 0
      while i+1 < powersOfTen.length && l >= powersOfTen(i+1) do i += 1
      i+1
    else
      if l < -8446744073709551616L then 19 else 20

  def appendEnclosed[A](disp: Display[A])(target: StB, a: A, limit: Int, strict: Boolean, open: String, close: String, short: Char)(using ctx: Display.Context): Display.Info =
    val space = if limit > 0 then limit min (ctx.width max 1) else ctx.width max 1
    if space < open.length + close.length + 1 then
      target append short
      Info.default
    else
      val l = target.length
      target append open
      disp.append(target, a, space - open.length - close.length, strict)
      target append close
      if strict && target.length - l > space then
        target.setLength(l + open.length)
        target append '\u2026'
        target append close
      Info.default

  given defaultUnitDisplay: Display[Unit] with
    protected def appendImpl(target: StB, a: Unit, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      target append '\u25CC'
      Info.default

  given defaultBooleanDisplay: Display[Boolean] with
    protected def appendImpl(target: StB, a: Boolean, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      if space > 4 || (space == 4 && !ctx.tabular) then target append a
      else target append (if a then 'T' else 'F')
      Info.default

  given defaultULongDisplay: Display[ULong] with
    protected def appendImpl(target: StB, a: ULong, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      var m = space
      target append java.lang.Long.toUnsignedString(a.unwrap)
      if target.length - l > space then
        if strict then
          target.setLength(l)
          while m > 0 do
            val n = m min 300
            target.append(lotsOfHashes, 0, n)
            m -= n
      Info.default

  given defaultUByteDisplay: Display[UByte] with
    protected def appendImpl(target: StB, a: UByte, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      summon[Display[ULong]].append(target, a.toULong, space, strict)

  given defaultLongDisplay: Display[Long] with
    protected def appendImpl(target: StB, a: Long, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      var m = space
      if ctx.tabular && a >= 0 then target append ' '
      target append a
      if target.length - l > space then
        if strict then
          target.setLength(l)
          while m > 0 do
            val n = m min 300
            target.append(lotsOfHashes, 0, n)
            m -= n
      Info.default

  given defaultByteDisplay: Display[Byte] with
    protected def appendImpl(target: StB, a: Byte, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      summon[Display[Long]].append(target, a.toLong, space, strict)

  given defaultShortDisplay: Display[Short] with
    protected def appendImpl(target: StB, a: Short, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      summon[Display[Long]].append(target, a.toLong, space, strict)

  given defaultIntDisplay: Display[Int] with
    protected def appendImpl(target: StB, a: Int, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      summon[Display[Long]].append(target, a.toLong, space, strict)

  given defaultFloatDisplay: Display[Float] with
    protected def appendImpl(target: StB, a: Float, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      ???

  given defaultDoubleDisplay: Display[Double] with
    protected def appendImpl(target: StB, a: Double, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      ???

  given defaultCharDisplay: Display[Char] with
    protected def appendImpl(target: StB, a: Char, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      needsSpecialHandling(a) match
        case 0 =>
          target append a
        case x if x > 0 =>
          if space < 3 then target append '\u2026'
          else if space > 3 && a == '\'' then target append "'\\''"
          else
            target append '\''
            target append a
            target append '\''
        case _ =>
          if space <= 3 && (space < 3 || a < ' ') then
            target append (if a < ' ' then (0x2400+a).toChar else '\u2026')
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
            if a == '\\' then target append "'\\\\'"
            else if space < 8 then target append "'\\u\u2026'"
            else
              target append "'\\u"
              target append a.hexString
              target append '\''
      Info.default

  given defaultStringDisplay: Display[String] with
    protected override def nesting = true
    protected def appendImpl(target: StB, a: String, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      val m = space +# l
      if a.isEmpty then
        if m-l >= 2 then target append "\"\""
        else target append '\u2026'
      else
        var i = 0
        var j = l
        var k = l
        var unquoted = true
        while unquoted && i < a.length && j < m do
          val c = a.charAt(i)
          if needsSpecialHandling(c) == 0 then
            target append c
            i += 1
            k = j
            j += 1
          else
            unquoted = false
        if j+1 >= m then
          unquoted = true
        else if !unquoted then
          target.insert(l, '"')
          k += 1
          j += 1
          var more = true
          while more && i < a.length && j < m do
            val c = a.charAt(i)
            needsSpecialHandling(c) match
              case 0 => target append c
              case 2 => target append "\\\""
              case x if x > 0 => target append c
              case _ =>
                if c < ' ' then
                  if      c == '\n' then target append "\\n"
                  else if c == '\r' then target append "\\r"
                  else if c == '\t' then target append "\\t"
                  else if c == '\b' then target append "\\b"
                  else if c == '\f' then target append "\\f"
                  else
                    target append "\\x"
                    target append c.toByte.hexString
                else
                  if c == '\\' then target append "\\\\"
                  else
                    target append "\\u"
                    target append c.hexString
            val n = target.length
            if n < m then
              k = j
              j = n
              i += 1
            else
              more = false
          if more then
            target append '"'
        if i < a.length || target.length > m then
          if unquoted then
            if m > l then target.setLength(m-1)
            else target.setLength(l)
            target append '\u2026'
          else
            if target.length <= m-2 then
              target append "\u2026\""
            else if m-3 >= l then
              target.setLength(if j <= m-2 then j else k)
              target append "\u2026\""
            else
              target.setLength(l)
              target append '\u2026'
      Info.default

  given defaultArrayDisplay[A](using disp: Display[A]): Display[Array[A]] with
    protected override def nesting = true
    protected def appendImpl(target: StB, a: Array[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      val m = space +# l
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
            disp.append(target, b, if disp.nesting then space - (spaci - l) - 3 max 1 else space, strict)
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

  given Display[Nothing] with
    protected def appendImpl(target: StB, a: Nothing, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      target append "???"
      Info.default

  given defaultThrowableDisplay(using disp: Display[String]): Display[Throwable] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Throwable, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      disp.append(target, a.explain(), space, strict)

  given defaultErrDisplay(using disp: Display[String]): Display[Err] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Err, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      disp.append(target, a.toString, space, strict)


  given defaultOptionDisplay[A](using disp: Display[A]): Display[Option[A]] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Option[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a match
        case Some(x) =>
          if disp.nesting then disp.append(target, x, space, strict)
          else
            val m = target.length +# space
            disp.append(target, x, space, strict)
            if target.length > m then
              target.setLength(m-1)
              target append '\u2026'
        case None =>
          space match
            case n if n >= 6 => target append "(none)"
            case n if n >= 4 => target append "None"
            case n if n >= 2 => target append "NA"
            case _           => target append '\u2205'
          Info.default

  given defaultEitherDisplay[L, R](using dl: Display[L], dr: Display[R]): Display[Either[L, R]] with
    override protected def nesting = true
    protected def appendImpl(target: StB, a: Either[L, R], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a match
        case Right(r) => appendEnclosed(dr)(target, r, space, strict, "Right(", ")", '\u25D1')
        case Left(l) =>  appendEnclosed(dl)(target, l, space, strict,  "Left(", ")", '\u25D0')

  given defaultTryDisplay[A](using da: Display[A], dt: Display[Throwable]): Display[Try[A]] with
    override protected def nesting = true
    protected def appendImpl(target: StB, a: Try[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a match
        case Success(x) => da.append(target, x, space, strict)
        case Failure(t) => appendEnclosed(dt)(target, t, space, strict, "Failure(", ")", '\u2622')

  given defaultOrDisplay[I, A](using di: Display[I], da: Display[A]): Display[I Or A] with
    override protected def nesting = if da eq defaultUnitDisplay then di.nesting else true
    protected def appendImpl(target: StB, a: I Or A, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.fold{ i =>
        if i == () then
          target append '\u2611'
          Info.default
        else di.append(target, i, space, strict)
      }{ x =>
        if x == () then
          target append '\u2612'
          Info.default
        else appendEnclosed(da)(target, x, space, strict, "Alt(", ")", '\u26A0')
      }

  given defaultIteratorDisplay: Display[Iterator[Any]] with
    protected def appendImpl(target: StB, a: Iterator[Any], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.knownSize match
        case x if x  < 0 => target append (if space < 10 then '\u23EF' else "(iterator)")
        case x if x == 0 => target append (if space <  7 then '\u23F9' else "(empty)")
        case x =>
          if x < 8 + decimalDigitsOf(x.toULong) then target append '\u23F5'
          else
            target append '('
            target append x
            target append " items)"
      Info.default

  given defaultStepperDisplay: Display[scala.collection.Stepper[Any]] with
    protected def appendImpl(target: StB, a: scala.collection.Stepper[Any], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.estimateSize match
        case x if x  < 0 => target append (if space < 9 then '\u23EF' else "(stepper)")
        case x if x == 0 => target append (if space < 7 then '\u23F9' else "(empty)")
        case x =>
          if x < 8 + decimalDigitsOf(x.toULong) then target append '\u23F5'
          else
            target append '('
            target append x
            target append " items)"
      Info.default

  given defaultMuDisplay[A](using disp: Display[A]): Display[Mu[A]] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Mu[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      disp.append(target, a.value, space, strict)

  given defaultAnonDisplay: Display[Anon[_]] with
    protected def appendImpl(target: StB, a: Anon[_], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      if space < 3 then target append '\u2026'
      else target append "..."
      Info.default

  given defaultIdentityDisplay[A](using disp: Display[A]): Display[Identity[A]] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Identity[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      disp.append(target, a.value, space, strict)

  given defaultLazyDisplay: Display[Lazy[_]] with
    protected def appendImpl(target: StB, a: Lazy[_], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      if space < 5 then target append '\u2026'
      else target append "(lazy)"
      Info.default

  given defaultWormDisplay[A](using disp: Display[A]): Display[Worm[A]] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Worm[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.getOrUnit.fold{
        x => disp.append(target, x, space, strict)
      }{
        _ =>
          if space < 7 then target append '\u2610'
          else target append "(unset)"
          Info.default
      }

  given defaultSoftDisplay[A](using disp: Display[A]): Display[Soft[_, A]] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Soft[_, A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.valueOrUnit.fold{
        x => disp.append(target, x, space, strict)
      }{
        _ =>
          if space < 10 then target append '\u2610'
          else target append "(uncached)"
          Info.default
      }

  given defaultHoldDisplay[A](using disp: Display[A]): Display[Hold[A]] with
    override protected def nesting = disp.nesting
    protected def appendImpl(target: StB, a: Hold[A], space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.getOrUnit.fold{
        x => disp.append(target, x._1, space, strict)
      }{
        _ =>
          if space < 10 then target append '\u2610'
          else target append "(uncached)"
          Info.default
      }

  given defaultVcDisplay(using disp: Display[Float]): Display[Vc] with
    protected def appendImpl(target: StB, a: Vc, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append '<'
      disp.append(target, a.x, space, strict)
      target append ", "
      disp.append(target, a.y, space, strict)
      target append '>'
      if target.length - l > space && strict then
        target.setLength(l)
        if space >= 6 then target append "<\u2026, \u2026>"
        else target append '\u2026'
      Info.default

  given defaultPlusMinusDisplay(using disp: Display[Float]): Display[PlusMinus] with
    protected def appendImpl(target: StB, a: PlusMinus, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      disp.append(target, a.value, space, strict)
      target append " \u00B1 "
      val m = target.length
      disp.append(target, a.error, space, strict)
      if target.length - l > space && strict then
        if m - l < space then
          target.setLength(m)
          target append '\u2026'
        else
          target.setLength(l)
          if space >= 5 then target append "\u2026 \u00B1 \u2026"
          else target append '\u2026'
      Info.default

  given defaultFracDisplay(using disp: Display[Int]): Display[Frac] with
    protected def appendImpl(target: StB, a: Frac, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      disp.append(target, a.numer, space, strict)
      target append " over "
      val m = target.length
      disp.append(target, a.denom, space, strict)
      if target.length - l > space && strict then
        if target.length - l <= space - 5 then
          target.replace(m - 6, m, "/")
        else
          target.setLength(l)
          if space >= 3 then target append "\u2026/\u2026"
          else target append '\u2026'
      Info.default

  given defaultDurationDisplay: Display[java.time.Duration] with
    protected def appendImpl(target: StB, a: java.time.Duration, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append a.toString
      if target.length - l > space && strict then
        target.setLength(l)
        target append '\u2026'
      Info.default

  given defaultNanoDurationDisplay: Display[NanoDuration] with
    protected def appendImpl(target: StB, a: NanoDuration, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append a.unwrap
      target append "ns"
      if target.length - l > space then
        target.setLength(l)
        if strict || space < 3 then target append '\u2026'
        else if space < 7 then target append "\u2026ns"
        else
          var t = a.round.into.us
          var n = 0
          while !(t > -1000 && t < 1000) do
            t = if t < 0 then (t - 500)/1000 else (t + 500)/1000
            n += 1
          target append t
          val units = n match
            case 0 => "us"
            case 1 => "ms"
            case 2 => "s"
            case 3 => "ks"
            case 4 => "Ms"
            case 5 => "Gs"
            case _ => "Ts"
          target append units
      Info.default

  given defaultDoubleDurationDisplay(using disp: Display[Double]): Display[DoubleDuration] with
    protected def appendImpl(target: StB, a: DoubleDuration, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      disp.append(target, a.unwrap, 1 max (space - 1), strict)
      if target.length - l < space then target append 's'
      Info.default

  given defaultNanoInstantDisplay: Display[NanoInstant] with
    protected def appendImpl(target: StB, a: NanoInstant, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append "stamp:"
      target append a.unwrap
      target append "ns"
      if target.length - l > space && strict then
        target.setLength(l)
        target append '\u2026'
      Info.default

  given defaultDoubleInstantDisplay(using disp: Display[Double]): Display[DoubleInstant] with
    protected def appendImpl(target: StB, a: DoubleInstant, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append "epoch+"
      disp.append(target, a.unwrap, 1 max (space - 7), strict)
      target append 's'
      if target.length - l > space then
        target.setLength(l)
        target append '\u2026'
      Info.default

  given defaultInstantDisplay: Display[java.time.Instant] with
    protected def appendImpl(target: StB, a: java.time.Instant, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append a.round.us.toString
      if target.length - l > space && strict then
        if space > 5 then
          target.setLength(l + space - 2)
          target append "\u2026Z"
        else
          target.setLength(l)
          target append '\u2026'
      Info.default

  given defaultLocalDateTimeDisplay: Display[java.time.LocalDateTime] with
    protected def appendImpl(target: StB, a: java.time.LocalDateTime, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append a.round.us.toString
      if target.length - l > space && strict then
        if space > 4 then target.setLength(l + space - 1)
        else target.setLength(l)
        target append '\u2026'
      Info.default

  given defaultOffsetDateTimeDisplay: Display[java.time.OffsetDateTime] with
    protected def appendImpl(target: StB, a: java.time.OffsetDateTime, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append a.round.ms.toString
      if target.length - l > space && strict then
        target.setLength(l)
        target append a.round.m.toString
        (target.length - l) match
          case x if x > space =>
            target.setLength(l)
            target append '\u2026'
          case x if x <= space + 3 =>
            target.setLength(l)
            target append a.round.s.toString
      Info.default

  given defaultZonedDateTimeDisplay: Display[java.time.ZonedDateTime] with
    protected def appendImpl(target: StB, a: java.time.ZonedDateTime, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      target append a.round.ms.toString
      if target.length - l > space && strict then
        target.setLength(l)
        target append a.round.m.toString
        (target.length - l) match
          case x if x > space =>
            target.setLength(l)
            target append '\u2026'
          case x if x <= space + 3 =>
            target.setLength(l)
            target append a.round.s.toString
      Info.default

  given defaultFileTimeDisplay: Display[java.nio.file.attribute.FileTime] with
    protected def appendImpl(target: StB, a: java.nio.file.attribute.FileTime, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      val l = target.length
      val t = a.double.unwrap
      if t < -6.4e16 || t > 6.4e16 then
        //FileTime toString overflows the year (??!) so change the display
        val myr = (t / 31556952e6).round
        target append (if myr < 0 then "epoch" else "epoch+")
        target append myr
        target append "Myr"
        if target.length - l > space && strict then
          target.setLength(l)
          target append '\u2026'
      else
        // FileTime toString gives a sensible answer
        target append a.round.ms.toString
        if target.length - l > space && strict then
          if space > 5 then
            target.setLength(l + space - 2)
            target append "\u2026Z"
          else
            target.setLength(l)
            target append '\u2026'
      Info.default

  given defaultAutoDisplayDisplay: Display[Auto] with
    protected def appendImpl(target: StB, a: Auto, space: Int, strict: Boolean)(using ctx: Display.Context): Display.Info =
      a.myDisplay.append(target, a, space, strict)
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
    protected def appendImpl(target: StB, a: Boolean, limit: Int, anchor: Int, account: Coordinator Or Unit): Info =
      target append (if limit >= 5 then a else if a then "T" else "F")
      Info.default

  given Displey[String] with
    protected def appendImpl(target: StB, a: String, limit: Int, anchor: Int, account: Coordinator Or Unit): Info =
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

extension [A](a: A)(using disp: Display[A], ctx: Display.Context) {
  inline def display: String = disp.display(a)(using ctx)
}

abstract class Displayable extends (StB => Unit) {}
object Displayable {
  private def h1(c: Char): Int =
    if c < '0'       then -1
    else if c <= '9' then (c - '0')
    else ((c & 0xFFDF) - 'A') match
      case x if x >= 0 && x < 6 => x
      case _ => -1

  private def h4(input: String, j: Int): Int =
    val a = h1(input.charAt(j+3)); if a < 0 then return -1
    val b = h1(input.charAt(j+2)); if b < 0 then return -1
    val c = h1(input.charAt(j+1)); if c < 0 then return -1
    val d = h1(input.charAt(j  )); if d < 0 then return -1
    a + 0x10*b + 0x100*c + 0x1000*d 

  def appendWithConvertedEscapes(target: StB, input: String): Unit =
    var i = 0
    var j = input.indexOf('\\')
    var sneaky = 0
    while j >= 0 do
      if j > i then target.append(input, i, j)
      if j+1 < input.length then
        j += 1
        input.charAt(j) match
          case 'n'  => target append '\n'
          case 't'  => target append '\t'
          case '\\' => target append '\\'
          case '"'  => target append '"'
          case '\'' => target append '\''
          case 'r'  => target append '\r'
          case 'f'  => target append '\f'
          case 'b'  => target append '\b'
          case 'u' if j+4 < input.length && { sneaky = h4(input, j+1); sneaky >= 0 } =>
            target append sneaky.toChar
            j += 4
          case _ => target.append(input, j-1, j+1)
      else target append '\\'
      i = j + 1
      j = input.indexOf('\\', i)
    if i < input.length then target.append(input, i, input.length)  

  given displayer[A](using disp: Display[A]): Conversion[A, Displayable] with
    def apply(a: A): Displayable = (stb: StB) => { disp.append(stb, a, Int.MaxValue, false); () }
}

extension (sc: StringContext) {
  def rawdisp(items: Displayable*): String =
    val ts = sc.parts.iterator
    val is = items.iterator
    val stb = new StB()
    if ts.hasNext then stb append ts.next
    while is.hasNext do
      is.next.apply(stb)
      stb append ts.next
    stb.toString

  def disp(items: Displayable*): String =
    val ts = sc.parts.iterator
    val is = items.iterator
    val stb = new StB()
    if ts.hasNext then Displayable.appendWithConvertedEscapes(stb, ts.next)
    while is.hasNext do
      is.next.apply(stb)
      Displayable.appendWithConvertedEscapes(stb, ts.next)
    stb.toString
}
