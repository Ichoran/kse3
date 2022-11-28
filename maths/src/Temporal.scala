// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020-22 Rex Kerr and Calico Life Sciences, LLC.

package kse.maths

import java.lang.{Math => jm}
import java.time._
import java.util.concurrent.TimeUnit
import java.nio.file.attribute.FileTime

import scala.annotation.targetName

import kse.maths._



extension (inline t: Byte | Short | Int | Long | Float | Double) {
  transparent inline def days: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofDays(i)
    case d: Double => DoubleDuration(d * 86400)
    case _ => compiletime.error("Use .days on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.days or .toDouble.days to specify which.")

  transparent inline def day: Duration | kse.maths.DoubleDuration = inline t match
    case _: 1   => Duration.ofDays(1)
    case _: 1.0 => DoubleDuration(86400.0)
    case _      => compiletime.error("Use .day on 1 to get a Duration or on 1.0 to get a DoubleDuration.\nInput has neither type; use .toInt.days or .toDouble.days to specify which.")

  transparent inline def h: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofHours(i)
    case d: Double => DoubleDuration(d * 3600)
    case _ => compiletime.error("Use .h on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.hr or .toDouble.hr to specify which.")

  transparent inline def m: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofMinutes(i)
    case d: Double => DoubleDuration(d * 60)
    case _ => compiletime.error("Use .m on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.m or .toDouble.m to specify which.")

  transparent inline def s: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofSeconds(i)
    case d: Double => DoubleDuration(d)
    case _ => compiletime.error("Use .s on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.s or .toDouble.s to specify which.")

  transparent inline def ms: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofMillis(i)
    case d: Double => DoubleDuration(d / 1e3)
    case _ => compiletime.error("Use .ms on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.ms or .toDouble.ms to specify which.")

  transparent inline def us: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofNanos(i*1000L)
    case d: Double => DoubleDuration(d / 1e6)
    case _ => compiletime.error("Use .us on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.us or .toDouble.us to specify which.")

  transparent inline def ns: Duration | kse.maths.DoubleDuration = inline t match
    case i: Int => Duration.ofNanos(i)
    case d: Double => DoubleDuration(d / 1e9)
    case _ => compiletime.error("Use .ns on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.ns or .toDouble.ns to specify which.")

  transparent inline def ns_nano: kse.maths.NanoDuration = inline t match
    case i: Int => NanoDuration(i+0L)
    case l: Long => NanoDuration(l)
    case _ => compiletime.error(".ns_nano only creates NanoDurations out of Int or Long")

  transparent inline def us_nano: kse.maths.NanoDuration = inline t match
    case i: Int => NanoDuration(i*1000L)
    case _ => compiletime.error(".us_nano only creates NanoDurations out of Int")

  transparent inline def ms_nano: kse.maths.NanoDuration = inline t match
    case i: Int => NanoDuration(i*1000000L)
    case _ => compiletime.error(".ms_nano only creates NanoDurations out of Int")

  transparent inline def s_nano: kse.maths.NanoDuration = inline t match
    case i: Int => NanoDuration(i*1000000000L)
    case _ => compiletime.error(".s_nano only creates NanoDurations out of Int")

  transparent inline def m_nano: kse.maths.NanoDuration = inline t match
    case s: Short => NanoDuration(s*60000000000L)
    case _ => compiletime.error(".m_nano only creates NanoDurations out of Short")

  transparent inline def h_nano: kse.maths.NanoDuration = inline t match
    case s: Short => NanoDuration(s*3600000000000L)
    case _ => compiletime.error(".h_nano only creates NanoDurations out of Short")

  transparent inline def days_nano: kse.maths.NanoDuration = inline t match
    case s: Short => NanoDuration(s*86400000000000L)
    case _ => compiletime.error(".days_nano only creates NanoDurations out of Short")

  transparent inline def day_nano: kse.maths.NanoDuration = inline t match
    case _: 1 => NanoDuration(86400000000000L)
    case _ => compiletime.error(".day_nano only creates NanoDurations out of the literal number 1")
}




object DurationCompanion {
  val MAX: Duration = Duration.ofSeconds(Long.MaxValue, 999999999)
  val MIN: Duration = Duration.ofSeconds(Long.MinValue)

  val MaxMicros: Duration  = Duration.ofSeconds(Long.MaxValue, 999999000)
  val MaxMillis: Duration  = Duration.ofSeconds(Long.MaxValue, 999000000)
  val MaxSeconds: Duration = Duration.ofSeconds(Long.MaxValue)
  val MinSeconds: Duration = MIN
  val MaxMinutes: Duration = Duration.ofSeconds( 9223372036854775800L)
  val MinMinutes: Duration = Duration.ofSeconds(-9223372036854775800L)
  val MaxHours: Duration   = Duration.ofSeconds( 9223372036854774000L)
  val MinHours: Duration   = Duration.ofSeconds(-9223372036854774000L)
  val MaxDays: Duration    = Duration.ofSeconds( 9223372036854720000L)
  val MinDays: Duration    = Duration.ofSeconds(-9223372036854720000L)

  val MaxLongNanos:  Duration = Duration.ofSeconds( 9223372036L, 854775807)
  val MinLongNanos:  Duration = Duration.ofSeconds(-9223372036L,-854775808)
  val MaxLongMicros: Duration = Duration.ofSeconds( 9223372036854L, 775807000)
  val MinLongMicros: Duration = Duration.ofSeconds(-9223372036854L,-775808000)
  val MaxLongMillis: Duration = Duration.ofSeconds( 9223372036854775L, 807000000)
  val MinLongMillis: Duration = Duration.ofSeconds(-9223372036854775L,-808000000)

  /*
  private[this] def robustFileTimeFrom(a: Long, scale: Long, bs: Long, bn: Int, sub: Boolean, sig: Int): FileTime =
    // Note: as and bs are seconds (any value)
    // an and bn are nanoseconds; the former ranges from -999999999 to 999999999, while the latter is 0 to 999999999
    // The times are interpreted, in nanoseconds, as (xs*1000000000 + xn)
    // If sub == true, then subtract b from a; otherwise add
    // sig is the smallest significant value (don't bother computing below that)
    val xh = as >> 8
    val xl = (as & 0xFF)*1000000000L + an
    val yh = bs >> 8
    val yl = (bs & 0xFF)*1000000000L + bn
    var zh = if sub then xh - yh else xh + yh
    var zl = if sub then xl - yl else xl + yl
    while zl < 0 != zh < 0 && zh != 0 do
      if zl < 0 then
        zl += 256000000000L
        zh -= 1
      else
        zl -= 256000000000L
        zh += 1
    val neg = zh < 0 || (zh == 0 && zl < 0)
    if neg then
      zh = -zh
      zl = -zl
    while zl >= 256000000000L do
      zl -= 256000000000L
      zh += 1
    if sig == 1 && (zh < 36028797 || (zh == 36028797 && zl <= 4854775807L)) then
      if (zl % 1000000) == 0 then
        if neg then FileTime.fromMillis(-zh*256000L - zl/1000000)
        else        FileTime.fromMillis( zh*256000L + zl/1000000)
      else
        if neg then FileTime.from(-zh*256000000000L - zl, TimeUnit.NANOSECONDS)
        else        FileTime.from( zh*256000000000L + zl, TimeUnit.NANOSECONDS)
    else if sig <= 1000 && (zh < 36028797018L || (zh == 36028797018L && zl <= 246775807999L)) then
      if (zl % 1000000) == 0 then
        if neg then FileTime.fromMillis(-zh*256000L - zl/1000000)
        else        FileTime.fromMillis( zh*256000L + zl/1000000)
      else
        if neg then FileTime.from(-zh*256000000L - zl/1000, TimeUnit.MICROSECONDS)
        else        FileTime.from( zh*256000000L + zl/1000, TimeUnit.MICROSECONDS)
    else if sig <= 1000000 && (zh < 36028797018963L || (zh == 36028797018963L && zl <= 247807999999L)) then
      if neg then FileTime.fromMillis(-zh*256000L - zl/1000000)
      else        FileTime.fromMillis( zh*256000L + zl/1000000)
    else if zh < 36028797018963968L then
      if neg then FileTime.from(-zh*256L - zl/1000000000, TimeUnit.SECONDS)
      else        FileTime.from( zh*256L + zl/1000000000, TimeUnit.SECONDS)
    else
      if neg then FileTimeCompanion.MIN
      else        FileTimeCompanion.MAX

  def robustAddition(ft: FileTime, d: Duration, subtract: Boolean): FileTime =
    var ds = d.getSeconds
    var dn = d.getNano
    var fms = ft.toMillis
    if fms > Long.MinValue && fms < Long.MaxValue then
      if fms > -9223372036856L && fms < 9223372036856L then
        val fns = ft.to(TimeUnit.NANOSECONDS)
        if fns == Long.MinValue || fns == Long.MaxValue then
          val fus = ft.to(TimeUnit.MICROSECONDS)
          robustFileTimeFrom(fus / 1000000, 1000*(fus % 1000000).toInt, ds, dn, subtract, 1000)
        else
          robustFileTimeFrom(fns / 1000000000, (fns % 1000000000).toInt, ds, dn, subtract, 1)
      else if fms > -9223372036854777L && fms < -9223372036854777L then
        val fus = ft.to(TimeUnit.MICROSECONDS)
        if fus == Long.MinValue || fus == Long.MaxValue then
          robustFileTimeFrom(fms / 1000, 1000000*(fms % 1000).toInt, ds, dn, subtract, 1000000)
        else
          robustFileTimeFrom(fus / 1000000, 1000*(fus % 1000000).toInt, ds, dn, subtract, 1000)
      else
        robustFileTimeFrom(fms / 1000, 1000000*(fms % 1000).toInt, ds, dn, subtract, 1000000)
    else
      robustFileTimeFrom(ft.to(TimeUnit.SECONDS), 0, ds, dn, subtract, 1000000000)

  def checkedAddition(ft: FileTime, d: Duration, subtract: Boolean): FileTime =
    val ft2 = robustAddition(ft, d, subtract)
    val ft0 = robustAddition(ft2, d, !subtract)
    if ft0 == ft then ft2
    else throw new ArithmeticException("overflow or loss of precision in FileTime")
  */

  def add(d: Duration, dd: Duration): Duration =
    val ns = d.getNano + dd.getNano
    val cs = d.getSeconds +# dd.getSeconds
    if ns == 0 then
      if cs < Long.MaxValue then Duration.ofSeconds(cs)
      else if (d.getSeconds + dd.getSeconds) == Long.MaxValue then DurationCompanion.MaxSeconds
      else DurationCompanion.MAX
    else if cs > Long.MinValue && cs < Long.MaxValue then
      if ns <= 1000000000 then Duration.ofSeconds(cs, ns)
      else Duration.ofSeconds(cs + 1, ns - 1000000000)
    else
      val ss = d.getSeconds + dd.getSeconds
      if ss == cs then
        if cs == Long.MaxValue then Duration.ofSeconds(Long.MaxValue, ns.clamp(0, 999999999))
        else if ns < 1000000000 then Duration.ofSeconds(cs, ns)
        else Duration.ofSeconds(cs + 1, ns - 1000000000)
      else
        if cs == Long.MaxValue then DurationCompanion.MAX
        else if ss != Long.MaxValue || ns < 1000000000 then DurationCompanion.MIN
        else Duration.ofSeconds(Long.MinValue, ns - 1000000000)

  def sub(d: Duration, dd: Duration): Duration =
    val ns = d.getNano - dd.getNano
    val cs = d.getSeconds -# dd.getSeconds
    if ns == 0 then
      if cs < Long.MaxValue then Duration.ofSeconds(cs)
      else if (d.getSeconds - dd.getSeconds) == Long.MaxValue then DurationCompanion.MaxSeconds
      else DurationCompanion.MAX
    else if cs > Long.MinValue && cs < Long.MaxValue then
      if ns >= 0 then Duration.ofSeconds(cs, ns)
      else Duration.ofSeconds(cs - 1, ns + 1000000000)
    else
      val ss = d.getSeconds - dd.getSeconds
      if ss == cs then
        if cs == Long.MinValue then Duration.ofSeconds(cs, ns.clamp(0, 999999999))
        else if ns >= 0 then Duration.ofSeconds(cs, ns)
        else Duration.ofSeconds(cs - 1, ns + 1000000000)
      else
        if cs == Long.MinValue then DurationCompanion.MIN
        else if ss != Long.MinValue || ns >= 0 then DurationCompanion.MAX
        else Duration.ofSeconds(Long.MaxValue, ns + 1000000000)

  def mul(d: Duration, scale: Int): Duration = scale match
    case  0 => Duration.ZERO
    case  1 => d
    case -1 => if d == DurationCompanion.MIN then DurationCompanion.MAX else d.negated
    case  _ =>
      var ns = d.getNano
      var ds = d.getSeconds
      if ns == 0 then
        val ds = d.getSeconds
        val s = ds *# scale
        if s < Long.MaxValue || (s == ds * scale) then Duration.ofSeconds(s)
        else DurationCompanion.MAX
      else if ds > Int.MinValue && ds <= Int.MaxValue then d.multipliedBy(scale)
      else if ds == Long.MinValue then
        if scale < 0 then DurationCompanion.MAX
        else DurationCompanion.MIN
      else
        val neg = scale < 0 == ds >= 0
        if ds < 0 then
          ds = -(ds + 1)
          ns = 1000000000 - ns
        val sc = if scale < 0 then -scale.toLong else scale.toLong
        var hi = (ds >>> 32) * sc
        var md = ((ds >>> 1) & 0x7FFFFFFFL) * sc
        var lo = ((1000000000 * (ds&1)) + ns) * sc
        val lover = lo / 2000000000
        if lover > 0 then
          lo -= 2000000000*lover
          md += lover
        val mover = md >>> 31
        if mover > 0 then
          md = md & 0x7FFFFFFFL
          hi += mover
        if (hi & 0x7FFFFFFFL) != hi then
          if neg then DurationCompanion.MIN else DurationCompanion.MAX
        else
          var s = (hi << 32) + (md << 1) + (if lo >= 1000000000 then 1 else 0)
          var ns = (if lo >= 1000000000 then lo - 1000000000 else lo).toInt
          if neg then
            if ns == 0 then Duration.ofSeconds(-s)
            else Duration.ofSeconds(-s - 1, 1000000000 - ns)
          else Duration.ofSeconds(s, ns)

  def mul(d: Duration, frac: Frac): Duration =
    var md = d.getSeconds
    val numer = frac.numer
    val denom = frac.denom
    val neg = md < 0 != numer < 0
    if denom == 0 then
      if d == Duration.ZERO then d
      else if neg then DurationCompanion.MIN
      else DurationCompanion.MAX
    else
      var hi = 0L
      var lo = d.getNano.toLong
      if md >= 0 then
        hi = md >>> 32
        if (md & 1) == 1 then lo += 1000000000
        md = (md >> 1) & 0x7FFFFFFFL
      else
        if lo == 0 then
          if (md & 1) == 1 then lo += 1000000000
          if md == Long.MinValue then
            hi = 0x80000000L
            md = 0
          else
            hi = ((-md) >>> 32)
            md = ((-md) >>> 1) & 0x7FFFFFFFL
        else
          md += 1
          lo = 1000000000 - lo
          hi = ((-md) >>> 32)
          if (md & 1) == 1 then lo += 1000000000
          md = ((-md) >>> 1) & 0x7FFFFFFFL
      if numer == Int.MinValue then
        hi = hi << 31
        md = md << 31
        lo = lo << 31
      else
        val nm = if numer < 0 then -numer else numer
        hi *= nm
        md *= nm
        lo *= nm
      val hr = hi % denom
      hi /= denom
      md += hr << 31
      val mr = md % denom
      md /= denom
      lo += mr * 2000000000
      lo /= denom
      if lo >= 2000000000 then
        md += lo / 2000000000
        lo = lo % 2000000000
      if md >= 0x7FFFFFFFL then
        hi += (md >> 31)
        md = md & 0x7FFFFFFFL
      if hi > Int.MaxValue then
        if neg then DurationCompanion.MIN else DurationCompanion.MAX
      else
        var s = (hi << 32) + (md << 1) + (if lo >= 1000000000 then 1 else 0)
        var ns = (if lo >= 1000000000 then lo - 1000000000 else lo).toInt
        if neg then
          s = -s
          if ns > 0 then
            s -= 1
            ns = 1000000000 - ns
        Duration.ofSeconds(s, ns)

  def div(d: Duration, factor: Int): Duration = factor match
    case  1 => d
    case -1 => if d.getSeconds == Long.MinValue && d.getNano == 0 then DurationCompanion.MAX else d.negated
    case  0 => if d == Duration.ZERO then d else if d.getSeconds < 0 then DurationCompanion.MIN else DurationCompanion.MAX
    case  _ => d dividedBy factor

  private def divUsingBigInt(ds: Long, dn: Int, es: Long, en: Int, checked: Boolean): Long =
      val bd = BigInt(ds)*1000000000 + dn
      val be = BigInt(es)*1000000000 + en
      val ans = bd / be
      if ans >= Long.MinValue && ans <= Long.MaxValue then ans.toLong
      else if checked then throw new ArithmeticException("long overflow")
      else if ds < 0 == es < 0 then Long.MaxValue
      else Long.MinValue

  private def divUsingLongConversion(ds: Long, dn: Int, es: Long, en: Int, mult: Int, divi: Int, checked: Boolean): Long =
    val nsd = if ds < 0 then (ds + 1)*mult + (1000000000 - dn)/divi else ds*mult + dn/divi
    val nse = if es < 0 then (es + 1)*mult + (1000000000 - en)/divi else es*mult + en/divi
    if nsd == Long.MinValue && nse == -1 then
      if checked then throw new ArithmeticException("long overflow")
      else Long.MaxValue
    else nsd / nse

  def div(d: Duration, e: Duration, checked: Boolean): Long =
    val ds = d.getSeconds
    val dn = d.getNano
    val es = e.getSeconds
    val en = e.getNano
    if es == 0 && en == 0 then
      if checked then throw new ArithmeticException("division by zero")
      else if ds == 0 && dn == 0 then 0L
      else if ds < 0 then Long.MaxValue
      else Long.MinValue
    else if en == 0 then
      if ds == Long.MinValue && dn == 0 && es == -1 then
        if checked then throw new ArithmeticException("long overflow")
        else Long.MaxValue
      else if ds < 0 then (ds + (if dn > 0 then 1 else 0)) / es
      else ds / es
    else if ds == 0 && dn == 0 then 0L
    else
      val dtiny = d.compareTo(MinLongNanos) >= 0 && d.compareTo(MaxLongNanos) <= 0
      val etiny = e.compareTo(MinLongNanos) >= 0 && e.compareTo(MaxLongNanos) <= 0
      if dtiny && etiny then divUsingLongConversion(ds, dn, es, en, 1000000000, 1, checked)
      else if dn % 1000 == 0 && en % 1000 == 0 then
        val dsmall = dtiny || (d.compareTo(MinLongMicros) >= 0 && d.compareTo(MaxLongMicros) <= 0)
        val esmall = etiny || (e.compareTo(MinLongMicros) >= 0 && e.compareTo(MaxLongMicros) <= 0)
        if dsmall & esmall then divUsingLongConversion(ds, dn, es, en, 1000000, 1000, checked)
        else if dn % 1000000 == 0 && en % 1000000 == 0 then
          val dpetite = dsmall || (d.compareTo(MinLongMillis) >= 0 && d.compareTo(MaxLongMillis) <= 0)
          val epetite = esmall || (e.compareTo(MinLongMillis) >= 0 && e.compareTo(MaxLongMillis) <= 0)
          if dpetite && epetite then divUsingLongConversion(ds, dn, es, en, 1000, 1000000, checked)
          else divUsingBigInt(ds, dn, es, en, checked)
        else divUsingBigInt(ds, dn, es, en, checked)
      else divUsingBigInt(ds, dn, es, en, checked)

  private def modUsingBigInt(ds: Long, dn: Int, es: Long, en: Int): Duration =
      val bd = BigInt(ds)*1000000000 + dn
      val be = BigInt(es)*1000000000 + en
      val ans = bd % be
      var s = (ans / 1000000000).toLong
      var ns = (ans - s * 1000000000).toInt
      if ns < 0 then
        s -= 1
        ns += 1000000000
      Duration.ofSeconds(s, ns)

  private def modUsingLongConversion(ds: Long, dn: Int, es: Long, en: Int, mult: Int, divi: Int): Duration =
    val nsd = if ds < 0 then (ds + 1)*mult + (1000000000 - dn)/divi else ds*mult + dn/divi
    val nse = if es < 0 then (es + 1)*mult + (1000000000 - en)/divi else es*mult + en/divi
    val ans = nsd % nse
    var s = ans / mult
    var ns = (ans - s * mult)*divi
    if ns < 0 then
      s -= 1
      ns += 1000000000
    Duration.ofSeconds(s, ns)


  def mod(d: Duration, e: Duration): Duration =
    val ds = d.getSeconds
    val dn = d.getNano
    val es = e.getSeconds
    val en = e.getNano
    if es == 0 && en == 0 then Duration.ZERO
    else if en == 0 then
      if ds < 0  && dn != 0 then Duration.ofSeconds((ds + 1) % es, dn - 1000000000)
      else Duration.ofSeconds(ds % es, dn)
    else if ds == 0 && dn == 0 then Duration.ZERO
    else
      val dtiny = d.compareTo(MinLongNanos) >= 0 && d.compareTo(MaxLongNanos) <= 0
      val etiny = e.compareTo(MinLongNanos) >= 0 && e.compareTo(MaxLongNanos) <= 0
      if dtiny && etiny then modUsingLongConversion(ds, dn, es, en, 1000000000, 1)
      else if dn % 1000 == 0 && en % 1000 == 0 then
        val dsmall = dtiny || (d.compareTo(MinLongMicros) >= 0 && d.compareTo(MaxLongMicros) <= 0)
        val esmall = etiny || (e.compareTo(MinLongMicros) >= 0 && e.compareTo(MaxLongMicros) <= 0)
        if dsmall & esmall then modUsingLongConversion(ds, dn, es, en, 1000000, 1000)
        else if dn % 1000000 == 0 && en % 1000000 == 0 then
          val dpetite = dsmall || (d.compareTo(MinLongMillis) >= 0 && d.compareTo(MaxLongMillis) <= 0)
          val epetite = esmall || (e.compareTo(MinLongMillis) >= 0 && e.compareTo(MaxLongMillis) <= 0)
          if dpetite && epetite then modUsingLongConversion(ds, dn, es, en, 1000, 1000000)
          else modUsingBigInt(ds, dn, es, en)
        else modUsingBigInt(ds, dn, es, en)
      else modUsingBigInt(ds, dn, es, en)

  private def durationAdjustSmall(
    d: Duration, max: Duration,
    base: Int, neg: Int, pos: Int
  ): Duration =
    var nano = d.getNano
    if nano == 0 then d
    else
      var sec = d.getSeconds
      if sec < 0 then
        if nano > 0 then
          nano -= 1000000000
          sec += 1
        val x = (nano + neg) / base
        val ns = x * base
        if nano == ns then d else Duration.ofSeconds(sec, ns)
      else
        val x = (nano + pos) / base
        val ns = x * base
        if sec < Long.MaxValue || ns <= max.getNano then
          if nano == ns then d else Duration.ofSeconds(sec, ns)
        else max

  private def durationAdjustLarge(
    d: Duration, min: Duration, max: Duration,
    base: Int, sneg: Int, nneg: Int, spos: Int, npos: Int
  ): Duration =
    var nano = d.getNano
    var sec = d.getSeconds
    if sec < 0 then
      if nano > 0 then
        sec += 1
        nano += nneg - 1000000000
      val q = sec / base
      val s = q * base
      val r = sec - s
      if r == 0 && nano == 0 then d
      else if (r + sneg + (if nano <= -1000000000 then -1 else 0)) <= -base then
        if s > min.getSeconds then Duration.ofSeconds(s - base)
        else min
      else Duration.ofSeconds(s)
    else
      val q = sec / base
      val s = q * base
      val r = sec - s
      if r == 0 && nano == 0 then d
      else if (r + spos + (if nano + npos < 1000000000 then 0 else 1)) >= base then
        if s < max.getSeconds then Duration.ofSeconds(s + base)
        else max
      else Duration.ofSeconds(s)

  private def durationSmallToLong(
    d: Duration, min: Duration, max: Duration,
    base: Int, neg: Int, pos: Int,
    die: Boolean
  ): Long =
    var sec = d.getSeconds
    var nano = d.getNano
    if sec < 0 then
      if nano > 0 then
        sec += 1
        nano -= 1000000000
      if d.compareTo(min) > 0 then sec * (1000000000/base) + (nano + neg)/base
      else if !die then Long.MinValue
      else if sec == min.getSeconds + 1 && (nano + neg)/base == (min.getNano - 1000000000)/base then Long.MinValue
      else throw new ArithmeticException("long overflow")
    else
      if d.compareTo(max) < 0 then sec * (1000000000/base) + (nano + pos)/base
      else if !die || (sec == max.getSeconds && (nano + pos)/base == max.getNano/base) then Long.MaxValue
      else throw new ArithmeticException("long overflow")

  private def durationLargeToLong(
    d: Duration,
    base: Int, sneg: Int, nneg: Int, spos: Int, npos: Int
  ): Long =
    var sec = d.getSeconds
    var nano = d.getNano
    val neg = sec < 0
    if neg && nano > 0 then
      sec += 1
      nano -= 1000000000
    val q = sec / base
    val r = sec - q * base + (if neg then sneg + (nano + nneg)/1000000000 else spos + (nano + npos)/1000000000)
    q + (if r <= -base then -1 else if r >= base then 1 else 0)

  opaque type Into = Duration
  object Into {
    def apply(d: Duration): kse.maths.DurationCompanion.Into = d

    extension (in: Into) {
      def ns: Long =
        var sec = in.getSeconds
        var nano = in.getNano
        if sec < 0 && nano > 0 then
          nano -= 1000000000
          sec += 1
        (sec *# 1000000000) +# nano
      def us: Long =
        var sec = in.getSeconds
        var nano = in.getNano
        if sec < 0 && nano > 0 then
          nano -= 1000000000
          sec += 1
        (sec *# 1000000) +# nano/1000
      def ms: Long =
        var sec = in.getSeconds
        var nano = in.getNano
        if sec < 0 && nano > 0 then
          nano -= 1000000000
          sec += 1
        (sec *# 1000) +# nano/1000000
      def s:  Long =
        val ans = in.getSeconds
        if ans < 0 && in.getNano > 0 then ans + 1 else ans
      def m:  Long =
        val sec = in.getSeconds
        val ans = sec/60
        if ans < 0 then
          if sec == ans*60 && in.getNano > 0 then ans + 1 else ans
        else ans
      def h:  Long =
        val sec = in.getSeconds
        val ans = sec/3600
        if ans < 0 then
          if sec == ans*3600 && in.getNano > 0 then ans + 1 else ans
        else ans
      def d:  Long =
        val sec = in.getSeconds
        val ans = sec/86400
        if ans < 0 then
          if sec == ans*86400 && in.getNano > 0 then ans + 1 else ans
        else ans
      inline def days: Long = in.d

      inline def round: kse.maths.DurationCompanion.InRound = in
      inline def floor: kse.maths.DurationCompanion.InFloor = in
      inline def ceil:  kse.maths.DurationCompanion.InCeil  = in
    }
  }

  opaque type Check = Duration
  object Check {
    def apply(d: Duration): kse.maths.DurationCompanion.Check = d

    extension (in: Check) {
      def ns: Long = (in: Duration).checkedNano.unwrap
      def us: Long = durationSmallToLong(ceil, MinLongMicros, MaxLongMicros, 1000, 0, 0, true)
      def ms: Long = durationSmallToLong(ceil, MinLongMillis, MaxLongMillis, 1000000, 0, 0, true)

      inline def round: kse.maths.DurationCompanion.CheckRound = in
      inline def floor: kse.maths.DurationCompanion.CheckFloor = in
      inline def ceil:  kse.maths.DurationCompanion.CheckCeil  = in
    }

  }

  opaque type Round = Duration
  object Round {
    def apply(d: Duration): kse.maths.DurationCompanion.Round = d

    extension (round: Round) {
      def us: Duration = durationAdjustSmall(round, MaxMicros, 1000, -499, 499)
      def ms: Duration = durationAdjustSmall(round, MaxMillis, 1000000, -499999, 499999)
      def  s: Duration = durationAdjustSmall(round, MaxSeconds, 1000000000, -499999999, 499999999)
      def  m: Duration = durationAdjustLarge(round, MinMinutes, MaxMinutes, 60, -29, -999999999, 29, 999999999)
      def  h: Duration = durationAdjustLarge(round, MinHours, MaxHours, 3600, -1799, -999999999, 1799, 999999999)
      def  d: Duration = durationAdjustLarge(round, MinDays, MaxDays, 86400, -43199, -999999999, 43199, 999999999)
      inline def days = round.d


      inline def into:    kse.maths.DurationCompanion.InRound    = round
      inline def checked: kse.maths.DurationCompanion.CheckRound = round
    }
  }

  opaque type Floor = Duration
  object Floor {
    def apply(d: Duration): kse.maths.DurationCompanion.Floor = d

    extension (floor: Floor) {
      def us: Duration = durationAdjustSmall(floor, MaxMicros, 1000, -999, 0)
      def ms: Duration = durationAdjustSmall(floor, MaxMillis, 1000000, -999999, 0)
      def  s: Duration = if floor.getNano == 0 then floor else Duration.ofSeconds(floor.getSeconds)
      def  m: Duration = durationAdjustLarge(floor, MinMinutes, MaxMinutes, 60, -59, -999999999, 0, 0)
      def  h: Duration = durationAdjustLarge(floor, MinHours, MaxHours, 3600, -3599, -999999999, 0, 0)
      def  d: Duration = durationAdjustLarge(floor, MinDays, MaxDays, 86400, -86399, -999999999, 0, 0)
      inline def days = floor.d

      inline def into:    kse.maths.DurationCompanion.InFloor    = floor
      inline def checked: kse.maths.DurationCompanion.CheckFloor = floor
    }
  }

  opaque type Ceil = Duration
  object Ceil {
    def apply(d: Duration): kse.maths.DurationCompanion.Ceil = d

    extension (ceil: Ceil) {
      def us: Duration = durationAdjustSmall(ceil, MaxMicros, 1000, 0, 999)
      def ms: Duration = durationAdjustSmall(ceil, MaxMillis, 1000000, 0, 999999)
      def  s: Duration = durationAdjustSmall(ceil, MaxSeconds, 1000000000, 0, 999999999)
      def  m: Duration = durationAdjustLarge(ceil, MinMinutes, MaxMinutes, 60, 0, 0, 59, 999999999)
      def  h: Duration = durationAdjustLarge(ceil, MinHours, MaxHours, 3600, 0, 0, 3599, 999999999)
      def  d: Duration = durationAdjustLarge(ceil, MinDays, MaxDays, 86400, 0, 0, 86399, 999999999)
      inline def days: Duration = ceil.d

      inline def into:    kse.maths.DurationCompanion.InCeil    = ceil
      inline def checked: kse.maths.DurationCompanion.CheckCeil = ceil
    }
  }

  opaque type Trunc = Duration
  object Trunc {
    def apply(d: Duration): kse.maths.DurationCompanion.Trunc = d

    extension (trunc: Trunc) {
      def us: Duration = durationAdjustSmall(trunc, MaxMicros, 1000, 0, 0)
      def ms: Duration = durationAdjustSmall(trunc, MaxMillis, 1000000, 0, 0)
      def  s: Duration = durationAdjustSmall(trunc, MaxSeconds, 1000000000, 0, 0)
      def  m: Duration = durationAdjustLarge(trunc, MinMinutes, MaxMinutes, 60, 0, 0, 0, 0)
      def  h: Duration = durationAdjustLarge(trunc, MinHours, MaxHours, 3600, 0, 0, 0, 0)
      def  d: Duration = durationAdjustLarge(trunc, MinDays, MaxDays, 86400, 0, 0, 0, 0)
      inline def days = trunc.d
    }
  }

  opaque type InRound = Duration
  object InRound {
    extension (round: InRound) {
      def us: Long = durationSmallToLong(round, MinLongMicros, MaxLongMicros, 1000, -499, 499, false)
      def ms: Long = durationSmallToLong(round, MinLongMillis, MaxLongMillis, 1000000, -499999, 499999, false)
      def  s: Long =
        val sec = round.getSeconds
        val nano = round.getNano
        if sec < 0 then
          if nano < 500000000 then sec
          else sec + 1
        else
          if nano > 500000000 && sec < Long.MaxValue then sec + 1
          else sec
      def  m: Long = durationLargeToLong(round, 60, -29, -999999999, 29, 999999999)
      def  h: Long = durationLargeToLong(round, 3600, -1799, -999999999, 1799, 999999999)
      def  d: Long = durationLargeToLong(round, 86400, -43199, -999999999, 43199, 999999999)
      def days: Long = round.d
    }
  }

  opaque type InFloor = Duration
  object InFloor {
    extension (floor: InFloor) {
      def us: Long = durationSmallToLong(floor, MinLongMicros, MaxLongMicros, 1000, -999, 0, false)
      def ms: Long = durationSmallToLong(floor, MinLongMillis, MaxLongMillis, 1000000, -999999, 0, false)
      def  s: Long = floor.getSeconds
      def  m: Long = durationLargeToLong(floor, 60, -59, -999999999, 0, 0)
      def  h: Long = durationLargeToLong(floor, 3600, -3599, -999999999, 0, 0)
      def  d: Long = durationLargeToLong(floor, 86400, -86399, -999999999, 0, 0)
      inline def days = floor.d
    }
  }

  opaque type InCeil = Duration
  object InCeil {
    extension (ceil: InCeil) {
      def us: Long = durationSmallToLong(ceil, MinLongMicros, MaxLongMicros, 1000, 0, 999, false)
      def ms: Long = durationSmallToLong(ceil, MinLongMillis, MaxLongMillis, 1000000, 0, 999999, false)
      def  s: Long = ceil.getSeconds + (if ceil.getNano > 0 && ceil.getSeconds < Long.MaxValue then 1 else 0)
      def  m: Long = durationLargeToLong(ceil, 60, 0, 0, 59, 999999999)
      def  h: Long = durationLargeToLong(ceil, 3600, 0, 0, 3599, 999999999)
      def  d: Long = durationLargeToLong(ceil, 86400, 0, 0, 86399, 999999999)
      inline def days: Long = ceil.d
    }
  }

  opaque type CheckRound = Duration
  object CheckRound {
    inline def apply(d: Duration): kse.maths.DurationCompanion.CheckRound = d
    extension (round: CheckRound) {
      def us: Long = durationSmallToLong(round, MinLongMicros, MaxLongMicros, 1000, -499, 499, true)
      def ms: Long = durationSmallToLong(round, MinLongMillis, MaxLongMillis, 1000000, -499999, 499999, true)
      def s: Long =
        val sec = round.getSeconds
        val nano = round.getNano
        if sec < 0 then
          if nano < 500000000 then sec
          else sec + 1
        else
          if nano > 500000000 then
            if sec < Long.MaxValue then sec + 1
            else throw new ArithmeticException("long overflow")
          else sec
    }
  }

  opaque type CheckFloor = Duration
  object CheckFloor {
    inline def apply(d: Duration): kse.maths.DurationCompanion.CheckFloor = d
    extension (floor: CheckFloor) {
      def us: Long = durationSmallToLong(floor, MinLongMicros, MaxLongMicros, 1000, -999, 0, true)
      def ms: Long = durationSmallToLong(floor, MinLongMillis, MaxLongMillis, 1000000, -999999, 0, true)
    }
  }

  opaque type CheckCeil = Duration
  object CheckCeil {
    inline def apply(d: Duration): kse.maths.DurationCompanion.CheckCeil = d
    extension (ceil: CheckCeil) {
      def us: Long = durationSmallToLong(ceil, MinLongMicros, MaxLongMicros, 1000, 0, 999, true)
      def ms: Long = durationSmallToLong(ceil, MinLongMillis, MaxLongMillis, 1000000, 0, 999999, true)
      def s: Long =
        val sec = ceil.getSeconds
        val nano = ceil.getNano
        if nano == 0 then sec
        else if sec == Long.MaxValue then throw new ArithmeticException("long overflow")
        else sec + 1
    }
  }
}
extension (d: Duration) {
  def unary_- : Duration =
    if d.getSeconds == Long.MinValue && d.getNano == 0 then DurationCompanion.MAX
    else d.negated

  // +(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
  // *(Int) in OverloadedExtensions
  // *(Frac) in OverloadedExtensions
  // /(Int) in OverloadedExtensions
  // /(Frac) in OverloadedExtensions
  // /(Duration) in OverloadedExtensions
  // %(Duration) in OverloadedExtensions
  // +(Instant) in OverloadedExtensions
  // +(LocalDateTime) in OverloadedExtensions
  // +(OffsetDateTime) in OverloadedExtensions
  // +(ZonedDateTime) in OverloadedExtensions
  // +(FileTime) in OverloadedExtensions
  // +!(Duration) in OverloadedExtensions
  // -!(Duration) in OverloadedExtensions
  // *!(Int) in OverloadedExtensions
  // /!(Int) in OverloadedExtensions
  // /!(Duration) in OverloadedExtensions
  // +!(Instant) in OverloadedExtensions
  // +!(LocalDateTime) in OverloadedExtensions
  // +!(OffsetDateTime) in OverloadedExtensions
  // +!(ZonedDateTime) in OverloadedExtensions
  // +!(FileTime) in OverloadedExtensions
  // trunc in OverloadedExtensions

  inline def <( e: Duration) = d.compareTo(e) <  0
  inline def <=(e: Duration) = d.compareTo(e) <= 0
  inline def >=(e: Duration) = d.compareTo(e) >= 0
  inline def >( e: Duration) = d.compareTo(e) >  0

  inline def safeAbs: Duration = if d.getSeconds == Long.MinValue && d.getNano == 0 then DurationCompanion.MAX else d.abs
  inline def min(e: Duration) = if d.compareTo(e) > 0 then e else d
  inline def max(e: Duration) = if d.compareTo(e) < 0 then e else d

  // clamp(Duration, Duration) moved to OverloadedExtensions
  // in(Duration, Duration) moved to OverloadedExtensions
  // checkIn(Duration, Duration) moved to OverloadedExtensions

  def nano: kse.maths.NanoDuration =
    if d.getSeconds < 0 then
      if d.getSeconds == Long.MinValue then NanoDuration(Long.MinValue)
      else if d.getNano == 0 then NanoDuration(d.getSeconds *# 1000000000L)
      else NanoDuration((d.getSeconds + 1) *# 1000000000L +# (d.getNano - 1000000000L))
    else
      NanoDuration(d.getSeconds *# 1000000000L +# d.getNano)
  inline def checkedNano: kse.maths.NanoDuration =
    if d.getSeconds < 0 then
      if d.getSeconds == Long.MinValue then NanoDuration(Long.MinValue)
      else if d.getNano == 0 then NanoDuration(d.getSeconds *! 1000000000L)
      else NanoDuration((d.getSeconds + 1) *! 1000000000L +! (d.getNano - 1000000000L))
    else
      NanoDuration(d.getSeconds *! 1000000000L +! d.getNano)
  inline def double: kse.maths.DoubleDuration = DoubleDuration(d.getNano/1e9 + d.getSeconds)

  inline def into:    kse.maths.DurationCompanion.Into  = DurationCompanion.Into (d)
  inline def checked: kse.maths.DurationCompanion.Check = DurationCompanion.Check(d)
  inline def round:   kse.maths.DurationCompanion.Round = DurationCompanion.Round(d)
  inline def floor:   kse.maths.DurationCompanion.Floor = DurationCompanion.Floor(d)
  inline def ceil:    kse.maths.DurationCompanion.Ceil  = DurationCompanion.Ceil (d)
}



opaque type NanoDuration = Long
object NanoDuration {
  inline def apply(nanos: Long): kse.maths.NanoDuration = nanos
  inline def since(nt: kse.maths.NanoInstant): kse.maths.NanoDuration = nt.age

  final val Zero: kse.maths.NanoDuration     = apply(0L)
  final val MinValue: kse.maths.NanoDuration = apply(Long.MinValue)
  final val MaxValue: kse.maths.NanoDuration = apply(Long.MaxValue)

  extension (dt: NanoDuration) {
    inline def unwrap: Long = dt
  }

  extension (dt: kse.maths.NanoDuration) {
    @targetName("nanodur_plus_nanodur")
    inline def +(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap +# et.unwrap)

    @targetName("nanodur_plus_nanoinst")
    def +(nt: kse.maths.NanoInstant): kse.maths.NanoInstant = NanoInstant(nt.unwrap + dt.unwrap)

    inline def -(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap -# et.unwrap)

    inline def unary_- : kse.maths.NanoDuration = NanoDuration(0L -# dt.unwrap)

    @targetName("nanodur_mul_long")
    inline def *(factor: Long): kse.maths.NanoDuration = NanoDuration(dt.unwrap *# factor)

    @targetName("nanodur_mul_frac")
    def *(frac: kse.maths.Frac): kse.maths.NanoDuration = NanoDuration(Frac.scaleClamped(dt.unwrap, frac))

    @targetName("nanodur_div_long")
    inline def /(factor: Long): kse.maths.NanoDuration = NanoDuration(dt.unwrap /# factor)

    @targetName("nanodur_div_frac")
    inline def /(frac: kse.maths.Frac): kse.maths.NanoDuration = dt * frac.reciprocal

    @targetName("nanodur_div_nanodur")
    inline def /(et: kse.maths.NanoDuration): Long = dt.unwrap /# et.unwrap

    @targetName("nanodur_mod_nanodur")
    inline def %(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap %# et.unwrap)

    @targetName("nanodur_plusxcl_nanodur")
    inline def +!(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(kse.maths.+!(dt.unwrap)(et.unwrap))

    @targetName("nanodur_plusxcl_nanoinst")
    inline def +!(nt: kse.maths.NanoInstant): kse.maths.NanoInstant = NanoInstant(kse.maths.+!(nt.unwrap)(dt.unwrap))

    inline def -!(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(kse.maths.-!(dt.unwrap)(et.unwrap))

    @targetName("nanodur_mulxcl_long")
    inline def *!(factor: Long): kse.maths.NanoDuration = NanoDuration(kse.maths.*!(dt.unwrap)(factor))

    @targetName("nanodur_mulxcl_frac")
    def *!(frac: kse.maths.Frac): kse.maths.NanoDuration = NanoDuration(Frac.scaleExactly(dt.unwrap, frac))

    @targetName("nanodur_divxcl_long")
    inline def /!(factor: Long): kse.maths.NanoDuration = NanoDuration(kse.maths./!(dt.unwrap)(factor))

    @targetName("nanodur_divxcl_frac")
    inline def /!(frac: kse.maths.Frac): kse.maths.NanoDuration = dt *! frac.reciprocal

    @targetName("nanodur_divxcl_nanodur")
    inline def /!(et: kse.maths.NanoDuration): Long = kse.maths./!(dt.unwrap)(et.unwrap)

    inline def <( et: kse.maths.NanoDuration): Boolean = dt.unwrap <  et.unwrap
    inline def <=(et: kse.maths.NanoDuration): Boolean = dt.unwrap <= et.unwrap
    inline def >=(et: kse.maths.NanoDuration): Boolean = dt.unwrap >= et.unwrap
    inline def >( et: kse.maths.NanoDuration): Boolean = dt.unwrap >  et.unwrap

    def abs: kse.maths.NanoDuration =
      if dt.unwrap < 0 then
        if dt.unwrap == Long.MinValue then NanoDuration.MaxValue
        else NanoDuration(-dt.unwrap)
      else dt
    inline def max(et: kse.maths.NanoDuration): kse.maths.NanoDuration = if dt.unwrap < et.unwrap then et else dt
    inline def min(et: kse.maths.NanoDuration): kse.maths.NanoDuration = if dt.unwrap > et.unwrap then et else dt

    def clamp(lo: kse.maths.NanoDuration, hi: kse.maths.NanoDuration): kse.maths.NanoDuration =
      if lo.unwrap <= dt.unwrap then
        if dt.unwrap <= hi.unwrap then dt.unwrap
        else if lo.unwrap <= hi.unwrap then hi
        else lo
      else lo
    inline def in(lo: kse.maths.NanoDuration, hi: kse.maths.NanoDuration) = lo.unwrap <= dt.unwrap && dt.unwrap <= hi.unwrap
    def checkIn(lo: kse.maths.NanoDuration, hi: kse.maths.NanoDuration): kse.maths.NanoDuration =
      if dt.unwrap < lo.unwrap || dt.unwrap > hi.unwrap then throw new ArithmeticException("long overflow")
      else dt

    inline def double: kse.maths.DoubleDuration = DoubleDuration(dt)
    def duration: Duration =
      val s = dt.unwrap/1000000000
      val n = (dt.unwrap - s*1000000000).toInt
      if n >= 0 then Duration.ofSeconds(s, n)
      else           Duration.ofSeconds(s-1, 1000000000 + n)

    inline def into:  kse.maths.NanoDuration.Into  = dt.unwrap
    inline def round: kse.maths.NanoDuration.Round = dt.unwrap
    inline def floor: kse.maths.NanoDuration.Floor = dt.unwrap
    inline def ceil:  kse.maths.NanoDuration.Ceil  = dt.unwrap
    inline def trunc: kse.maths.NanoDuration.Trunc = dt.unwrap

    def pr: String =
      s"${dt.unwrap} ns"
  }

  opaque type Into = Long
  object Into {
    extension (in: Into) {
      inline def ns: Long = in
      inline def us: Long = in/1000
      inline def ms: Long = in/1000000
      inline def  s: Long = in/1000000000
      inline def  m: Int  = (in/60000000000L).toInt
      inline def  h: Int  = (in/3600000000000L).toInt
      inline def  d: Int  = (in/86400000000000L).toInt
      inline def days: Int = in.d

      inline def round: InRound = in
      inline def floor: InFloor = in
      inline def ceil: InCeil   = in
    }
  }

  opaque type Round = Long
  object Round {
    extension (round: Round) {
      def us: kse.maths.NanoDuration =
        NanoDuration(1000L * (if round < 0 then (round -# 499)/1000 else (round +# 499)/1000))

      def ms: kse.maths.NanoDuration =
        NanoDuration(1000000L * (if round < 0 then (round -# 499999)/1000000 else (round +# 499999)/1000000))

      def  s: kse.maths.NanoDuration =
        NanoDuration(1000000000L * (if round < 0 then (round -# 499999999)/1000000000 else (round +# 499999999)/1000000000))

      def  m: kse.maths.NanoDuration =
        NanoDuration(60000000000L * (if round < 0 then (round -# 29999999999L)/60000000000L else (round +# 29999999999L)/60000000000L))

      def  h: kse.maths.NanoDuration =
        NanoDuration(3600000000000L * (if round < 0 then (round -# 1799999999999L)/3600000000000L else (round +# 1799999999999L)/3600000000000L))

      def  d: kse.maths.NanoDuration =
        NanoDuration(86400000000000L * (if round < 0 then (round -# 43199999999999L)/86400000000000L else (round +# 43199999999999L)/86400000000000L))

      inline def days: kse.maths.NanoDuration = round.d

      inline def into: kse.maths.NanoDuration.InRound = round
    }
  }

  opaque type Floor = Long
  object Floor {
    extension (floor: Floor) {
      def us: kse.maths.NanoDuration =
        NanoDuration(1000L * (if floor < 0 then (floor -# 999)/1000 else floor/1000))

      def ms: kse.maths.NanoDuration =
        NanoDuration(1000000L * (if floor < 0 then (floor -# 999999)/1000000 else floor/1000000))

      def  s: kse.maths.NanoDuration =
        NanoDuration(1000000000L * (if floor < 0 then (floor -# 999999999)/1000000000 else floor/1000000000))

      def  m: kse.maths.NanoDuration =
        NanoDuration(60000000000L * (if floor < 0 then (floor -# 59999999999L)/60000000000L else floor/60000000000L))

      def  h: kse.maths.NanoDuration =
        NanoDuration(3600000000000L * (if floor < 0 then (floor -# 3599999999999L)/3600000000000L else floor/3600000000000L))

      def  d: kse.maths.NanoDuration =
        NanoDuration(86400000000000L * (if floor < 0 then (floor -# 86399999999999L)/86400000000000L else floor/86400000000000L))

      inline def days: kse.maths.NanoDuration = floor.d

      inline def into: kse.maths.NanoDuration.InFloor = floor
    }
  }

  opaque type Ceil = Long
  object Ceil {
    extension (ceil: Ceil) {
      def us: kse.maths.NanoDuration =
        NanoDuration(1000L * (if ceil > 0 then (ceil +# 999)/1000 else ceil/1000))

      def ms: kse.maths.NanoDuration =
        NanoDuration(1000000L * (if ceil > 0 then (ceil +# 999999)/1000000 else ceil/1000000))

      def  s: kse.maths.NanoDuration =
        NanoDuration(1000000000L * (if ceil > 0 then (ceil +# 999999999)/1000000000 else ceil/1000000000))

      def  m: kse.maths.NanoDuration =
        NanoDuration(60000000000L * (if ceil > 0 then (ceil +# 59999999999L)/60000000000L else ceil/60000000000L))

      def  h: kse.maths.NanoDuration =
        NanoDuration(3600000000000L * (if ceil > 0 then (ceil +# 3599999999999L)/3600000000000L else ceil/3600000000000L))

      def  d: kse.maths.NanoDuration =
        NanoDuration(86400000000000L * (if ceil > 0 then (ceil +# 86399999999999L)/86400000000000L else ceil/86400000000000L))

      inline def days: kse.maths.NanoDuration = ceil.d

      inline def into: kse.maths.NanoDuration.InCeil = ceil
    }
  }

  opaque type Trunc = Long
  object Trunc {
    extension (trunc: Trunc) {
      def us: kse.maths.NanoDuration = NanoDuration(1000L * (trunc/1000))

      def ms: kse.maths.NanoDuration = NanoDuration(1000000L * (trunc/1000000))

      def  s: kse.maths.NanoDuration = NanoDuration(1000000000L * (trunc/1000000000))

      def  m: kse.maths.NanoDuration = NanoDuration(60000000000L * (trunc/60000000000L))

      def  h: kse.maths.NanoDuration = NanoDuration(3600000000000L * (trunc/3600000000000L))

      def  d: kse.maths.NanoDuration = NanoDuration(86400000000000L * (trunc/86400000000000L))

      inline def days: kse.maths.NanoDuration = trunc.d
    }
  }

  private def nanoDurationInto(value: Long, div: Long, neg: Long, pos: Long): Long =
    val ans = value / div
    val err = value - ans * div
    if err < 0 then
      if err + neg + div <= 0 then ans - 1 else ans
    else
      if err + pos >= div then ans + 1 else ans

  opaque type InRound = Long
  object InRound {
    extension (round: InRound) {
      def us: Long = nanoDurationInto(round, 1000, -499, 499)
      def ms: Long = nanoDurationInto(round, 1000000, -499999, 499999)
      def  s: Long = nanoDurationInto(round, 1000000000, -499999999, 499999999)
      def  m: Int  = nanoDurationInto(round, 60000000000L, -29999999999L, 29999999999L).toInt
      def  h: Int  = nanoDurationInto(round, 3600000000000L, -1799999999999L, 1799999999999L).toInt
      def  d: Int  = nanoDurationInto(round, 86400000000000L, -43199999999999L, 43199999999999L).toInt
      inline def days: Int = round.d
    }
  }

  opaque type InFloor = Long
  object InFloor {
    extension (floor: InFloor) {
      def us: Long = nanoDurationInto(floor, 1000, -999, 0)
      def ms: Long = nanoDurationInto(floor, 1000000, -999999, 0)
      def  s: Long = nanoDurationInto(floor, 1000000000, -999999999, 0)
      def  m: Int  = nanoDurationInto(floor, 60000000000L, -59999999999L, 0).toInt
      def  h: Int  = nanoDurationInto(floor, 3600000000000L, -3599999999999L, 0).toInt
      def  d: Int  = nanoDurationInto(floor, 86400000000000L, -86399999999999L, 0).toInt
      inline def days: Int = floor.d
    }
  }

  opaque type InCeil = Long
  object InCeil {
    extension (ceil: InCeil) {
      def us: Long = nanoDurationInto(ceil, 1000, 0, 999)
      def ms: Long = nanoDurationInto(ceil, 1000000, 0, 999999)
      def  s: Long = nanoDurationInto(ceil, 1000000000, 0, 999999999)
      def  m: Int  = nanoDurationInto(ceil, 60000000000L, 0, 59999999999L).toInt
      def  h: Int  = nanoDurationInto(ceil, 3600000000000L, 0, 3599999999999L).toInt
      def  d: Int  = nanoDurationInto(ceil, 86400000000000L, 0, 86399999999999L).toInt
      inline def days: Int = ceil.d
    }
  }

  given Ordering[kse.maths.NanoDuration] = new {
    def compare(a: kse.maths.NanoDuration, b: kse.maths.NanoDuration) = a.unwrap compareTo b.unwrap
  }
}



opaque type DoubleDuration = Double
object DoubleDuration {
  inline def apply(dt: Double): kse.maths.DoubleDuration = dt
  inline def apply(d: Duration): kse.maths.DoubleDuration = d.getSeconds + d.getNano/1e9
  inline def apply(n: kse.maths.NanoDuration): kse.maths.DoubleDuration = n.unwrap/1e9

  def since(dt: kse.maths.DoubleInstant): kse.maths.DoubleDuration = dt.age

  extension (dt: DoubleDuration) {
    inline def unwrap: Double = dt
  }

  extension (dt: kse.maths.DoubleDuration) {
    @targetName("dbldur_plus_instant")
    inline def +(t: kse.maths.DoubleInstant): kse.maths.DoubleInstant = DoubleInstant(t.unwrap + dt.unwrap)

    @targetName("dbldur_plus_dbldur")
    inline def +(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap + du.unwrap)

    inline def -(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap - du.unwrap)

    inline def unary_- : kse.maths.DoubleDuration = DoubleDuration(-dt.unwrap)

    inline def *(scale: Double): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap * scale)

    @targetName("dbldur_div_double")
    inline def /(scale: Double): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap / scale)

    @targetName("dbldur_div_dbldur")
    inline def /(et: kse.maths.DoubleDuration): Double = dt.unwrap / et.unwrap

    inline def %(et: kse.maths.DoubleDuration): kse.maths.DoubleDuration =
      val x = dt.unwrap
      val y = et.unwrap
      val q = kse.maths.trunc(x / y)
      DoubleDuration(x - y * q)

    inline def <( du: kse.maths.DoubleDuration): Boolean = dt.unwrap <  du.unwrap
    inline def <=(du: kse.maths.DoubleDuration): Boolean = dt.unwrap <= du.unwrap
    inline def >=(du: kse.maths.DoubleDuration): Boolean = dt.unwrap >= du.unwrap
    inline def >( du: kse.maths.DoubleDuration): Boolean = dt.unwrap >  du.unwrap

    inline def abs: kse.maths.DoubleDuration = if dt.unwrap < 0 then DoubleDuration(-dt.unwrap) else dt
    inline def max(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration =
      if dt.unwrap >= du.unwrap then dt else if du.unwrap >= dt.unwrap then du else DoubleDuration(Double.NaN)
    inline def min(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration =
      if dt.unwrap <= du.unwrap then dt else if du.unwrap <= dt.unwrap then du else DoubleDuration(Double.NaN)

    inline def clamp(lo: kse.maths.DoubleDuration, hi: kse.maths.DoubleDuration): kse.maths.DoubleDuration =
      DoubleDuration(kse.maths.clamp(dt.unwrap)(lo.unwrap, hi.unwrap))

    inline def in(lo: kse.maths.DoubleDuration, hi: kse.maths.DoubleDuration): Boolean = kse.maths.in(dt.unwrap)(lo.unwrap, hi.unwrap)

    inline def checkIn(lo: kse.maths.DoubleDuration, hi: kse.maths.DoubleDuration): kse.maths.DoubleDuration =
      kse.maths.checkIn(dt.unwrap)(lo.unwrap, hi.unwrap)

    def nano: kse.maths.NanoDuration = NanoDuration(jm.rint(dt.unwrap * 1e9).toLong)

    def checkedNano: kse.maths.NanoDuration =
      val l = jm.rint(dt.unwrap * 1e9)
      if l >= Long.MinValue && l <= Long.MaxValue then NanoDuration(l.toLong)
      else throw new ArithmeticException("long overflow")

    def duration: Duration =
      val t = jm.floor(dt.unwrap)
      val n = jm.rint((dt.unwrap - t)*1e9)
      Duration.ofSeconds(t.toLong, if t > Long.MaxValue || n > 999999999.0 then 999999999 else n.toInt)

    def checkedDuration: Duration =
      if dt.unwrap >= Long.MinValue && dt.unwrap <= Long.MaxValue then dt.duration
      else throw new ArithmeticException("Duration overflow")

    inline def into:    kse.maths.DoubleDuration.Into      = dt.unwrap
    inline def long:    kse.maths.DoubleDuration.InLong    = dt.unwrap
    inline def checked: kse.maths.DoubleDuration.CheckLong = dt.unwrap

    inline def round: kse.maths.DoubleDuration.Round = dt.unwrap
    inline def floor: kse.maths.DoubleDuration.Floor = dt.unwrap
    inline def ceil:  kse.maths.DoubleDuration.Ceil  = dt.unwrap
    inline def trunc: kse.maths.DoubleDuration.Trunc = dt.unwrap

    def pr: String =
      s"${dt.unwrap} sec"
  }

  opaque type Into = Double
  object Into {
    extension (in: Into) {
      inline def ns: Double  = in * 1e9
      inline def us: Double  = in * 1e6
      inline def ms: Double  = in * 1e3
      inline def s: Double   = in
      inline def m: Double   = in / 60.0
      inline def h: Double   = in / 3600.0
      inline def d: Double   = in / 86400.0
      inline def days: Double = in.d

      inline def round: kse.maths.DoubleDuration.InRound = in
      inline def floor: kse.maths.DoubleDuration.InFloor = in
      inline def ceil:  kse.maths.DoubleDuration.InCeil  = in
      inline def trunc: kse.maths.DoubleDuration.InTrunc = in
    }
  }

  opaque type Round = Double
  object Round {
    extension (round: Round) {
      inline def ns: kse.maths.DoubleDuration = DoubleDuration( jm.rint(round * 1e9) / 1e9 )
      inline def us: kse.maths.DoubleDuration = DoubleDuration( jm.rint(round * 1e6) / 1e6 )
      inline def ms: kse.maths.DoubleDuration = DoubleDuration( jm.rint(round * 1e3) / 1e3 )
      inline def s:  kse.maths.DoubleDuration = DoubleDuration( jm.rint(round) )
      inline def m:  kse.maths.DoubleDuration = DoubleDuration( jm.rint(round / 60) * 60 )
      inline def h:  kse.maths.DoubleDuration = DoubleDuration( jm.rint(round / 3600) * 3600 )
      inline def d:  kse.maths.DoubleDuration = DoubleDuration( jm.rint(round / 86400) * 86400 )
      inline def days: kse.maths.DoubleDuration = round.d

      inline def into:    kse.maths.DoubleDuration.InRound        = round
      inline def long:    kse.maths.DoubleDuration.InLongRound    = round
      inline def checked: kse.maths.DoubleDuration.CheckLongRound = round
    }
  }

  opaque type Floor = Double
  object Floor {
    extension (floor: Floor) {
      inline def ns: kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor * 1e9) / 1e9 )
      inline def us: kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor * 1e6) / 1e6 )
      inline def ms: kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor * 1e3) / 1e3 )
      inline def s:  kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor) )
      inline def m:  kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor / 60) * 60 )
      inline def h:  kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor / 3600) * 3600 )
      inline def d:  kse.maths.DoubleDuration = DoubleDuration( jm.floor(floor / 86400) * 86400 )
      inline def days: kse.maths.DoubleDuration = floor.d

      inline def into:    kse.maths.DoubleDuration.InFloor        = floor
      inline def long:    kse.maths.DoubleDuration.InLongFloor    = floor
      inline def checked: kse.maths.DoubleDuration.CheckLongFloor = floor
    }
  }

  opaque type Ceil = Double
  object Ceil {
    extension (ceil: Ceil) {
      inline def ns: kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil * 1e9) / 1e9 )
      inline def us: kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil * 1e6) / 1e6 )
      inline def ms: kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil * 1e3) / 1e3 )
      inline def s:  kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil) )
      inline def m:  kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil / 60) * 60 )
      inline def h:  kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil / 3600) * 3600 )
      inline def d:  kse.maths.DoubleDuration = DoubleDuration( jm.ceil(ceil / 86400) * 86400 )
      inline def days: kse.maths.DoubleDuration = ceil.d

      inline def into:    kse.maths.DoubleDuration.InCeil        = ceil
      inline def long:    kse.maths.DoubleDuration.InLongCeil    = ceil
      inline def checked: kse.maths.DoubleDuration.CheckLongCeil = ceil
    }
  }

  opaque type Trunc = Double
  object Trunc {
    extension (trunc: Trunc) {
      def ns: kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc * 1e9)   else jm.floor(trunc * 1e9) ) / 1e9 )
      def us: kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc * 1e6)   else jm.floor(trunc * 1e6) ) / 1e6 )
      def ms: kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc * 1e3)   else jm.floor(trunc * 1e3) ) / 1e3 )
      def s:  kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc)         else jm.floor(trunc) ) )
      def m:  kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc / 60)    else jm.floor(trunc / 60) ) * 60 )
      def h:  kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc / 3600)  else jm.floor(trunc / 3600) ) * 3600 )
      def d:  kse.maths.DoubleDuration = DoubleDuration( (if trunc < 0 then jm.ceil(trunc / 86400) else jm.floor(trunc / 86400) ) * 86400 )
      inline def days: kse.maths.DoubleDuration = trunc.d

      inline def into: kse.maths.DoubleDuration.InTrunc = trunc
    }
  }

  opaque type InRound = Double
  object InRound {
    extension (round: InRound) {
      inline def ns: Double = jm.rint(round * 1e9)
      inline def us: Double = jm.rint(round * 1e6)
      inline def ms: Double = jm.rint(round * 1e3)
      inline def s:  Double = jm.rint(round)
      inline def m:  Double = jm.rint(round / 60)
      inline def h:  Double = jm.rint(round / 3600)
      inline def d:  Double = jm.rint(round / 86400)
      inline def days: Double = round.d
    }
  }

  opaque type InFloor = Double
  object InFloor {
    extension (floor: InFloor) {
      inline def ns: Double = jm.floor(floor * 1e9)
      inline def us: Double = jm.floor(floor * 1e6)
      inline def ms: Double = jm.floor(floor * 1e3)
      inline def s:  Double = jm.floor(floor)
      inline def m:  Double = jm.floor(floor / 60)
      inline def h:  Double = jm.floor(floor / 3600)
      inline def d:  Double = jm.floor(floor / 86400)
      inline def days: Double = floor.d
    }
  }

  opaque type InCeil = Double
  object InCeil {
    extension (ceil: InCeil) {
      inline def ns: Double = jm.ceil(ceil * 1e9)
      inline def us: Double = jm.ceil(ceil * 1e6)
      inline def ms: Double = jm.ceil(ceil * 1e3)
      inline def s:  Double = jm.ceil(ceil)
      inline def m:  Double = jm.ceil(ceil / 60)
      inline def h:  Double = jm.ceil(ceil / 3600)
      inline def d:  Double = jm.ceil(ceil / 86400)
      inline def days: Double = ceil.d
    }
  }

  opaque type InTrunc = Double
  object InTrunc {
    extension (trunc: InTrunc) {
      def ns: Double = ( if trunc < 0 then jm.ceil(trunc * 1e9)   else jm.floor(trunc * 1e9) )
      def us: Double = ( if trunc < 0 then jm.ceil(trunc * 1e6)   else jm.floor(trunc * 1e6) )
      def ms: Double = ( if trunc < 0 then jm.ceil(trunc * 1e3)   else jm.floor(trunc * 1e3) )
      def s:  Double = ( if trunc < 0 then jm.ceil(trunc)         else jm.floor(trunc) )
      def m:  Double = ( if trunc < 0 then jm.ceil(trunc / 60)    else jm.floor(trunc / 60) )
      def h:  Double = ( if trunc < 0 then jm.ceil(trunc / 3600)  else jm.floor(trunc / 3600) )
      def d:  Double = ( if trunc < 0 then jm.ceil(trunc / 86400) else jm.floor(trunc / 86400) )
      inline def days: Double = trunc.d
    }
  }

  opaque type InLong = Double
  object InLong {
    extension (in: InLong) {
      inline def ns: Long  = (in * 1e9).toLong
      inline def us: Long  = (in * 1e6).toLong
      inline def ms: Long  = (in * 1e3).toLong
      inline def s: Long   = in.toLong
      inline def m: Long   = (in / 60.0).toLong
      inline def h: Long   = (in / 3600.0).toLong
      inline def d: Long   = (in / 86400.0).toLong
      inline def days: Long = in.d

      inline def round: kse.maths.DoubleDuration.InLongRound = in
      inline def floor: kse.maths.DoubleDuration.InLongFloor = in
      inline def ceil:  kse.maths.DoubleDuration.InLongCeil  = in
    }
  }

  opaque type InLongRound = Double
  object InLongRound {
    extension (in: InLongRound) {
      inline def ns: Long  = jm.rint(in * 1e9).toLong
      inline def us: Long  = jm.rint(in * 1e6).toLong
      inline def ms: Long  = jm.rint(in * 1e3).toLong
      inline def s: Long   = jm.rint(in).toLong
      inline def m: Long   = jm.rint(in / 60.0).toLong
      inline def h: Long   = jm.rint(in / 3600.0).toLong
      inline def d: Long   = jm.rint(in / 86400.0).toLong
      inline def days: Long = in.d
    }
  }

  opaque type InLongFloor = Double
  object InLongFloor {
    extension (in: InLongFloor) {
      inline def ns: Long  = jm.floor(in * 1e9).toLong
      inline def us: Long  = jm.floor(in * 1e6).toLong
      inline def ms: Long  = jm.floor(in * 1e3).toLong
      inline def s: Long   = jm.floor(in).toLong
      inline def m: Long   = jm.floor(in / 60).toLong
      inline def h: Long   = jm.floor(in / 3600).toLong
      inline def d: Long   = jm.floor(in / 86400).toLong
      inline def days: Long = in.d
    }
  }

  opaque type InLongCeil = Double
  object InLongCeil {
    extension (in: InLongCeil) {
      inline def ns: Long  = jm.ceil(in * 1e9).toLong
      inline def us: Long  = jm.ceil(in * 1e6).toLong
      inline def ms: Long  = jm.ceil(in * 1e3).toLong
      inline def s: Long   = jm.ceil(in).toLong
      inline def m: Long   = jm.ceil(in / 60.0).toLong
      inline def h: Long   = jm.ceil(in / 3600.0).toLong
      inline def d: Long   = jm.ceil(in / 86400.0).toLong
      inline def days: Long = in.d
    }
  }

  private def inRangeLong(d: Double): Long =
    if d >= Long.MinValue && d <= Long.MaxValue then d.toLong
    else throw new ArithmeticException("long overflow")

  opaque type CheckLong = Double
  object CheckLong {
    extension (in: CheckLong) {
      def ns: Long  = inRangeLong(in * 1e9)
      def us: Long  = inRangeLong(in * 1e6)
      def ms: Long  = inRangeLong(in * 1e3)
      def s: Long   = inRangeLong(in)
      def m: Long   = inRangeLong(in / 60.0)
      def h: Long   = inRangeLong(in / 3600.0)
      def d: Long   = inRangeLong(in / 86400.0)
      inline def days: Long = in.d

      inline def round: kse.maths.DoubleDuration.CheckLongRound = in
      inline def floor: kse.maths.DoubleDuration.CheckLongFloor = in
      inline def ceil:  kse.maths.DoubleDuration.CheckLongCeil  = in
    }
  }

  opaque type CheckLongRound = Double
  object CheckLongRound {
    extension (in: CheckLongRound) {
      def ns: Long  = inRangeLong( jm.rint(in * 1e9) )
      def us: Long  = inRangeLong( jm.rint(in * 1e6) )
      def ms: Long  = inRangeLong( jm.rint(in * 1e3) )
      def s: Long   = inRangeLong( jm.rint(in) )
      def m: Long   = inRangeLong( jm.rint(in / 60.0) )
      def h: Long   = inRangeLong( jm.rint(in / 3600.0) )
      def d: Long   = inRangeLong( jm.rint(in / 86400.0) )
      inline def days: Long = in.d
    }
  }

  opaque type CheckLongFloor = Double
  object CheckLongFloor {
    extension (in: CheckLongFloor) {
      def ns: Long  = inRangeLong( jm.floor(in * 1e9) )
      def us: Long  = inRangeLong( jm.floor(in * 1e6) )
      def ms: Long  = inRangeLong( jm.floor(in * 1e3) )
      def s: Long   = inRangeLong( jm.floor(in) )
      def m: Long   = inRangeLong( jm.floor(in / 60) )
      def h: Long   = inRangeLong( jm.floor(in / 3600) )
      def d: Long   = inRangeLong( jm.floor(in / 86400) )
      inline def days: Long = in.d
    }
  }

  opaque type CheckLongCeil = Double
  object CheckLongCeil {
    extension (in: CheckLongCeil) {
      def ns: Long  = inRangeLong( jm.ceil(in * 1e9) )
      def us: Long  = inRangeLong( jm.ceil(in * 1e6) )
      def ms: Long  = inRangeLong( jm.ceil(in * 1e3) )
      def s: Long   = inRangeLong( jm.ceil(in) )
      def m: Long   = inRangeLong( jm.ceil(in / 60.0) )
      def h: Long   = inRangeLong( jm.ceil(in / 3600.0) )
      def d: Long   = inRangeLong( jm.ceil(in / 86400.0) )
      inline def days: Long = in.d
    }
  }

  given Ordering[kse.maths.DoubleDuration] = new {
    def compare(dt: kse.maths.DoubleDuration, du: kse.maths.DoubleDuration) = java.lang.Double.compare(dt.unwrap, du.unwrap)
  }
}



object TemporalCompanion {
  val FileTimeMax = FileTime.from(Long.MaxValue, TimeUnit.DAYS)
  val FileTimeMin = FileTime.from(Long.MinValue, TimeUnit.DAYS)

  def toDouble(instant: Instant): kse.maths.DoubleInstant = DoubleInstant(instant)
  def toDouble(datetime: LocalDateTime): kse.maths.DoubleInstant = toDouble(datetime.atZone(ZoneId.systemDefault))
  def toDouble(datetime: OffsetDateTime): kse.maths.DoubleInstant = DoubleInstant.fromSeconds(datetime.toEpochSecond, datetime.getNano)
  def toDouble(datetime: ZonedDateTime): kse.maths.DoubleInstant = DoubleInstant.fromSeconds(datetime.toEpochSecond, datetime.getNano)

  def toInstant(datetime: LocalDateTime): Instant = datetime.atZone(ZoneId.systemDefault).toInstant

  def toLocal(instant: Instant): LocalDateTime = instant.atZone(ZoneId.systemDefault).toLocalDateTime
  def toLocal(datetime: ZonedDateTime): LocalDateTime = datetime.withZoneSameInstant(ZoneId.systemDefault).toLocalDateTime
  def toLocal(datetime: OffsetDateTime): LocalDateTime = datetime.atZoneSameInstant(ZoneId.systemDefault).toLocalDateTime

  def toUTC(instant: Instant): OffsetDateTime = instant.atOffset(ZoneOffset.UTC)
  def toUTC(datetime: LocalDateTime): OffsetDateTime = datetime.atZone(ZoneId.systemDefault).toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)
  def toUTC(datetime: ZonedDateTime): OffsetDateTime = datetime.toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)

  def toZoned(instant: Instant): ZonedDateTime = instant.atZone(ZoneId.systemDefault)
  def toZoned(datetime: LocalDateTime): ZonedDateTime = datetime.atZone(ZoneId.systemDefault)
  def toZoned(datetime: OffsetDateTime): ZonedDateTime = datetime.atZoneSameInstant(ZoneId.systemDefault)

  def toOffset(instant: Instant): OffsetDateTime = toZoned(instant).toOffsetDateTime
  def toOffset(datetime: LocalDateTime): OffsetDateTime = toZoned(datetime).toOffsetDateTime
}


/*
extension (i: Instant) {
  /*
  inline def double: kse.maths.DoubleInstant = DoubleInstant(i)
  inline def local: LocalDateTime            = TemporalCompanion.toLocal(i)
  inline def offset: OffsetDateTime          = TemporalCompanion.toOffset(i)
  inline def utc: OffsetDateTime             = TemporalCompanion.toUTC(i)
  inline def zoned: ZonedDateTime            = TemporalCompanion.toZoned(i)
  inline def filetime: FileTime              = TemporalCompanion.toFileTime(i)
  */

  // +(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
}
*/

opaque type NanoInstant = Long
object NanoInstant {
  inline def apply(nanos: Long): kse.maths.NanoInstant = nanos
  inline def now: kse.maths.NanoInstant = System.nanoTime

  final val MinValue: kse.maths.NanoInstant = apply(Long.MinValue)
  final val MaxValue: kse.maths.NanoInstant = apply(Long.MaxValue)

  extension (nt: NanoInstant) {
    inline def unwrap: Long = nt
  }

  extension (nt: kse.maths.NanoInstant) {
    inline def +(dt: kse.maths.NanoDuration): kse.maths.NanoInstant = NanoInstant(nt.unwrap + dt.unwrap)

    @targetName("nanoinst_sub_nanodur")
    inline def -(dt: kse.maths.NanoDuration): kse.maths.NanoInstant = NanoInstant(nt.unwrap - dt.unwrap)

    @targetName("nanoinst_subxcl_nanoinst")
    inline def -(mt: kse.maths.NanoInstant): kse.maths.NanoDuration = NanoDuration(nt.unwrap - mt.unwrap)

    inline def to(mt: kse.maths.NanoInstant): kse.maths.NanoDuration = NanoDuration(mt.unwrap - nt.unwrap)

    inline def <( mt: kse.maths.NanoInstant): Boolean = nt.unwrap - mt.unwrap < 0
    inline def <=(mt: kse.maths.NanoInstant): Boolean = nt.unwrap - mt.unwrap <= 0
    inline def >=(mt: kse.maths.NanoInstant): Boolean = nt.unwrap - mt.unwrap >= 0
    inline def >( mt: kse.maths.NanoInstant): Boolean = nt.unwrap - mt.unwrap > 0

    inline def max(mt: kse.maths.NanoInstant): kse.maths.NanoInstant = if nt.unwrap - mt.unwrap < 0 then mt else nt
    inline def min(mt: kse.maths.NanoInstant): kse.maths.NanoInstant = if nt.unwrap - mt.unwrap > 0 then mt else nt

    def clamp(lo: kse.maths.NanoInstant, hi: kse.maths.NanoInstant): kse.maths.NanoInstant =
      if lo.unwrap - nt.unwrap <= 0 then
        if nt.unwrap - hi.unwrap <= 0 then nt
        else if lo.unwrap - hi.unwrap <= 0 then hi
        else lo
      else lo

    def in(lo: kse.maths.NanoInstant, hi: kse.maths.NanoInstant): Boolean =
      lo.unwrap - nt.unwrap <= 0 && nt.unwrap - hi.unwrap <= 0

    def checkIn(lo: kse.maths.NanoInstant, hi: kse.maths.NanoInstant): kse.maths.NanoInstant =
      if in(lo, hi) then nt else throw new ArithmeticException("NanoInstant out of bounds")

    inline def age: kse.maths.NanoDuration = NanoDuration(System.nanoTime - nt.unwrap)

    def pr: String =
      s"nanotime=${nt.unwrap}"
  }

  given Ordering[NanoInstant] = new {
    def compare(a: NanoInstant, b: NanoInstant) =
      val diff = a.unwrap - b.unwrap
      if diff > 0 then 1 else (diff >> 63).toInt
  }
}



opaque type DoubleInstant = Double
object DoubleInstant {
  val MaxInstantDouble = Instant.MAX.getEpochSecond.toDouble
  val MinInstantDouble = Instant.MIN.getEpochSecond.toDouble

  val MaxFileTimeDouble = Long.MaxValue.toDouble * 86400
  val MinFileTimeDouble = Long.MinValue.toDouble * 86400

  inline def apply(t: Double): kse.maths.DoubleInstant = t
  inline def apply(i: Instant): kse.maths.DoubleInstant = fromSeconds(i.getEpochSecond, i.getNano)

  def fromSeconds(seconds: Long, nanos: Int = 0): kse.maths.DoubleInstant =
    if nanos == 0 then seconds.toDouble
    else if nanos % 1000000 == 0 then
      if jm.abs(seconds) <= 9223372036854775L then (1000*seconds + nanos/1000000)/1e3 else seconds + nanos/1e9
    else if nanos % 1000 == 0 then
      if jm.abs(seconds) <= 9223372036854L then (1000000*seconds + nanos/1000)/1e6 else seconds + nanos/1e9
    else
      if jm.abs(seconds) <= 9223372036L then (1000000000*seconds + nanos)/1e9 else seconds + nanos/1e9

  def fromDays(days: Long, extraNanos: Long = 0L): kse.maths.DoubleInstant =
    if extraNanos == 0 then
      if jm.abs(days) <= 106751991167300L then (days * 86400).toDouble else days.toDouble * 86400
    else if extraNanos % 1000000 == 0 then
      if jm.abs(days) <= 106751991167L then (86400000*days + extraNanos/1000000)/1e3 else days*86400.0 + extraNanos/1e9
    else if extraNanos % 1000 == 0 then
      if jm.abs(days) <= 106751991L then (86400000000L*days + extraNanos/1000)/1e6 else days*86400.0 + extraNanos/1e9
    else if jm.abs(days) <= 106751L then (86400000000000L*days + extraNanos)/1e9 else days*860400.0 + extraNanos/1e9

  inline def now: kse.maths.DoubleInstant = apply(Instant.now)

  extension (t: DoubleInstant) {
    inline def unwrap: Double = t
  }

  extension (t: kse.maths.DoubleInstant) {
    // SCALABUG - not inline because it confuses the opaque types
    def +(dt: kse.maths.DoubleDuration): kse.maths.DoubleInstant = DoubleInstant(t.unwrap + dt.unwrap)

    // SCALABUG - not inline because it confuses the opaque types
    @targetName("instant_sub_duration")
    def -(dt: kse.maths.DoubleDuration): kse.maths.DoubleInstant = DoubleInstant(t.unwrap - dt.unwrap)

    @targetName("instant_sub_instant")
    inline def -(u: kse.maths.DoubleInstant): kse.maths.DoubleDuration = DoubleDuration(t.unwrap - u.unwrap)

    inline def to(u: kse.maths.DoubleInstant): kse.maths.DoubleDuration = DoubleDuration(u.unwrap - t.unwrap)

    def <( u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) < 0
    def <=(u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) <= 0
    def >=(u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) >= 0
    def >( u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) > 0

    inline def max(u: kse.maths.DoubleInstant): kse.maths.DoubleInstant = if DoubleInstant.<(t)(u) then u else t
    inline def min(u: kse.maths.DoubleInstant): kse.maths.DoubleInstant = if DoubleInstant.>(t)(u) then u else t

    def clamp(lo: kse.maths.DoubleInstant, hi: kse.maths.DoubleInstant): kse.maths.DoubleInstant =
      DoubleInstant(kse.maths.clamp(t.unwrap)(lo.unwrap, hi.unwrap))

    def in(lo: kse.maths.DoubleInstant, hi: kse.maths.DoubleInstant): Boolean = kse.maths.in(t.unwrap)(lo.unwrap, hi.unwrap)

    def checkIn(lo: kse.maths.DoubleInstant, hi: kse.maths.DoubleInstant): kse.maths.DoubleInstant =
      DoubleInstant(kse.maths.checkIn(t.unwrap)(lo.unwrap, hi.unwrap))

    def age: kse.maths.DoubleDuration = DoubleDuration(DoubleInstant(Instant.now).unwrap - t)

    def instant: Instant =
      val b = t.unwrap
      if b.nan || b >= MaxInstantDouble then Instant.MAX
      else if     b <= MinInstantDouble then Instant.MIN
      else
        val u = jm.ulp(b)
        if u <= 0.25e-1 then
          val s = kse.maths.trunc(b).toLong
          if u <= 0.25e-6 then
            // Microseconds--we won't go smaller than that
            Instant.ofEpochSecond(s, jm.rint((b - s)*1e6).toInt * 1000)
          else if u <= 0.25e-3 then
            // Milliseconds or smaller
            if u <= 0.25e-4 then
              if u <= 0.25e-5 then
                Instant.ofEpochSecond(s, jm.rint((b - s)*1e5).toInt * 10000)
              else
                Instant.ofEpochSecond(s, jm.rint((b - s)*1e4).toInt * 100000)
            else
              // Actually milliseconds
              Instant.ofEpochMilli(jm.rint(b*1e3).toLong)
          else
            if u <= 0.25e-2 then
              Instant.ofEpochSecond(s, jm.rint((b - s)*1e2).toInt * 10000000)
            else
              Instant.ofEpochSecond(s, jm.rint((b - s)*1e1).toInt * 100000000)
        else
          // Seconds or bigger
          var s = jm.rint(b).toLong
          if u <= 0.25 then Instant.ofEpochSecond(s)
          else
            // Values for hours and days are unnecessary since they won't fit in Instant anyway
            val mod = if u <= 2.5 then 10 else 60
            println(s"$u $mod ${s - (s % mod)} ${MaxInstantDouble}")
            Instant.ofEpochSecond(s - (s % mod))

    def filetime: FileTime =
      val b = t.unwrap
      if b.nan || b >= MaxFileTimeDouble then TemporalCompanion.FileTimeMax
      else if     b <= MinFileTimeDouble then TemporalCompanion.FileTimeMin
      else
        if b > MinInstantDouble && b < MaxInstantDouble then FileTime.from(DoubleInstant.instant(t))
        else
          val u = jm.ulp(b)
          if      u <= 15  then FileTime.from((b/60).toLong, TimeUnit.MINUTES)
          else if u <= 900 then FileTime.from((b/3600).toLong, TimeUnit.HOURS)
          else                  FileTime.from((b/86400).toLong, TimeUnit.DAYS)

    inline def local: LocalDateTime   = TemporalCompanion.toLocal( DoubleInstant.instant(t))
    inline def offset: OffsetDateTime = TemporalCompanion.toOffset(DoubleInstant.instant(t))
    inline def utc: OffsetDateTime    = TemporalCompanion.toUTC(   DoubleInstant.instant(t))
    inline def zoned: ZonedDateTime   = TemporalCompanion.toZoned( DoubleInstant.instant(t))

    def trunc: kse.maths.DoubleInstant.Trunc = t.unwrap
    def floor: kse.maths.DoubleInstant.Floor = t.unwrap
    def round: kse.maths.DoubleInstant.Round = t.unwrap
    def ceil:  kse.maths.DoubleInstant.Ceil  = t.unwrap

    def pr: String = s"epoch + ${t.unwrap} sec"
  }

  opaque type Trunc = Double
  object Trunc {
    extension (trunc: Trunc) {
      def us: kse.maths.DoubleInstant = DoubleInstant( (if trunc < 0 then jm.ceil(trunc * 1e6)   else jm.floor(trunc * 1e6) ) / 1e6 )
      def ms: kse.maths.DoubleInstant = DoubleInstant( (if trunc < 0 then jm.ceil(trunc * 1e3)   else jm.floor(trunc * 1e3) ) / 1e3 )
      def s:  kse.maths.DoubleInstant = DoubleInstant( (if trunc < 0 then jm.ceil(trunc)         else jm.floor(trunc) ) )
      def m:  kse.maths.DoubleInstant = DoubleInstant( (if trunc < 0 then jm.ceil(trunc / 60)    else jm.floor(trunc / 60) ) * 60 )
      def h:  kse.maths.DoubleInstant = DoubleInstant( (if trunc < 0 then jm.ceil(trunc / 3600)  else jm.floor(trunc / 3600) ) * 3600 )
      def d:  kse.maths.DoubleInstant = DoubleInstant( (if trunc < 0 then jm.ceil(trunc / 86400) else jm.floor(trunc / 86400) ) * 86400 )
      inline def days: kse.maths.DoubleInstant = trunc.d
    }
  }

  opaque type Floor = Double
  object Floor {
    extension (floor: Floor) {
      inline def us: kse.maths.DoubleInstant = DoubleInstant( jm.floor(floor * 1e6) / 1e6 )
      inline def ms: kse.maths.DoubleInstant = DoubleInstant( jm.floor(floor * 1e3) / 1e3 )
      inline def s:  kse.maths.DoubleInstant = DoubleInstant( jm.floor(floor) )
      inline def m:  kse.maths.DoubleInstant = DoubleInstant( jm.floor(floor / 60) * 60 )
      inline def h:  kse.maths.DoubleInstant = DoubleInstant( jm.floor(floor / 3600) * 3600 )
      inline def d:  kse.maths.DoubleInstant = DoubleInstant( jm.floor(floor / 86400) * 86400 )
      inline def days: kse.maths.DoubleInstant = floor.d
    }
  }

  opaque type Round = Double
  object Round {
    extension (round: Round) {
      inline def us: kse.maths.DoubleInstant = DoubleInstant( jm.rint(round * 1e6) / 1e6 )
      inline def ms: kse.maths.DoubleInstant = DoubleInstant( jm.rint(round * 1e3) / 1e3 )
      inline def s:  kse.maths.DoubleInstant = DoubleInstant( jm.rint(round) )
      inline def m:  kse.maths.DoubleInstant = DoubleInstant( jm.rint(round / 60) * 60 )
      inline def h:  kse.maths.DoubleInstant = DoubleInstant( jm.rint(round / 3600) * 3600 )
      inline def d:  kse.maths.DoubleInstant = DoubleInstant( jm.rint(round / 86400) * 86400 )
      inline def days: kse.maths.DoubleInstant = round.d
    }
  }

  opaque type Ceil = Double
  object Ceil {
    extension (ceil: Ceil) {
      inline def us: kse.maths.DoubleInstant = DoubleInstant( jm.ceil(ceil * 1e6) / 1e6 )
      inline def ms: kse.maths.DoubleInstant = DoubleInstant( jm.ceil(ceil * 1e3) / 1e3 )
      inline def s:  kse.maths.DoubleInstant = DoubleInstant( jm.ceil(ceil) )
      inline def m:  kse.maths.DoubleInstant = DoubleInstant( jm.ceil(ceil / 60) * 60 )
      inline def h:  kse.maths.DoubleInstant = DoubleInstant( jm.ceil(ceil / 3600) * 3600 )
      inline def d:  kse.maths.DoubleInstant = DoubleInstant( jm.ceil(ceil / 86400) * 86400 )
      inline def days: kse.maths.DoubleInstant = ceil.d
    }
  }

  given Ordering[kse.maths.DoubleInstant] = new {
    def compare(t: kse.maths.DoubleInstant, u: kse.maths.DoubleInstant) = java.lang.Double.compare(t.unwrap, u.unwrap)
  }
}


/*
extension (instant: Instant) {
  inline def double: kse.maths.DoubleInstant = DoubleInstant(instant)
  inline def local: LocalDateTime            = TemporalCompanion.toLocal(instant)
  inline def offset: OffsetDateTime          = TemporalCompanion.toOffset(instant)
  inline def utc: OffsetDateTime             = TemporalCompanion.toUTC(instant)
  inline def zoned: ZonedDateTime            = TemporalCompanion.toZoned(instant)

  inline def +(duration: Duration): Instant = instant plus duration

  inline def -(inst: Instant): Duration = Duration.between(inst, instant)

  inline def to(inst: Instant): Duration = Duration.between(instant,inst)

  inline def <( inst: Instant): Boolean = instant.compareTo(inst) < 0
  inline def <=(inst: Instant): Boolean = instant.compareTo(inst) <= 0
  inline def >=(inst: Instant): Boolean = instant.compareTo(inst) >= 0
  inline def >( inst: Instant): Boolean = instant.compareTo(inst) > 0

  inline def max(inst: Instant): Instant = if instant.compareTo(inst) < 0 then inst else instant
  inline def min(inst: Instant): Instant = if instant.compareTo(inst) > 0 then inst else instant
}

extension (zoned: ZonedDateTime) {
  inline def double: kse.maths.DoubleInstant = TemporalCompanion.toDouble(zoned)
  inline def instant: Instant                = zoned.toInstant
  inline def local: LocalDateTime            = TemporalCompanion.toLocal(zoned)
  inline def offset: OffsetDateTime          = zoned.toOffsetDateTime
  inline def utc: OffsetDateTime             = TemporalCompanion.toUTC(zoned)

  inline def +(duration: Duration): ZonedDateTime = zoned plus duration

  inline def -(z: ZonedDateTime): DeltaDateTime = DeltaDateTime.between(z, zoned)

  inline def to(z: ZonedDateTime): DeltaDateTime = DeltaDateTime.between(zoned, z)

  inline def <( z: ZonedDateTime): Boolean = zoned.compareTo(z) < 0
  inline def <=(z: ZonedDateTime): Boolean = zoned.compareTo(z) <= 0
  inline def >=(z: ZonedDateTime): Boolean = zoned.compareTo(z) >= 0
  inline def >( z: ZonedDateTime): Boolean = zoned.compareTo(z) > 0

  inline def max(z: ZonedDateTime): ZonedDateTime = if zoned.compareTo(z) < 0 then z else zoned
  inline def min(z: ZonedDateTime): ZonedDateTime = if zoned.compareTo(z) > 0 then z else zoned
}
*/

class DeltaDateTime private (val years: Int, val months: Int, val days: Int, val seconds: Int, val nanos: Int) {
  override def toString =
    val b = new java.lang.StringBuilder
    if years != 0 then
      if years > 0 then b append '+'
      b append years
      b append 'Y'
    if months != 0 then
      if months > 0 then b append '+'
      b append months
      b append 'M'
    if days != 0 then
      if days > 0 then b append '+'
      b append days
      b append 'D'
    if seconds != 0 || nanos != 0 then
      var v = seconds
      val h = v/3600
      if h != 0 then
        if h > 0 then b append '+'
        b append h
        b append 'h'
      v = v - 3600 * h
      val m = v/60
      if m != 0 then
        if m > 0 then b append '+'
        b append m
        b append 'm'
      v = v - 60 * m
      if v != 0 || nanos != 0 then
        if v >= 0 then b append '+'
        b append v
        b append 's'
        if nanos % 1000 != 0 then
          if v > 0 then b append '+'
          b append nanos
          b append "ns"
        else if nanos % 1000_000 != 0 then
          if nanos > 0 then b append '+'
          b append nanos/1000
          b append "us"
        else
          if nanos > 0 then b append '+'
          b append nanos/1000000
          b append "ms"
    if b.length == 0 then b append "+0s"
    b.toString
}
object DeltaDateTime {
  def apply(yr: Int, mo: Int, d: Int, h: Int, m: Int, s: Int, ns: Int) =
    val mm = yr.toLong*12 + mo
    var ss = d.toLong*86400 + h.toLong*3600 + m.toLong*60 + s.toLong + (ns/1000000000)
    val nn = ns % 1000000000
    new DeltaDateTime((mm / 12).toInt, (mm % 12).toInt, (ss/86400).toInt, (ss % 86400).toInt, nn)

  def between(a: ZonedDateTime, b: ZonedDateTime): DeltaDateTime = ???
}
/*
class DoubleAsDeltaT private (val time: Double) extends AnyVal {
  def <(that: DoubleAsDeltaT) = time < that.time
  def <=(that: DoubleAsDeltaT) = time <= that.time
  def >=(that: DoubleAsDeltaT) = time >= that.time
  def >(that: DoubleAsDeltaT) = time > that.time
  def abs = if (time < 0) new DoubleAsDeltaT(-time) else this
  def max(that: DoubleAsDeltaT) =
    if      (time <  that.time) that
    else if (time >  that.time) this
    else if (time == that.time) this
    else if (time.isNaN) this
    else that
  def min(that: DoubleAsDeltaT) =
    if      (time <  that.time) this
    else if (time >  that.time) that
    else if (time == that.time) this
    else if (time.isNaN) this
    else that
  def unary_- = new DoubleAsDeltaT(-time)
  def +(that: DoubleAsDeltaT) = new DoubleAsDeltaT(time + that.time)
  def -(that: DoubleAsDeltaT) = new DoubleAsDeltaT(time - that.time)
  def *(factor: Double) = DoubleAsDeltaT.apply(factor * time)
  def /(factor: Double) = DoubleAsDeltaT.apply(time / factor)
  def timeTo(t: Double) = DoubleAsDeltaT.apply(t)
  def timeFn(f: Double => Double) = DoubleAsDeltaT.apply(f(time))
  def floorMillis = new DoubleAsDeltaT(math.floor(time * 1e3) / 1e3)
  def floorSecs = new DoubleAsDeltaT(math.floor(time))
  def floorMins = new DoubleAsDeltaT(math.floor(time / 60) * 60)
  def floorHours = new DoubleAsDeltaT(math.floor(time / 3600) * 3600)
  def floorDays = new DoubleAsDeltaT(math.floor(time / 86400) * 86400)
  def roundMillis = new DoubleAsDeltaT(math.rint(time * 1e3) / 1e3)
  def roundSecs = new DoubleAsDeltaT(math.rint(time))
  def roundMins = new DoubleAsDeltaT(math.rint(time / 60) * 60)
  def roundHours = new DoubleAsDeltaT(math.rint(time / 3600) * 3600)
  def roundDays = new DoubleAsDeltaT(math.rint(time / 86400) * 86400)
  def toNanos = math.rint(time * 1e9).toLong
  def toMillis = math.rint(time * 1e3).toLong
  def toSecs = math.rint(time).toLong
  def toMins = math.rint(time / 60).toLong
  def toHours = math.rint(time / 3600).toLong
  def toDays = math.rint(time / 86400).toLong
  def duration = {
    val t = math.abs(time)
    if (t < 9e9) Duration.ofNanos(math.rint(time * 1e9).toLong)
    else if (t < 9e15) Duration.ofMillis(math.rint(time * 1e3).toLong)
    else if (t < 9e18) Duration.ofSeconds(math.rint(time).toLong)
    else if (t < 54e19) Duration.ofMinutes(math.rint(time/60).toLong)
    else if (t < 324e20) Duration.ofHours(math.rint(time/3600).toLong)
    else Duration.ofDays(math.rint(time/86400).toLong)
  }
  override def toString = time.toString + " sec"
  def =~=(that: DoubleAsDeltaT) = {
    val t = math.max(math.abs(time), math.abs(time))
    if (t < 11259e2)                         time == that.time
    else if (t < 11259e5)   math.rint(time*1e6)   == math.rint(that.time*1e6)
    else if (t < 11259e8)   math.rint(time*1e3)   == math.rint(that.time*1e3)
    else if (t < 11259e11)  math.rint(time)       == math.rint(that.time)
    else if (t < 67554e12)  math.rint(time/60)    == math.rint(that.time/60)
    else if (t < 405324e13) math.rint(time/3600)  == math.rint(that.time/3600)
    else                    math.rint(time/86400) == math.rint(that.time/86400)
  }
}
object DoubleAsDeltaT {
  given Ordering[DoubleAsDeltaT] = new {
    def compare(a: DoubleAsDeltaT, b: DoubleAsDeltaT) = if (a.=~=(b)) 0 else java.lang.Double.compare(a.time, b.time)
  }

  def apply(time: Double) = new DoubleAsDeltaT(math.rint(time*1e9)/1e9)
  def apply(time: Duration) = new DoubleAsDeltaT(time.getSeconds.toDouble + time.getNano/1e9)
}

class DoubleAsEpoch private (val time: Double) extends AnyVal {
  def <(that: DoubleAsEpoch) = time < that.time
  def <=(that: DoubleAsEpoch) = time <= that.time
  def >=(that: DoubleAsEpoch) = time >= that.time
  def >(that: DoubleAsEpoch) = time > that.time
  def max(that: DoubleAsEpoch) =
    if      (time <  that.time) that
    else if (time >  that.time) this
    else if (time == that.time) this
    else if (time.isNaN) this
    else that
  def min(that: DoubleAsEpoch) =
    if      (time <  that.time) this
    else if (time >  that.time) that
    else if (time == that.time) this
    else if (time.isNaN) this
    else that
  def +(that: DoubleAsDeltaT) = new DoubleAsEpoch(time + that.time)
  def +(that: Duration) = new DoubleAsEpoch(time + that.getSeconds + that.getNano/1e9)
  def -(that: DoubleAsDeltaT) = new DoubleAsEpoch(time - that.time)
  def -(that: Duration) = DoubleAsDeltaT.apply(time - (that.getSeconds.toDouble + that.getNano/1e9))
  def timeTo(t: Double) = DoubleAsEpoch.apply(t)
  def timeFn(f: Double => Double) = DoubleAsEpoch.apply(f(time))
  def floorMillis = new DoubleAsEpoch(math.floor(time * 1e3) / 1e3)
  def floorSecs = new DoubleAsEpoch(math.floor(time))
  def floorMins = new DoubleAsEpoch(math.floor(time / 60) * 60)
  def floorHours = new DoubleAsEpoch(math.floor(time / 3600) * 3600)
  def floorDays = new DoubleAsEpoch(math.floor(time / 86400) * 86400)
  def roundMillis = new DoubleAsEpoch(math.rint(time * 1e3) / 1e3)
  def roundSecs = new DoubleAsEpoch(math.rint(time))
  def roundMins = new DoubleAsEpoch(math.rint(time / 60) * 60)
  def roundHours = new DoubleAsEpoch(math.rint(time / 3600) * 3600)
  def roundDays = new DoubleAsEpoch(math.rint(time / 86400) * 86400)
  def toNanos = math.rint(time * 1e9).toLong
  def toMillis = math.rint(time * 1e3).toLong
  def toSecs = math.rint(time).toLong
  def toMins = math.rint(time / 60).toLong
  def toHours = math.rint(time / 3600).toLong
  def toDays = math.rint(time / 86400).toLong
  def instant = {
    val millis = time * 1e3
    if (math.abs(millis) < Long.MaxValue) Instant.ofEpochMilli(math.rint(millis).toLong)
    else if (time > DoubleAsEpoch.InstantMaxEpochSeconds) Instant.MAX
    else if (time < DoubleAsEpoch.InstantMinEpochSeconds) Instant.MIN
    else Instant.ofEpochSecond(math.rint(time).toLong)
  }
  def local = TemporalMaths.toLocal(instant)
  def utc = TemporalMaths.toUTC(instant)
  def offset = TemporalMaths.toOffset(instant)
  def zoned = TemporalMaths.toZoned(instant)
  def filetime =
    if (math.abs(time) < 9e15) FileTime.fromMillis(math.rint(time * 1e3).toLong)
    else FileTime.from(math.rint(time).toLong, java.util.concurrent.TimeUnit.SECONDS)
  override def toString = "1970 + " + time.toString + " sec"
  def =~=(that: DoubleAsEpoch) = {
    val t = math.max(math.abs(time), math.abs(time))
    if (t < 11259e2)                         time == that.time
    else if (t < 11259e5)   math.rint(time*1e6)   == math.rint(that.time*1e6)
    else if (t < 11259e8)   math.rint(time*1e3)   == math.rint(that.time*1e3)
    else if (t < 11259e11)  math.rint(time)       == math.rint(that.time)
    else if (t < 67554e12)  math.rint(time/60)    == math.rint(that.time/60)
    else if (t < 405324e13) math.rint(time/3600)  == math.rint(that.time/3600)
    else                    math.rint(time/86400) == math.rint(that.time/86400)
  }
}
object DoubleAsEpoch {
  val InstantMaxEpochSeconds = Instant.MAX.getEpochSecond
  val InstantMinEpochSeconds = Instant.MIN.getEpochSecond

  given Ordering[DoubleAsEpoch] = new {
    def compare(a: DoubleAsEpoch, b: DoubleAsEpoch) = if (a.=~=(b)) 0 else java.lang.Double.compare(a.time, b.time)
  }

  def apply(seconds: Long, nanos: Int): DoubleAsEpoch = new DoubleAsEpoch(seconds.toDouble + nanos/1e-9)
  def apply(epoch: Double): DoubleAsEpoch = new DoubleAsEpoch(math.rint(epoch*1e9)/1e9)
  def apply(instant: Instant): DoubleAsEpoch = apply(instant.getEpochSecond, instant.getNano)
  def apply(datetime: ZonedDateTime): DoubleAsEpoch = apply(datetime.toEpochSecond, datetime.getNano)
  def apply(datetime: OffsetDateTime): DoubleAsEpoch = apply(datetime.toEpochSecond, datetime.getNano)
}

object Temporal {
  def toInstant(datetime: LocalDateTime): Instant = datetime.atZone(ZoneId.systemDefault).toInstant

  def toLocal(instant: Instant): LocalDateTime = instant.atZone(ZoneId.systemDefault).toLocalDateTime
  def toLocal(datetime: ZonedDateTime): LocalDateTime = datetime.withZoneSameInstant(ZoneId.systemDefault).toLocalDateTime
  def toLocal(datetime: OffsetDateTime): LocalDateTime = datetime.atZoneSameInstant(ZoneId.systemDefault).toLocalDateTime

  def toUTC(instant: Instant): OffsetDateTime = instant.atOffset(ZoneOffset.UTC)
  def toUTC(datetime: LocalDateTime): OffsetDateTime = datetime.atZone(ZoneId.systemDefault).toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)
  def toUTC(datetime: ZonedDateTime): OffsetDateTime = datetime.toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)

  def toZoned(instant: Instant): ZonedDateTime = instant.atZone(ZoneId.systemDefault)
  def toZoned(datetime: LocalDateTime): ZonedDateTime = datetime.atZone(ZoneId.systemDefault)
  def toZoned(datetime: OffsetDateTime): ZonedDateTime = datetime.atZoneSameInstant(ZoneId.systemDefault)

  def toOffset(instant: Instant): OffsetDateTime = toZoned(instant).toOffsetDateTime
  def toOffset(datetime: LocalDateTime): OffsetDateTime = toZoned(datetime).toOffsetDateTime

  def roundMillis(instant: Instant): Instant =
    val n = instant.getNano % 1000000
    if (n == 0) instant
    else instant.plusNanos(if (n < 500000) -n else 1000000 - n)

  def floorMillis(instant: Instant): Instant =
    val r = instant.getNano % 1000000
    if (r == 0) instant else instant.minusNanos(r)

  def roundSecs(instant: Instant): Instant =
    val n = instant.getNano
    if (n == 0) instant
    else instant.plusNanos(if (n < 500000000) -n else 1000000000 - n)

  def floorSecs(instant: Instant): Instant =
    val n = instant.getNano
    if (n == 0) instant else instant.minusNanos(n)


  def multiply(duration: Duration, factor: Double): Duration =
    val d = if (duration.getSeconds < 0) duration.negated else duration
    val f = if (factor < 0) -factor else factor
    val s = d.getSeconds * f
    var n = d.getNano * f
    var s0 = s.floor
    n = java.lang.Math.rint(n + (s - s0) * 1e9)
    while (n >= 1e9) { n -= 1e9; s0 += 1 }
    val x = Duration.ofSeconds(s0.toLong, n.toInt)
    if (factor < 0 != duration.getSeconds < 0) x.negated else x

  def divide(duration: Duration, factor: Double): Duration =
    val d = if (duration.getSeconds < 0) duration.negated else duration
    val f = if (factor < 0) -factor else factor
    val s = d.getSeconds / f
    var n = d.getNano / f
    var s0 = s.floor
    n = java.lang.Math.rint(n + (s - s0) * 1e9)
    while (n >= 1e9) { n -= 1e9; s0 += 1 }
    val x = Duration.ofSeconds(s0.toLong, n.toInt)
    if (factor < 0 != duration.getSeconds < 0) x.negated else x

  class DurationDispatcher(private val value: Long) extends AnyVal {
    def nano = Duration.ofNanos(value)
    def nanos = Duration.ofNanos(value)
    def milli = Duration.ofMillis(value)
    def millis = Duration.ofMillis(value)
    def sec = Duration.ofSeconds(value)
    def secs = Duration.ofSeconds(value)
    def min = Duration.ofMinutes(value)
    def mins = Duration.ofMinutes(value)
    def hour = Duration.ofHours(value)
    def hours = Duration.ofHours(value)
    def day = Duration.ofDays(value)
    def days = Duration.ofDays(value)
  }
}

extension (double: Double)
  def time = DoubleAsDeltaT.apply(double)
  def millis = DoubleAsDeltaT.apply(double/1e3)
  def secs = DoubleAsDeltaT.apply(double)
  def mins = DoubleAsDeltaT.apply(double)
  def hours = DoubleAsDeltaT.apply(double*3600)
  def days = DoubleAsDeltaT.apply(double*86400)
  def epoch = DoubleAsEpoch.apply(double)
  def *(that: DoubleAsDeltaT) = that * double
  def *(that: Duration) = TemporalMaths.multiply(that, double)

extension (long: Long)
  def duration = new TemporalMaths.DurationDispatcher(long)

extension (epoch: DoubleAsEpoch)
  def -(that: DoubleAsEpoch) = DoubleAsDeltaT.apply(epoch.time - that.time)

extension (duration: Duration) {
  def <(that: Duration) = duration.compareTo(that) < 0
  def <=(that: Duration) = duration.compareTo(that) <= 0
  def >=(that: Duration) = duration.compareTo(that) >= 0
  def >(that: Duration) = duration.compareTo(that) > 0
  def max(that: Duration) = if (duration.compareTo(that) < 0) that else duration
  def min(that: Duration) = if (duration.compareTo(that) > 0) that else duration
  def +(that: Duration) = duration plus that
  def -(that: Duration) = duration minus that
  def unary_- = duration.negated
  def *(factor: Double) = TemporalMaths.multiply(duration, factor)
  def /(factor: Double) = TemporalMaths.divide(duration, factor)

  def floorMillis =
    val n = duration.getNano % 1000000
    if (n == 0) duration else duration.minusNanos(n)

  def floorSecs =
    val n = duration.getNano
    if (n == 0) duration else duration.minusNanos(n)

  def floorMins =
    val n = duration.getNano + 1000000000L*(duration.getSeconds % 60)
    if (n == 0) duration else if (n > 0) duration.minusNanos(n) else duration.minusNanos(60000000000L - n)

  def floorHours =
    val n = duration.getNano + 1000000000L*(duration.getSeconds % 3600)
    if (n == 0) duration else if (n > 0) duration.minusNanos(n) else duration.minusNanos(3600000000000L - n)

  def floorDays =
    val n = duration.getNano + 1000000000L*(duration.getSeconds % 86400)
    if (n == 0) duration else if (n > 0) duration.minusNanos(n) else duration.minusNanos(86400000000000L - n)

  def roundMillis =
    val n = duration.getNano % 1000000
    if (n == 0) duration else if (n < 500000) duration.minusNanos(n) else duration.plusNanos(1000000 - n)

  def roundSecs =
    val n = duration.getNano
    if (n == 0) duration else if (n < 500000000) duration.minusNanos(n) else duration.plusNanos(1000000000-n)

  def roundMins =
    val n = duration.getNano + 1000000000L*(duration.getSeconds % 60)
    if (n == 0)     duration
    else if (n > 0) { if (n <  30000000000L) duration.minusNanos(n) else duration.plusNanos(60000000000L - n) }
    else            { if (n > -30000000000L) duration.minusNanos(n) else duration.plusNanos(-60000000000L - n) }

  def roundHours =
    val n = duration.getNano + 1000000000L*(duration.getSeconds % 3600)
    if (n == 0)     duration
    else if (n > 0) { if (n <  1800000000000L) duration.minusNanos(n) else duration.plusNanos(3600000000000L - n) }
    else            { if (n > -1800000000000L) duration.minusNanos(n) else duration.plusNanos(-3600000000000L - n) }

  def roundDays =
    val n = duration.getNano + 1000000000L*(duration.getSeconds % 86400)
    if (n == 0)     duration
    else if (n > 0) { if (n <  43200000000000L) duration.minusNanos(n) else duration.plusNanos(86400000000000L - n) }
    else            { if (n > -43200000000000L) duration.minusNanos(n) else duration.plusNanos(-86400000000000L - n) }

  def double = DoubleAsDeltaT(duration)
}

extension (instant: Instant) {
  def <(when: Instant)  = instant isBefore when
  def <=(when: Instant) = !(when isBefore instant)
  def >=(when: Instant) = !(instant isBefore when)
  def >(when: Instant)  = when isBefore instant
  def <(when: FileTime)  = { val j = when.toInstant; if (j==Instant.MAX || j==Instant.MIN) FileTime.from(instant).compareTo(when) < 0  else instant isBefore j }
  def <=(when: FileTime) = { val j = when.toInstant; if (j==Instant.MAX || j==Instant.MIN) FileTime.from(instant).compareTo(when) <= 0 else !(j isBefore instant) }
  def >=(when: FileTime) = { val j = when.toInstant; if (j==Instant.MAX || j==Instant.MIN) FileTime.from(instant).compareTo(when) >= 0 else !(instant isBefore j) }
  def >(when: FileTime)  = { val j = when.toInstant; if (j==Instant.MAX || j==Instant.MIN) FileTime.from(instant).compareTo(when) > 0  else j isBefore instant }
  def min(when: Instant) = if (when isBefore instant) when else instant
  def max(when: Instant) = if (when isAfter instant) when else instant
  def +(that: Duration) = instant plus that
  def +(that: DoubleAsDeltaT) = instant plus that.duration
  def -(that: Duration) = instant minus that
  def -(that: DoubleAsDeltaT) = instant minus that.duration
  def -(when: Instant) = Duration.between(when, instant)

  def roundMillis = TemporalMaths.roundMillis(instant)
  def floorMillis = TemporalMaths.floorMillis(instant)
  def roundSecs = TemporalMaths.roundSecs(instant)
  def floorSecs = TemporalMaths.floorSecs(instant)

  def epoch = DoubleAsEpoch.apply(instant)
  def local = TemporalMaths.toLocal(instant)
  def utc = TemporalMaths.toUTC(instant)
  def offset = TemporalMaths.toOffset(instant)
  def zoned = TemporalMaths.toZoned(instant)
  def filetime = FileTime from instant
}

extension (datetime: LocalDateTime) {
  def <(when: LocalDateTime) = datetime isBefore when
  def <=(when: LocalDateTime) = !(when isBefore datetime)
  def >=(when: LocalDateTime) = !(datetime isBefore when)
  def >(when: LocalDateTime) = when isBefore datetime
  def min(when: LocalDateTime) = if (when isBefore datetime) when else datetime
  def max(when: LocalDateTime) = if (when isAfter datetime) when else datetime
  def +(that: Duration) = datetime plus that
  def +(that: DoubleAsDeltaT) = datetime plus that.duration
  def -(that: Duration) = datetime minus that
  def -(that: DoubleAsDeltaT) = datetime minus that.duration
  def -(when: LocalDateTime) = Duration.between(when, datetime)

  def roundMillis =
    val n = datetime.getNano % 1000000
    if (n == 0) datetime
    else datetime.plusNanos(if (n < 500000) -n else 1000000 - n)
  
  def floorMillis =
    val r = datetime.getNano % 1000000
    if (r == 0) datetime else datetime.minusNanos(r)
  
  def roundSeconds =
    val n = datetime.getNano
    if (n == 0) datetime
    else datetime.plusNanos(if (n < 500000000) -n else 1000000000 - n)
  
  def floorSeconds =
    val n = datetime.getNano
    if (n == 0) datetime else datetime.minusNanos(n)
  
  def epoch = DoubleAsEpoch(TemporalMaths.toInstant(datetime))
  def instant = TemporalMaths.toInstant(datetime)
  def utc = TemporalMaths.toUTC(datetime)
  def offset = TemporalMaths.toOffset(datetime)
  def zoned = TemporalMaths.toZoned(datetime)
  def filetime = FileTime from TemporalMaths.toInstant(datetime)
}

extension (datetime: ZonedDateTime) {
  def <(when: ZonedDateTime) = datetime isBefore when
  def <=(when: ZonedDateTime) = !(when isBefore datetime)
  def >=(when: ZonedDateTime) = !(datetime isBefore when)
  def >(when: ZonedDateTime) = when isBefore datetime
  def min(when: ZonedDateTime) = if (when isBefore datetime) when else datetime
  def max(when: ZonedDateTime) = if (when isAfter datetime) when else datetime
  def +(that: Duration) = datetime plus that
  def +(that: DoubleAsDeltaT) = datetime plus that.duration
  def -(that: Duration) = datetime minus that
  def -(that: DoubleAsDeltaT) = datetime minus that.duration
  def -(when: ZonedDateTime) = Duration.between(when, datetime)

  def roundMillis =
    val n = datetime.getNano % 1000000
    if (n == 0) datetime
    else datetime.plusNanos(if (n < 500000) -n else 1000000 - n)
  
  def floorMillis =
    val r = datetime.getNano % 1000000
    if (r == 0) datetime else datetime.minusNanos(r)
  
  def roundSeconds =
    val n = datetime.getNano
    if (n == 0) datetime
    else datetime.plusNanos(if (n < 500000000) -n else 1000000000 - n)
  
  def floorSeconds =
    val n = datetime.getNano
    if (n == 0) datetime else datetime.minusNanos(n)
  
  def epoch = DoubleAsEpoch.apply(datetime)
  def utc = TemporalMaths.toUTC(datetime)
  def offset = datetime.toOffsetDateTime
  def local = TemporalMaths.toLocal(datetime)
  def instant = datetime.toInstant
  def filetime = FileTime from datetime.toInstant
}

extension (datetime: OffsetDateTime) {
  def <(when: OffsetDateTime) = datetime isBefore when
  def <=(when: OffsetDateTime) = !(when isBefore datetime)
  def >=(when: OffsetDateTime) = !(datetime isBefore when)
  def >(when: OffsetDateTime) = when isBefore datetime
  def min(when: OffsetDateTime) = if (when isBefore datetime) when else datetime
  def max(when: OffsetDateTime) = if (when isAfter datetime) when else datetime
  def +(that: Duration) = datetime plus that
  def +(that: DoubleAsDeltaT) = datetime plus that.duration
  def -(that: Duration) = datetime minus that
  def -(that: DoubleAsDeltaT) = datetime minus that.duration
  def -(when: OffsetDateTime) = Duration.between(when, datetime)

  def roundMillis =
    val n = datetime.getNano % 1000000
    if (n == 0) datetime
    else datetime.plusNanos(if (n < 500000) -n else 1000000 - n)
  
  def floorMillis =
    val r = datetime.getNano % 1000000
    if (r == 0) datetime else datetime.minusNanos(r)
  
  def roundSeconds =
    val n = datetime.getNano
    if (n == 0) datetime
    else datetime.plusNanos(if (n < 500000000) -n else 1000000000 - n)

  def floorSeconds =
    val n = datetime.getNano
    if (n == 0) datetime else datetime.minusNanos(n)
  
  def epoch = DoubleAsEpoch.apply(datetime)
  def utc = datetime.withOffsetSameInstant(ZoneOffset.UTC)
  def zoned = TemporalMaths.toZoned(datetime)
  def local = TemporalMaths.toLocal(datetime)
  def instant = datetime.toInstant
  def filetime = FileTime from datetime.toInstant
}

extension (filetime: FileTime) {
  def <(when: FileTime)  = filetime.compareTo(when) < 0
  def <=(when: FileTime) = filetime.compareTo(when) <= 0
  def >=(when: FileTime) = filetime.compareTo(when) >= 0
  def >(when: FileTime)  = filetime.compareTo(when) > 0
  def min(when: FileTime) = if (filetime.compareTo(when) > 0) when else filetime
  def max(when: FileTime) = if (filetime.compareTo(when) < 0) when else filetime
  def +(that: Duration) = FileTime from (filetime.toInstant plus that)
  def +(that: DoubleAsDeltaT) = FileTime from (filetime.toInstant plus that.duration)
  def -(that: Duration) = FileTime from (filetime.toInstant minus that)
  def -(that: DoubleAsDeltaT) = FileTime from (filetime.toInstant minus that.duration)
  def -(when: FileTime) = Duration.between(when.toInstant, filetime.toInstant)

  def roundMillis =
    val i = filetime.toInstant
    if (i == Instant.MAX || i == Instant.MIN) filetime
    else {
      val i2 = TemporalMaths.roundMillis(i)
      if (i == i2) filetime else FileTime from i2
    }

  def floorMillis =
    val i = filetime.toInstant
    if (i == Instant.MAX || i == Instant.MIN) filetime
    else {
      val i2 = TemporalMaths.floorMillis(i)
      if (i == i2) filetime else FileTime from i2
    }

  def roundSeconds = 
    val i = filetime.toInstant
    if (i == Instant.MAX || i == Instant.MIN) filetime
    else {
      val i2 = TemporalMaths.roundSecs(i)
      if (i == i2) filetime else FileTime from i2
    }

  def floorSeconds =
    val i = filetime.toInstant
    if (i == Instant.MAX || i == Instant.MIN) filetime
    else {
      val i2 = TemporalMaths.floorSecs(i)
      if (i == i2) filetime else FileTime from i2
    }

  def epoch =
    val i = filetime.toInstant
    DoubleAsEpoch.apply(
      if (i == Instant.MAX || i == Instant.MIN) filetime.to(java.util.concurrent.TimeUnit.SECONDS).toDouble
      else ((i.getEpochSecond*1e9) + i.getNano) / 1e9
    )
  def utc = TemporalMaths.toUTC(filetime.toInstant)
  def zoned = TemporalMaths.toZoned(filetime.toInstant)
  def local = TemporalMaths.toLocal(filetime.toInstant)
  def instant = filetime.toInstant
}
*/


opaque type Tic = Long
object Tic {
  def apply(): kse.maths.Tic = NanoInstant.now.unwrap

  extension (t: Tic) {
    inline def unwrap: kse.maths.NanoInstant = NanoInstant(t: Long)
  }
  extension (t: kse.maths.Tic) {
    inline def toc: kse.maths.NanoDuration = t.unwrap.age
  }
}

inline def tic: kse.maths.Tic = Tic()
