// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020-22 Rex Kerr and Calico Life Sciences, LLC.

package kse.maths

import java.lang.{Math => jm}
import java.time._
import java.util.concurrent.TimeUnit
import java.nio.file.attribute.FileTime

import scala.annotation.targetName

import kse.maths._


// Note: Duration.between is not guaranteed to be safe on ZonedDateTime or OffsetDateTime!
// Just get epoch seconds manually and calculate.  Duration tries to shift these to a common
// offset or zone, which technically can fail.



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

    inline def D: kse.maths.DoubleDuration = DoubleDuration(dt)
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
  inline def D: kse.maths.DoubleDuration = DoubleDuration(d.getNano/1e9 + d.getSeconds)

  inline def into:    kse.maths.DurationCompanion.Into  = DurationCompanion.Into (d)
  inline def checked: kse.maths.DurationCompanion.Check = DurationCompanion.Check(d)
  inline def round:   kse.maths.DurationCompanion.Round = DurationCompanion.Round(d)
  inline def floor:   kse.maths.DurationCompanion.Floor = DurationCompanion.Floor(d)
  inline def ceil:    kse.maths.DurationCompanion.Ceil  = DurationCompanion.Ceil (d)
}




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

  def apply(ft: FileTime): kse.maths.DoubleInstant =
    val i = ft.toInstant
    if i == Instant.MAX || i == Instant.MIN then
      val s = ft to TimeUnit.SECONDS
      if s == Long.MinValue || s == Long.MaxValue then
        val m = ft to TimeUnit.MINUTES
        if m == Long.MinValue || m == Long.MaxValue then
          val h = ft to TimeUnit.HOURS
          if h == Long.MinValue || h == Long.MaxValue then
            DoubleInstant.apply((ft to TimeUnit.DAYS).toDouble * 86400)
          else DoubleInstant.apply(h.toDouble * 3600)
        else DoubleInstant.apply(m.toDouble * 60)
      else DoubleInstant.apply(s.toDouble)
    else DoubleInstant.apply(i)

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
            Instant.ofEpochSecond(s - (s % mod))
    def checkedInstant: Instant =
      val b = t.unwrap
      if b.nan || b > MaxInstantDouble || b < MinInstantDouble then throw new DateTimeException("Instant overflow")
      else instant

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
    def checkedFileTime: FileTime =
      val b = t.unwrap
      if b.nan || b > MaxFileTimeDouble || b < MinFileTimeDouble then throw new DateTimeException("FileTime overflow")
      else filetime

    def local: LocalDateTime =
      val b = t.unwrap
      if b.nan || b >= TemporalCompanion.DateTimeSuperMaxSeconds then LocalDateTime.MAX
      else if     b <= TemporalCompanion.DateTimeSuperMinSeconds then LocalDateTime.MIN
      else
        try LocalDateTime.ofInstant(DoubleInstant.instant(t), ZoneId.systemDefault)
        catch case _: DateTimeException => if b < 0 then LocalDateTime.MIN else LocalDateTime.MAX
    def checkedLocal: LocalDateTime =
      LocalDateTime.ofInstant(DoubleInstant.checkedInstant(t), ZoneId.systemDefault)

    def offset: OffsetDateTime =
      val b = t.unwrap
      if b.nan || b >= TemporalCompanion.DateTimeSuperMaxSeconds then TemporalCompanion.currentMaxOffsetDateTime
      else if     b <= TemporalCompanion.DateTimeSuperMinSeconds then TemporalCompanion.currentMinOffsetDateTime
      else
        try OffsetDateTime.ofInstant(DoubleInstant.instant(t), ZoneId.systemDefault)
        catch case _: DateTimeException =>
          if b < 0 then TemporalCompanion.currentMinOffsetDateTime
          else          TemporalCompanion.currentMaxOffsetDateTime
    def checkedOffset: OffsetDateTime =
      OffsetDateTime.ofInstant(DoubleInstant.checkedInstant(t), ZoneId.systemDefault)

    def utc: OffsetDateTime =
      val b = t.unwrap
      if b.nan || b >= TemporalCompanion.DateTimeMaxSeconds then TemporalCompanion.MaxUTCDateTime
      else if     b <= TemporalCompanion.DateTimeMinSeconds then TemporalCompanion.MinUTCDateTime
      else
        try OffsetDateTime.ofInstant(DoubleInstant.instant(t), ZoneOffset.UTC)
        catch case _: DateTimeException => if b < 0 then TemporalCompanion.MinUTCDateTime else TemporalCompanion.MaxUTCDateTime
    def checkedUTC: OffsetDateTime =
      OffsetDateTime.ofInstant(DoubleInstant.checkedInstant(t), ZoneOffset.UTC)

    def zoned: ZonedDateTime =
      val b = t.unwrap
      if b.nan || b >= TemporalCompanion.DateTimeSuperMaxSeconds then TemporalCompanion.currentMaxZonedDateTime
      else if     b <= TemporalCompanion.DateTimeSuperMinSeconds then TemporalCompanion.currentMinZonedDateTime
      else
        try ZonedDateTime.ofInstant(DoubleInstant.instant(t), ZoneId.systemDefault)
        catch case _: DateTimeException =>
          if b < 0 then TemporalCompanion.currentMinZonedDateTime
          else          TemporalCompanion.currentMaxZonedDateTime
    def checkedZoned: ZonedDateTime =
      ZonedDateTime.ofInstant(DoubleInstant.checkedInstant(t), ZoneId.systemDefault)

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



object TemporalCompanion {
  val FileTimeMax = FileTime.from(Long.MaxValue, TimeUnit.DAYS)
  val FileTimeMin = FileTime.from(Long.MinValue, TimeUnit.DAYS)

  val DateTimeMaxSeconds = LocalDateTime.MAX.toEpochSecond(ZoneOffset.UTC)
  val DateTimeMinSeconds = LocalDateTime.MIN.toEpochSecond(ZoneOffset.UTC)

  val DateTimeSuperMaxSeconds = DateTimeMaxSeconds + 16 * 3600
  val DateTimeSuperMinSeconds = DateTimeMinSeconds - 16 * 3600

  def currentMaxOffsetDateTime = OffsetDateTime.of(LocalDateTime.MAX, ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MAX))
  def currentMinOffsetDateTime = OffsetDateTime.of(LocalDateTime.MIN, ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MIN))

  val MaxUTCDateTime = OffsetDateTime.of(LocalDateTime.MAX, ZoneOffset.UTC)
  val MinUTCDateTime = OffsetDateTime.of(LocalDateTime.MIN, ZoneOffset.UTC)

  def currentMaxZonedDateTime = ZonedDateTime.of(LocalDateTime.MAX, ZoneId.systemDefault)
  def currentMinZonedDateTime = ZonedDateTime.of(LocalDateTime.MIN, ZoneId.systemDefault)

  val ZoneOfUTC = ZoneId.of("UTC")


  def addToInstant(instant: Instant, duration: Duration, subtract: Boolean): Instant =
    var s =
      if subtract then instant.getEpochSecond -# duration.getSeconds
      else             instant.getEpochSecond +# duration.getSeconds
    var ns =
      if subtract then instant.getNano - duration.getNano
      else             instant.getNano + duration.getNano
    if ns < 0 then
      while ns < 0 do
        ns += 1000000000
        if s < Long.MaxValue then s -= 1
    else if ns > 0 then
      while ns >= 1000000000 do
        ns -= 1000000000
        if s > Long.MinValue then s += 1
    if s < Instant.MIN.getEpochSecond then Instant.MIN
    else if s > Instant.MAX.getEpochSecond then Instant.MAX
    else Instant.ofEpochSecond(s, ns)

  private def predictLocalOverflow(epochSecs: Long, epochNano: Int, secs: Long, nanos: Long, subtract: Boolean): Int =
    var s =
      if subtract then epochSecs -# secs
      else             epochSecs +# secs
    var ns =
      if subtract then epochNano - nanos
      else             epochNano + nanos
    if ns < 0 then
      while ns < 0 do
        ns += 1000000000
        if s < Long.MaxValue then s -= 1
    else if ns > 0 then
      while ns >= 1000000000 do
        ns -= 1000000000
        if s > Long.MinValue then s += 1
    if s < DateTimeMinSeconds then -1
    else if s > DateTimeMaxSeconds then 1
    else 0

  def addToLocal(local: LocalDateTime, duration: Duration, subtract: Boolean): LocalDateTime =
    try
      predictLocalOverflow(local.toEpochSecond(ZoneOffset.UTC), local.getNano, duration.getSeconds, duration.getNano, subtract) match
        case -1 => LocalDateTime.MIN
        case  1 => LocalDateTime.MAX
        case _  => if subtract then local minus duration else local plus duration
    catch case _: DateTimeException =>
      if duration.getSeconds < 0 == subtract then LocalDateTime.MAX else LocalDateTime.MIN

  def addToOffset(offset: OffsetDateTime, duration: Duration, subtract: Boolean): OffsetDateTime =
    try
      predictLocalOverflow(offset.toEpochSecond + offset.getOffset.getTotalSeconds, offset.getNano, duration.getSeconds, duration.getNano, subtract) match
        case -1 => LocalDateTime.MIN.atOffset(offset.getOffset)
        case  1 => LocalDateTime.MAX.atOffset(offset.getOffset)
        case _  => if subtract then offset minus duration else offset plus duration
    catch case _: DateTimeException =>
      (if duration.getSeconds < 0 == subtract then LocalDateTime.MAX else LocalDateTime.MIN).atOffset(offset.getOffset)

  def addToZoned(zoned: ZonedDateTime, duration: Duration, subtract: Boolean): ZonedDateTime =
    try if subtract then zoned minus duration else zoned plus duration
    catch case _: DateTimeException =>
      (if duration.getSeconds < 0 == subtract then LocalDateTime.MAX else LocalDateTime.MIN).atZone(zoned.getZone)


  def compareUnlike(instant: Instant, offset: OffsetDateTime): Int =
    val ea = instant.getEpochSecond
    val eb = offset.toEpochSecond
    if      ea < eb then -1
    else if ea > eb then 1
    else                 Integer.compare(instant.getNano, offset.getNano)

  def compareUnlike(instant: Instant, zoned: ZonedDateTime): Int =
    val ea = instant.getEpochSecond
    val eb = zoned.toEpochSecond
    if      ea < eb then -1
    else if ea > eb then 1
    else                 Integer.compare(instant.getNano, zoned.getNano)

  def compareUnlike(offset: OffsetDateTime, instant: Instant): Int =
    val ea = offset.toEpochSecond
    val eb = instant.getEpochSecond
    if      ea < eb then -1
    else if ea > eb then 1
    else                 Integer.compare(offset.getNano, instant.getNano)

  def compareUnlike(offset: OffsetDateTime, zoned: ZonedDateTime): Int =
    val ea = offset.toEpochSecond
    val eb = zoned.toEpochSecond
    if      ea < eb then -1
    else if ea > eb then 1
    else                 Integer.compare(offset.getNano, zoned.getNano)

  def compareUnlike(zoned: ZonedDateTime, instant: Instant): Int =
    val ea = zoned.toEpochSecond
    val eb = instant.getEpochSecond
    if      ea < eb then -1
    else if ea > eb then 1
    else                 Integer.compare(zoned.getNano, instant.getNano)

  def compareUnlike(zoned: ZonedDateTime, offset: OffsetDateTime): Int =
    val ea = zoned.toEpochSecond
    val eb = offset.toEpochSecond
    if      ea < eb then -1
    else if ea > eb then 1
    else                 Integer.compare(zoned.getNano, offset.getNano)


  def toInstant(datetime: LocalDateTime): Instant =
    datetime.toInstant(ZoneId.systemDefault.getRules.getOffset(datetime))

  def toLocal(instant: Instant): LocalDateTime =
    val e = instant.getEpochSecond
    if      e > DateTimeSuperMaxSeconds then LocalDateTime.MAX
    else if e < DateTimeSuperMinSeconds then LocalDateTime.MIN
    else
      try toLocalChecked(instant)
      catch case _: DateTimeException => if e < 0 then LocalDateTime.MIN else LocalDateTime.MAX
  def toLocalChecked(instant: Instant): LocalDateTime =
    LocalDateTime.ofEpochSecond(instant.getEpochSecond, instant.getNano, ZoneId.systemDefault.getRules.getOffset(instant))

  def toLocal(offset: OffsetDateTime): LocalDateTime =
    try toLocalChecked(offset)
    catch case _: DateTimeException => if offset.getYear < 0 then LocalDateTime.MIN else LocalDateTime.MAX
  def toLocalChecked(offset: OffsetDateTime): LocalDateTime =
    val withoutOffset = offset.toLocalDateTime
    val myOffset = ZoneId.systemDefault.getRules.getOffset(withoutOffset)
    if myOffset == offset.getOffset then withoutOffset
    else withoutOffset.plusSeconds(myOffset.getTotalSeconds - offset.getOffset.getTotalSeconds)

  def toLocal(zoned: ZonedDateTime): LocalDateTime =
    try toLocalChecked(zoned)
    catch case _: DateTimeException => if zoned.getYear < 0 then LocalDateTime.MIN else LocalDateTime.MAX
  def toLocalChecked(zoned: ZonedDateTime): LocalDateTime =
    if zoned.getZone == ZoneId.systemDefault then zoned.toLocalDateTime
    else zoned.withZoneSameInstant(ZoneId.systemDefault).toLocalDateTime

  def toOffset(instant: Instant): OffsetDateTime =
    val e = instant.getEpochSecond
    if      e > DateTimeSuperMaxSeconds then currentMaxOffsetDateTime
    else if e < DateTimeSuperMinSeconds then currentMinOffsetDateTime
    else
      try toOffsetChecked(instant)
      catch case _: DateTimeException => if e < 0 then currentMinOffsetDateTime else currentMaxOffsetDateTime
  def toOffsetChecked(instant: Instant): OffsetDateTime = OffsetDateTime.ofInstant(instant, ZoneId.systemDefault)

  def toOffset(local: LocalDateTime): OffsetDateTime =
    local.atOffset(ZoneId.systemDefault.getRules.getOffset(local))

  def toLocalOffset(offset: OffsetDateTime): OffsetDateTime =
    val withoutOffset = offset.toLocalDateTime
    val myOffset = ZoneId.systemDefault.getRules.getOffset(withoutOffset)
    if myOffset == offset.getOffset then offset
    else
      try offset.withOffsetSameInstant(myOffset)
      catch case _: DateTimeException => if offset.getYear < 0 then currentMinOffsetDateTime else currentMaxOffsetDateTime
  def toLocalOffsetChecked(offset: OffsetDateTime): OffsetDateTime =
    offset.withOffsetSameInstant(ZoneId.systemDefault.getRules.getOffset(offset.toLocalDateTime))

  def toLocalOffset(zoned: ZonedDateTime): OffsetDateTime =
    if ZoneId.systemDefault == zoned.getZone then zoned.toOffsetDateTime
    else toLocalOffset(zoned.toOffsetDateTime)
  def toLocalOffsetChecked(zoned: ZonedDateTime): OffsetDateTime =
    if ZoneId.systemDefault == zoned.getZone then zoned.toOffsetDateTime
    else toLocalOffsetChecked(zoned.toOffsetDateTime)

  def toUTC(instant: Instant): OffsetDateTime =
    val e = instant.getEpochSecond
    if      e > DateTimeMaxSeconds then MaxUTCDateTime
    else if e < DateTimeMinSeconds then MinUTCDateTime
    else
      try toUTCChecked(instant)
      catch case _: DateTimeException => if e < 0 then MinUTCDateTime else MaxUTCDateTime
  def toUTCChecked(instant: Instant): OffsetDateTime = OffsetDateTime.ofInstant(instant, ZoneOfUTC)

  def toUTC(local: LocalDateTime): OffsetDateTime =
    try toUTCChecked(local)
    catch case _: DateTimeException =>
      if local.getYear < 0 then MinUTCDateTime else MaxUTCDateTime
  def toUTCChecked(local: LocalDateTime): OffsetDateTime =
    local.atOffset(ZoneId.systemDefault.getRules.getOffset(local)).withOffsetSameInstant(ZoneOffset.UTC)

  def toUTC(offset: OffsetDateTime): OffsetDateTime =
    if offset.getOffset == ZoneOffset.UTC then offset
    else
      try offset.withOffsetSameInstant(ZoneOffset.UTC)
      catch case _: DateTimeException => if offset.getYear < 0 then MinUTCDateTime else MaxUTCDateTime
  def toUTCChecked(offset: OffsetDateTime): OffsetDateTime =
    offset.withOffsetSameInstant(ZoneOffset.UTC)

  def toUTC(zoned: ZonedDateTime): OffsetDateTime = toUTC(zoned.toOffsetDateTime)
  def toUTCChecked(zoned: ZonedDateTime): OffsetDateTime = zoned.toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)

  def toZoned(instant: Instant): ZonedDateTime =
    val e = instant.getEpochSecond
    if      e > DateTimeSuperMaxSeconds then currentMaxZonedDateTime
    else if e < DateTimeSuperMinSeconds then currentMinZonedDateTime
    else
      try toZonedChecked(instant)
      catch case _: DateTimeException => if e < 0 then currentMinZonedDateTime else currentMaxZonedDateTime
  def toZonedChecked(instant: Instant): ZonedDateTime = ZonedDateTime.ofInstant(instant, ZoneId.systemDefault)

  def toZoned(local: LocalDateTime): ZonedDateTime = local.atZone(ZoneId.systemDefault)

  def toZoned(offset: OffsetDateTime): ZonedDateTime =
    try toZonedChecked(offset)
    catch case _: DateTimeException => if offset.getYear < 0 then currentMinZonedDateTime else currentMaxZonedDateTime
  def toZonedChecked(offset: OffsetDateTime): ZonedDateTime = offset.atZoneSameInstant(ZoneId.systemDefault)

  def toLocalZoned(zoned: ZonedDateTime): ZonedDateTime =
    if ZoneId.systemDefault == zoned.getZone then zoned
    else
      try zoned.withZoneSameInstant(ZoneId.systemDefault)
      catch case _: DateTimeException =>
        if zoned.getYear < 0 then LocalDateTime.MIN.atZone(ZoneId.systemDefault)
        else                      LocalDateTime.MAX.atZone(ZoneId.systemDefault)
  def toLocalZonedChecked(zoned: ZonedDateTime): ZonedDateTime = zoned.withZoneSameInstant(ZoneId.systemDefault)


  private def adjustTemporalNanos(sec: Long, nano: Int, mod: Int, pos: Int, neg: Int): Instant =
    var n = nano
    val m = n % mod
    if sec < 0 then
      if m + neg > 0 then n += (mod - m)
      else n -= m
    else
      if m + pos < mod then n -= m
      else n += (mod - m)
    if n >= 1000000000 then
      if sec + 1 > Instant.MAX.getEpochSecond then Instant.ofEpochSecond(sec, n - mod)
      else Instant.ofEpochSecond(sec + 1, n - 1000000000)
    else Instant.ofEpochSecond(sec, n)

  private def adjustTemporalSecs(sec: Long, nano: Int, mod: Int, spos: Int, npos: Int, sneg: Int, nneg: Int): Instant =
    var s = sec
    if s < 0 then
      s += sneg
      if nano + nneg > 0 then s += 1
      s -= (s % mod)
      Instant.ofEpochSecond(if s < Instant.MIN.getEpochSecond then s + mod else s)
    else
      s += spos
      if nano + npos >= 1000000000 then s += 1
      s -= (s % mod)
      Instant.ofEpochSecond(if s > Instant.MAX.getEpochSecond then s - mod else s)

  private def adjustInstantNanos(instant: Instant, mod: Int, pos: Int, neg: Int): Instant =
    val n = instant.getNano
    if n % mod == 0 then instant
    else adjustTemporalNanos(instant.getEpochSecond, n, mod, pos, neg)

  private def adjustInstantSecs(instant: Instant, mod: Int, spos: Int, npos:Int, sneg: Int, nneg: Int): Instant =
    val s = instant.getEpochSecond
    val n = instant.getNano
    if n == 0 && (s % mod) == 0 then instant
    else adjustTemporalSecs(s, n, mod, spos, npos, sneg, nneg)

  opaque type FloorInstant = Instant
  object FloorInstant {
    inline def apply(instant: Instant): kse.maths.TemporalCompanion.FloorInstant = instant
    extension(floor: FloorInstant) {
      def us: Instant = adjustInstantNanos(floor, 1000, 0, -999)
      def ms: Instant = adjustInstantNanos(floor, 1000000, 0, -999999)
      def s:  Instant = adjustInstantNanos(floor, 1000000000, 0, -999999999)
      def m:  Instant = adjustInstantSecs(floor, 60, 0, 0, -59, -999999999)
      def h:  Instant = adjustInstantSecs(floor, 3600, 0, 0, -3599, -999999999)
      def d:  Instant = adjustInstantSecs(floor, 86400, 0, 0, -86399, -999999999)
      inline def days: Instant = FloorInstant.d(floor)
    }
  }

  opaque type RoundInstant = Instant
  object RoundInstant {
    inline def apply(instant: Instant): kse.maths.TemporalCompanion.RoundInstant = instant
    extension(round: RoundInstant) {
      def us: Instant = adjustInstantNanos(round, 1000, 499, -500)
      def ms: Instant = adjustInstantNanos(round, 1000000, 499999, -500000)
      def s:  Instant = adjustInstantNanos(round, 1000000000, 499999999, -500000000)
      def m:  Instant = adjustInstantSecs(round, 60, 29, 999999999, -30, 0)
      def h:  Instant = adjustInstantSecs(round, 3600, 1799, 999999999, -1800, 0)
      def d:  Instant = adjustInstantSecs(round, 86400, 43199, 999999999, -43200, 0)
      inline def days: Instant = RoundInstant.d(round)
    }
  }

  opaque type CeilInstant = Instant
  object CeilInstant {
    inline def apply(instant: Instant): kse.maths.TemporalCompanion.CeilInstant = instant
    extension(ceil: CeilInstant) {
      def us: Instant = adjustInstantNanos(ceil, 1000, 999, 0)
      def ms: Instant = adjustInstantNanos(ceil, 1000000, 999999, 0)
      def s:  Instant = adjustInstantNanos(ceil, 1000000000, 999999999, 0)
      def m:  Instant = adjustInstantSecs(ceil, 60, 59, 999999999, 0, 0)
      def h:  Instant = adjustInstantSecs(ceil, 3600, 3599, 999999999, 0, 0)
      def d:  Instant = adjustInstantSecs(ceil, 86400, 86399, 999999999, 0, 0)
      inline def days: Instant = CeilInstant.d(ceil)
    }
  }


  private inline val Mns = 60000000000L
  private inline val Hns = 3600000000000L
  private inline val Dns = 86400000000000L
  private inline val MnsHp1 = 30000000001L
  private inline val HnsHp1 = 1800000000001L
  private inline val DnsHp1 = 43200000000001L

  private def fixErrLocal(local: LocalDateTime, err: Long, scale: Long, thresh: Long): LocalDateTime =
    if err == 0 then local
    else if err < thresh then local.minusNanos(err)
    else
      val fix = scale - err
      val fs = fix/1000000000
      try
        predictLocalOverflow(local.toEpochSecond(ZoneOffset.UTC), local.getNano, fs, fix - fs*1000000000, subtract = false) match
          case 1 => LocalDateTime.MAX.minusNanos(scale - 1)
          case _ => local plusNanos fix
      catch case _: DateTimeException => LocalDateTime.MAX.minusNanos(scale - 1)

  private def localMinErr(local: LocalDateTime): Long = 1000000000L*local.getSecond + local.getNano
  private def localHrErr(local: LocalDateTime): Long = 1000000000L*(60*local.getMinute + local.getSecond) + local.getNano
  private def localDayErr(local: LocalDateTime): Long = 1000000000L*(3600*local.getHour + 60*local.getMinute + local.getSecond) + local.getNano

  opaque type FloorLocal = LocalDateTime
  object FloorLocal {
    inline def apply(local: LocalDateTime): kse.maths.TemporalCompanion.FloorLocal = local
    extension (floor: FloorLocal) {
      def us: LocalDateTime = fixErrLocal(floor, floor.getNano % 1000, 1000, 1000)
      def ms: LocalDateTime = fixErrLocal(floor, floor.getNano % 1000000, 1000000, 1000000)
      def s:  LocalDateTime = fixErrLocal(floor, floor.getNano, 1000000000, 1000000000)
      def m:  LocalDateTime = fixErrLocal(floor, localMinErr(floor), Mns, Mns)
      def h:  LocalDateTime = fixErrLocal(floor, localHrErr( floor), Hns, Hns)
      def d:  LocalDateTime = fixErrLocal(floor, localDayErr(floor), Dns, Dns)
      inline def days: LocalDateTime = FloorLocal.d(floor)
    }
  }

  opaque type RoundLocal = LocalDateTime
  object RoundLocal {
    inline def apply(local: LocalDateTime): kse.maths.TemporalCompanion.RoundLocal = local
    extension (round: RoundLocal) {
      def us: LocalDateTime = fixErrLocal(round, round.getNano % 1000, 1000, 501)
      def ms: LocalDateTime = fixErrLocal(round, round.getNano % 1000000, 1000000, 500001)
      def s:  LocalDateTime = fixErrLocal(round, round.getNano, 1000000000, 500000001)
      def m:  LocalDateTime = fixErrLocal(round, localMinErr(round), Mns, MnsHp1)
      def h:  LocalDateTime = fixErrLocal(round, localHrErr( round), Hns, HnsHp1)
      def d:  LocalDateTime = fixErrLocal(round, localDayErr(round), Dns, DnsHp1)
      inline def days: LocalDateTime = RoundLocal.d(round)
    }
  }

  opaque type CeilLocal = LocalDateTime
  object CeilLocal {
    inline def apply(local: LocalDateTime): kse.maths.TemporalCompanion.CeilLocal = local
    extension (ceil: CeilLocal) {
      def us: LocalDateTime = fixErrLocal(ceil, ceil.getNano % 1000, 1000, 1)
      def ms: LocalDateTime = fixErrLocal(ceil, ceil.getNano % 1000000, 1000000, 1)
      def s:  LocalDateTime = fixErrLocal(ceil, ceil.getNano, 1000000000, 1)
      def m:  LocalDateTime = fixErrLocal(ceil, localMinErr(ceil), Mns, 1)
      def h:  LocalDateTime = fixErrLocal(ceil, localHrErr( ceil), Hns, 1)
      def d:  LocalDateTime = fixErrLocal(ceil, localDayErr(ceil), Dns, 1)
      inline def days: LocalDateTime = CeilLocal.d(ceil)
    }
  }

  private def fixNanoErrOffset(offset: OffsetDateTime, err: Int, scale: Int, thresh: Int): OffsetDateTime =
    if err == 0 then offset
    else if err < thresh then offset.minusNanos(err)
    else
      try offset.plusNanos(scale - err)
      catch case _: DateTimeException => LocalDateTime.MAX.minusNanos(scale - 1).atOffset(offset.getOffset)

  private def offsetToNearby(offset: OffsetDateTime, useHr: Boolean, useMin: Boolean, upThresh: Long): OffsetDateTime =
    val s = offset.getSecond + (if useMin then 60*offset.getMinute + (if useHr then 3600*offset.getHour else 0) else 0)
    val ns = offset.getNano
    if s == 0 && ns == 0 then offset
    else
      val down = ns + 1000000000L*s
      val up = (if useHr then Dns else if useMin then Hns else Mns) - down
      if down >= upThresh then
        try offset.plusNanos(up)
        catch case _: DateTimeException => offset.minusNanos(down)
      else
        try offset.minusNanos(down)
        catch case _: DateTimeException => offset.plusNanos(up)

  opaque type FloorOffset = OffsetDateTime
  object FloorOffset {
    inline def apply(offset: OffsetDateTime): kse.maths.TemporalCompanion.FloorOffset = offset
    extension (floor: FloorOffset) {
      def us: OffsetDateTime = fixNanoErrOffset(floor, floor.getNano % 1000, 1000, 1000)
      def ms: OffsetDateTime = fixNanoErrOffset(floor, floor.getNano % 1000000, 1000000, 1000000)
      def s:  OffsetDateTime = fixNanoErrOffset(floor, floor.getNano, 1000000000, 1000000000)
      def m:  OffsetDateTime = offsetToNearby(floor, false, false, Mns)
      def h:  OffsetDateTime = offsetToNearby(floor, false, true, Hns)
      def d:  OffsetDateTime = offsetToNearby(floor, true, true, Dns)
      inline def days: OffsetDateTime = FloorOffset.d(floor)
    }
  }

  opaque type RoundOffset = OffsetDateTime
  object RoundOffset {
    inline def apply(offset: OffsetDateTime): kse.maths.TemporalCompanion.RoundOffset = offset
    extension (round: RoundOffset) {
      def us: OffsetDateTime = fixNanoErrOffset(round, round.getNano % 1000, 1000, 501)
      def ms: OffsetDateTime = fixNanoErrOffset(round, round.getNano % 1000000, 1000000, 500001)
      def s:  OffsetDateTime = fixNanoErrOffset(round, round.getNano, 1000000000, 500000001)
      def m:  OffsetDateTime = offsetToNearby(round, false, false, MnsHp1)
      def h:  OffsetDateTime = offsetToNearby(round, false, true, HnsHp1)
      def d:  OffsetDateTime = offsetToNearby(round, true, true, DnsHp1)
      inline def days: OffsetDateTime = RoundOffset.d(round)
    }
  }

  opaque type CeilOffset = OffsetDateTime
  object CeilOffset {
    inline def apply(offset: OffsetDateTime): kse.maths.TemporalCompanion.CeilOffset = offset
    extension (ceil: CeilOffset) {
      def us: OffsetDateTime = fixNanoErrOffset(ceil, ceil.getNano % 1000, 1000, 1)
      def ms: OffsetDateTime = fixNanoErrOffset(ceil, ceil.getNano % 1000000, 1000000, 1)
      def s:  OffsetDateTime = fixNanoErrOffset(ceil, ceil.getNano, 1000000000, 1)
      def m:  OffsetDateTime = offsetToNearby(ceil, false, false, 1L)
      def h:  OffsetDateTime = offsetToNearby(ceil, false, true, 1L)
      def d:  OffsetDateTime = offsetToNearby(ceil, true, true, 1L)
      inline def days: OffsetDateTime = CeilOffset.d(ceil)
    }
  }

  private def fixNanoErrZoned(zoned: ZonedDateTime, err: Int, scale: Int, thresh: Int): ZonedDateTime =
    if err == 0 then zoned
    else if err < thresh then zoned.minusNanos(err)
    else
      try zoned.plusNanos(scale - err)
      catch case _: DateTimeException => LocalDateTime.MAX.minusNanos(scale - 1).atZone(zoned.getZone)

  private def zonedToNearby(zoned: ZonedDateTime, useHr: Boolean, useMin: Boolean, upThresh: Long): ZonedDateTime =
    val s = zoned.getSecond + (if useMin then 60*zoned.getMinute + (if useHr then 3600*zoned.getHour else 0) else 0)
    val ns = zoned.getNano
    if s == 0 && ns == 0 then zoned
    else
      val down = ns + 1000000000L*s
      val limit = (if useHr then Dns else if useMin then Hns else Mns)
      val up = limit - down
      var direction = 0
      val ans =
        if down >= upThresh then
          try
            direction = 1
            zoned.plusNanos(up)
          catch case _: DateTimeException =>
            direction = -2
            zoned.minusNanos(down)
        else
          try
            direction = -1
            zoned.minusNanos(down)
          catch case _: DateTimeException =>
            direction = 2
            zoned.plusNanos(up)
      val sans = ans.getSecond + (if useMin then 60*ans.getMinute + (if useHr then 3600*ans.getHour else 0) else 0)
      if sans == 0 then ans
      else direction match
        case -1 =>
          try ans.minusSeconds(sans)
          catch case _: DateTimeException => zoned.minusNanos(down)
        case 1 =>
          try ans.plusSeconds(limit - sans)
          catch case _: DateTimeException => zoned.plusNanos(up)
        case -2 => ans.minusSeconds(sans)
        case 2 => ans.plusSeconds(limit - sans)
        case _ => throw new DateTimeException("Logic error in ZonedDateTime rounding")

  opaque type FloorZoned = ZonedDateTime
  object FloorZoned {
    inline def apply(zoned: ZonedDateTime): kse.maths.TemporalCompanion.FloorZoned = zoned
    extension (floor: FloorZoned) {
      def us: ZonedDateTime = fixNanoErrZoned(floor, floor.getNano % 1000, 1000, 1000)
      def ms: ZonedDateTime = fixNanoErrZoned(floor, floor.getNano % 1000000, 1000000, 1000000)
      def s:  ZonedDateTime = fixNanoErrZoned(floor, floor.getNano, 1000000000, 1000000000)
      def m:  ZonedDateTime = zonedToNearby(floor, false, false, Mns)
      def h:  ZonedDateTime = zonedToNearby(floor, false, true, Hns)
      def d:  ZonedDateTime = zonedToNearby(floor, true, true, Dns)
      inline def days: ZonedDateTime = FloorZoned.d(floor)
    }
  }

  opaque type RoundZoned = ZonedDateTime
  object RoundZoned {
    inline def apply(zoned: ZonedDateTime): kse.maths.TemporalCompanion.RoundZoned = zoned
    extension (round: RoundZoned) {
      def us: ZonedDateTime = fixNanoErrZoned(round, round.getNano % 1000, 1000, 501)
      def ms: ZonedDateTime = fixNanoErrZoned(round, round.getNano % 1000000, 1000000, 500001)
      def s:  ZonedDateTime = fixNanoErrZoned(round, round.getNano, 1000000000, 500000001)
      def m:  ZonedDateTime = zonedToNearby(round, false, false, MnsHp1)
      def h:  ZonedDateTime = zonedToNearby(round, false, true, HnsHp1)
      def d:  ZonedDateTime = zonedToNearby(round, true, true, DnsHp1)
      inline def days: ZonedDateTime = RoundZoned.d(round)
    }
  }

  opaque type CeilZoned = ZonedDateTime
  object CeilZoned {
    inline def apply(zoned: ZonedDateTime): kse.maths.TemporalCompanion.CeilZoned = zoned
    extension (ceil: CeilZoned) {
      def us: ZonedDateTime = fixNanoErrZoned(ceil, ceil.getNano % 1000, 1000, 1)
      def ms: ZonedDateTime = fixNanoErrZoned(ceil, ceil.getNano % 1000000, 1000000, 1)
      def s:  ZonedDateTime = fixNanoErrZoned(ceil, ceil.getNano, 1000000000, 1)
      def m:  ZonedDateTime = zonedToNearby(ceil, false, false, 1L)
      def h:  ZonedDateTime = zonedToNearby(ceil, false, true, 1L)
      def d:  ZonedDateTime = zonedToNearby(ceil, true, true, 1L)
      inline def days: ZonedDateTime = CeilZoned.d(ceil)
    }
  }
}



extension (instant: Instant) {
  // +(Duration) in OverloadedExtensions
  // +!(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
  // -!(Duration) in OverloadedExtensions
  // -(Instant) in OverloadedExtensions
  // -(OffsetDateTime) in OverloadedExtensions
  // -(ZonedDateTime) in OverloadedExtensions

  inline def to(inst: Instant): Duration = Duration.between(instant, inst)
  def to(odt: OffsetDateTime): Duration =
    Duration.ofSeconds(odt.toEpochSecond - instant.getEpochSecond, odt.getNano - instant.getNano)
  def to(zdt: ZonedDateTime): Duration =
    Duration.ofSeconds(zdt.toEpochSecond - instant.getEpochSecond, zdt.getNano - instant.getNano)

  inline def <( inst: Instant): Boolean = instant.compareTo(inst) < 0
  inline def <=(inst: Instant): Boolean = instant.compareTo(inst) <= 0
  inline def >=(inst: Instant): Boolean = instant.compareTo(inst) >= 0
  inline def >( inst: Instant): Boolean = instant.compareTo(inst) > 0

  inline def <( odt: OffsetDateTime): Boolean = TemporalCompanion.compareUnlike(instant, odt) < 0
  inline def <=(odt: OffsetDateTime): Boolean = TemporalCompanion.compareUnlike(instant, odt) <= 0
  inline def >=(odt: OffsetDateTime): Boolean = TemporalCompanion.compareUnlike(instant, odt) >= 0
  inline def >( odt: OffsetDateTime): Boolean = TemporalCompanion.compareUnlike(instant, odt) > 0


  inline def max(inst: Instant): Instant = if instant.compareTo(inst) < 0 then inst else instant
  inline def min(inst: Instant): Instant = if instant.compareTo(inst) > 0 then inst else instant

  // clamp(Instant, Instant) in OverloadedExtensions
  // in(Instant, Instant) in OverloadedExtensions
  // checkIn(Instant, Instant) in OverloadedExtensions

  inline def age: Duration = Duration.between(Instant.now, instant)

  inline def D: kse.maths.DoubleInstant    = DoubleInstant(instant)

  inline def local: LocalDateTime          = TemporalCompanion.toLocal(instant)
  inline def checkedLocal: LocalDateTime   = TemporalCompanion.toLocalChecked(instant)

  inline def offset: OffsetDateTime        = TemporalCompanion.toOffset(instant)
  inline def checkedOffset: OffsetDateTime = TemporalCompanion.toOffsetChecked(instant)

  inline def utc: OffsetDateTime           = TemporalCompanion.toUTC(instant)
  inline def checkedUTC                    = TemporalCompanion.toUTCChecked(instant)

  inline def zoned: ZonedDateTime          = TemporalCompanion.toZoned(instant)
  inline def checkedZoned: ZonedDateTime   = TemporalCompanion.toZonedChecked(instant)

  inline def filetime: FileTime            = FileTime.from(instant)

  // trunc in OverloadedExtensions
  inline def floor: kse.maths.TemporalCompanion.FloorInstant = TemporalCompanion.FloorInstant(instant)
  inline def round: kse.maths.TemporalCompanion.RoundInstant = TemporalCompanion.RoundInstant(instant)
  inline def ceil:  kse.maths.TemporalCompanion.CeilInstant  = TemporalCompanion.CeilInstant(instant)
}



extension (local: LocalDateTime) {
  // +(Duration) in OverloadedExtensions
  // +!(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
  // -!(Duration) in OverloadedExtensions
  // -(LocalDateTime) in OverloadedExtensions

  inline def to(ldt: LocalDateTime): Duration = Duration.between(local, ldt)

  inline def <( ldt: LocalDateTime): Boolean = local.compareTo(ldt) < 0
  inline def <=(ldt: LocalDateTime): Boolean = local.compareTo(ldt) <= 0
  inline def >=(ldt: LocalDateTime): Boolean = local.compareTo(ldt) >= 0
  inline def >( ldt: LocalDateTime): Boolean = local.compareTo(ldt) > 0

  inline def max(ldt: LocalDateTime): LocalDateTime = if local.compareTo(ldt) < 0 then ldt else local
  inline def min(ldt: LocalDateTime): LocalDateTime = if local.compareTo(ldt) > 0 then ldt else local

  // clamp(LocalDateTime, LocalDateTime) in OverloadedExtensions
  // in(LocalDateTime, LocalDateTime) in OverloadedExtensions
  // checkIn(LocalDateTime, LocalDateTime) in OverloadedExtensions

  def D: kse.maths.DoubleInstant =
    DoubleInstant.fromSeconds(local.toEpochSecond(ZoneId.systemDefault.getRules.getOffset(local)), local.getNano)

  inline def instant: Instant       = TemporalCompanion.toInstant(local)

  inline def offset: OffsetDateTime = TemporalCompanion.toOffset(local)

  inline def utc: OffsetDateTime    = TemporalCompanion.toUTC(local)
  inline def checkedUTC             = TemporalCompanion.toUTCChecked(local)

  inline def zoned: ZonedDateTime   = TemporalCompanion.toZoned(local)

  inline def filetime: FileTime     = FileTime.from(TemporalCompanion.toInstant(local))


  // trunc in OverloadedExtensions
  inline def floor: kse.maths.TemporalCompanion.FloorLocal = TemporalCompanion.FloorLocal(local)
  inline def round: kse.maths.TemporalCompanion.RoundLocal = TemporalCompanion.RoundLocal(local)
  inline def ceil:  kse.maths.TemporalCompanion.CeilLocal  = TemporalCompanion.CeilLocal(local)
}


extension (offset: OffsetDateTime) {
  def MaxValue: OffsetDateTime = LocalDateTime.MAX atOffset offset.getOffset
  def MinValue: OffsetDateTime = LocalDateTime.MIN atOffset offset.getOffset

  // +(Duration) in OverloadedExtensions
  // +!(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
  // -!(Duration) in OverloadedExtensions
  // -(OffsetDateTime) in OverloadedExtensions
  // -(Instant) in OverloadedExtensions
  // -(ZonedDateTime) in OverloadedExtensions

  def to(odt: OffsetDateTime): Duration =
    Duration.ofSeconds(odt.toEpochSecond - offset.toEpochSecond, odt.getNano - offset.getNano)
  def to(instant: Instant): Duration =
    Duration.ofSeconds(instant.getEpochSecond - offset.toEpochSecond, instant.getNano - offset.getNano)
  def to(zdt: ZonedDateTime): Duration =
    Duration.ofSeconds(zdt.toEpochSecond - offset.toEpochSecond, zdt.getNano - offset.getNano)

  inline def <( odt: OffsetDateTime): Boolean = offset.compareTo(odt) < 0
  inline def <=(odt: OffsetDateTime): Boolean = offset.compareTo(odt) <= 0
  inline def >=(odt: OffsetDateTime): Boolean = offset.compareTo(odt) >= 0
  inline def >( odt: OffsetDateTime): Boolean = offset.compareTo(odt) > 0

  inline def max(odt: OffsetDateTime): OffsetDateTime = if offset.compareTo(odt) < 0 then odt else offset
  inline def min(odt: OffsetDateTime): OffsetDateTime = if offset.compareTo(odt) > 0 then odt else offset

  // clamp(OffsetDateTime, OffsetDateTime) in OverloadedExtensions
  // in(OffsetDateTime, OffsetDateTime) in OverloadedExtensions
  // checkIn(OffsetDateTime, OffsetDateTime) in OverloadedExtensions

  inline def D: kse.maths.DoubleInstant    = DoubleInstant.fromSeconds(offset.toEpochSecond, offset.getNano)

  inline def instant: Instant              = offset.toInstant

  inline def discardOffset: LocalDateTime  = offset.toLocalDateTime

  inline def local: LocalDateTime          = TemporalCompanion.toLocal(offset)
  inline def checkedLocal: LocalDateTime   = TemporalCompanion.toLocalChecked(offset)

  inline def offset: OffsetDateTime        = TemporalCompanion.toLocalOffset(offset)
  inline def checkedOffset: OffsetDateTime = TemporalCompanion.toLocalOffsetChecked(offset)

  inline def utc: OffsetDateTime           = TemporalCompanion.toUTC(offset)
  inline def checkedUTC: OffsetDateTime    = TemporalCompanion.toUTCChecked(offset)

  inline def zoned: ZonedDateTime          = TemporalCompanion.toZoned(offset)
  inline def checkedZoned: ZonedDateTime   = TemporalCompanion.toZonedChecked(offset)

  inline def filetime: FileTime            = FileTime.from(offset.toInstant)

  // trunc in OverloadedExtensions
  inline def floor: kse.maths.TemporalCompanion.FloorOffset = TemporalCompanion.FloorOffset(offset)
  inline def round: kse.maths.TemporalCompanion.RoundOffset = TemporalCompanion.RoundOffset(offset)
  inline def ceil:  kse.maths.TemporalCompanion.CeilOffset  = TemporalCompanion.CeilOffset(offset)
}


extension (zoned: ZonedDateTime) {
  def MaxValue: ZonedDateTime = LocalDateTime.MAX atZone zoned.getZone
  def MinValue: ZonedDateTime = LocalDateTime.MIN atZone zoned.getZone

  // +(Duration) in OverloadedExtensions
  // +!(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
  // -!(Duration) in OverloadedExtensions
  // -(ZonedDateTime) in OverloadedExtensions
  // -(Instant) in OverloadedExtensions
  // -(OffsetDateTime) in OverloadedExtensions

  def to(zdt: ZonedDateTime): Duration =
    Duration.ofSeconds(zdt.toEpochSecond - zoned.toEpochSecond, zdt.getNano - zoned.getNano)
  def to(instant: Instant): Duration =
    Duration.ofSeconds(instant.getEpochSecond - zoned.toEpochSecond, instant.getNano - zoned.getNano)
  def to(odt: OffsetDateTime): Duration =
    Duration.ofSeconds(odt.toEpochSecond - zoned.toEpochSecond, odt.getNano - zoned.getNano)

  inline def <( zdt: ZonedDateTime): Boolean = zoned.compareTo(zdt) < 0
  inline def <=(zdt: ZonedDateTime): Boolean = zoned.compareTo(zdt) <= 0
  inline def >=(zdt: ZonedDateTime): Boolean = zoned.compareTo(zdt) >= 0
  inline def >( zdt: ZonedDateTime): Boolean = zoned.compareTo(zdt) > 0

  inline def max(zdt: ZonedDateTime): ZonedDateTime = if zoned.compareTo(zdt) < 0 then zdt else zoned
  inline def min(zdt: ZonedDateTime): ZonedDateTime = if zoned.compareTo(zdt) > 0 then zdt else zoned

  // clamp(ZonedDateTime, ZonedDateTime) in OverloadedExtensions
  // in(ZonedDateTime, ZonedDateTime) in OverloadedExtensions
  // checkIn(ZonedDateTime, ZonedDateTime) in OverloadedExtensions

  inline def D: kse.maths.DoubleInstant    = DoubleInstant.fromSeconds(zoned.toEpochSecond, zoned.getNano)

  inline def instant: Instant              = zoned.toInstant

  inline def discardZoned: LocalDateTime   = zoned.toLocalDateTime

  inline def local: LocalDateTime          = TemporalCompanion.toLocal(zoned)
  inline def checkedLocal: LocalDateTime   = TemporalCompanion.toLocalChecked(zoned)

  inline def zoned: ZonedDateTime          = TemporalCompanion.toLocalZoned(zoned)
  inline def checkedZoned: ZonedDateTime   = TemporalCompanion.toLocalZonedChecked(zoned)

  inline def utc: OffsetDateTime           = TemporalCompanion.toUTC(zoned)
  inline def checkedUTC: OffsetDateTime    = TemporalCompanion.toUTCChecked(zoned)

  inline def offset: OffsetDateTime        = TemporalCompanion.toLocalOffset(zoned)
  inline def checkedOffset: OffsetDateTime = TemporalCompanion.toLocalOffsetChecked(zoned)

  inline def filetime: FileTime            = FileTime.from(zoned.toInstant)

  // trunc in OverloadedExtensions
  inline def floor: kse.maths.TemporalCompanion.FloorZoned = TemporalCompanion.FloorZoned(zoned)
  inline def round: kse.maths.TemporalCompanion.RoundZoned = TemporalCompanion.RoundZoned(zoned)
  inline def ceil:  kse.maths.TemporalCompanion.CeilZoned  = TemporalCompanion.CeilZoned(zoned)
}


extension (filetime: FileTime) {
  // +(Duration) in OverloadedExtensions
  // +!(Duration) in OverloadedExtensions
  // -(Duration) in OverloadedExtensions
  // -!(Duration) in OverloadedExtensions
  // -(FileTime) in OverloadedExtensions
  // -!(FileTime) in OverloadedExtensions

  /*
  def to(ft: FileTime): Duration = ???
  */

  inline def <( ft: FileTime): Boolean = filetime.compareTo(ft) < 0
  inline def <=(ft: FileTime): Boolean = filetime.compareTo(ft) <= 0
  inline def >=(ft: FileTime): Boolean = filetime.compareTo(ft) >= 0
  inline def >( ft: FileTime): Boolean = filetime.compareTo(ft) > 0

  inline def max(ft: FileTime): FileTime = if filetime.compareTo(ft) < 0 then ft else filetime
  inline def min(ft: FileTime): FileTime = if filetime.compareTo(ft) > 0 then ft else filetime

  // clamp(FileTime, FileTime) in OverloadedExtensions
  // in(FileTime, FileTime) in OverloadedExtensions
  // checkIn(FileTime, FileTime) in OverloadedExtensions

  /*
  inline def D: kse.maths.DoubleInstant    = DoubleInstant.fromSeconds(zoned.toEpochSecond, zoned.getNano)
  */

  /*
  inline def instant: Instant              = zoned.toInstant

  inline def local: LocalDateTime          = TemporalCompanion.toLocal(zoned)
  inline def checkedLocal: LocalDateTime   = TemporalCompanion.toLocalChecked(zoned)

  inline def utc: OffsetDateTime           = TemporalCompanion.toUTC(zoned)
  inline def checkedUTC: OffsetDateTime    = TemporalCompanion.toUTCChecked(zoned)

  inline def offset: OffsetDateTime        = TemporalCompanion.toLocalOffset(zoned)
  inline def checkedOffset: OffsetDateTime = TemporalCompanion.toLocalOffsetChecked(zoned)

  inline def zoned: ZonedDateTime          = TemporalCompanion.toLocalZoned(zoned)
  inline def checkedZoned: ZonedDateTime   = TemporalCompanion.toLocalZonedChecked(zoned)

  // trunc in OverloadedExtensions
  inline def floor: kse.maths.TemporalCompanion.FloorZoned = TemporalCompanion.FloorZoned(zoned)
  inline def round: kse.maths.TemporalCompanion.RoundZoned = TemporalCompanion.RoundZoned(zoned)
  inline def ceil:  kse.maths.TemporalCompanion.CeilZoned  = TemporalCompanion.CeilZoned(zoned)
  */
}





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
