// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020-22 Rex Kerr and Calico Life Sciences, LLC.

package kse.maths

import java.lang.{Math => jm}
import java.time._
import java.util.concurrent.TimeUnit
import java.nio.file.attribute.FileTime

import scala.annotation.targetName

opaque type NanoInstant = Long
object NanoInstant {
  inline def apply(nanos: Long): NanoInstant = nanos
  inline def now: NanoInstant = System.nanoTime

  extension (nt: NanoInstant) {
    inline def unwrap: Long = nt
  }

  extension (nt: kse.maths.NanoInstant) {
    inline def +(dt: kse.maths.NanoDuration): kse.maths.NanoInstant = NanoInstant(nt.unwrap + dt.unwrap)

    @targetName("instant_minus_duration")
    inline def -(dt: kse.maths.NanoDuration): kse.maths.NanoInstant = NanoInstant(nt.unwrap - dt.unwrap)

    @targetName("instant_minus_instant")
    inline def -(mt: kse.maths.NanoInstant): kse.maths.NanoDuration = NanoDuration(nt.unwrap - mt.unwrap)

    inline def to(mt: kse.maths.NanoInstant): kse.maths.NanoDuration = NanoDuration(mt.unwrap - nt.unwrap)

    inline def <( mt: kse.maths.NanoInstant): Boolean = nt.unwrap <  mt.unwrap
    inline def <=(mt: kse.maths.NanoInstant): Boolean = nt.unwrap <= mt.unwrap
    inline def >=(mt: kse.maths.NanoInstant): Boolean = nt.unwrap >= mt.unwrap
    inline def >( mt: kse.maths.NanoInstant): Boolean = nt.unwrap >  mt.unwrap

    inline def max(mt: kse.maths.NanoInstant): kse.maths.NanoInstant = if nt.unwrap < mt.unwrap then mt else nt
    inline def min(mt: kse.maths.NanoInstant): kse.maths.NanoInstant = if nt.unwrap > mt.unwrap then mt else nt

    def toNow: kse.maths.NanoDuration = NanoDuration since nt

    def pr: String =
      s"timestamp ${nt.unwrap} ns"
  }

  given Ordering[NanoInstant] = new {
    def compare(a: NanoInstant, b: NanoInstant) = a.unwrap compareTo b.unwrap
  }
}

opaque type NanoDuration = Long
object NanoDuration {
  inline def apply(nanos: Long): NanoDuration = nanos
  inline def since(nt: kse.maths.NanoInstant): NanoDuration = NanoDuration(System.nanoTime - nt.unwrap)

  extension (dt: NanoDuration) {
    inline def unwrap: Long = dt
  }

  extension (dt: kse.maths.NanoDuration) {
    @targetName("duration_plus_duration")
    inline def +(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap + et.unwrap)

    @targetName("duration_plus_instant")
    inline def +(nt: kse.maths.NanoInstant): kse.maths.NanoInstant = NanoInstant(nt.unwrap + dt.unwrap)

    inline def -(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap - et.unwrap)

    inline def unary_- : kse.maths.NanoDuration = NanoDuration(-dt.unwrap)

    @targetName("long_mul")
    inline def *(factor: Long): kse.maths.NanoDuration = NanoDuration(dt.unwrap * factor)

    @targetName("frac_mul")
    def *(frac: kse.maths.Frac): kse.maths.NanoDuration =
      val t = dt.unwrap
      if Int.MinValue < t && t <= Int.MaxValue then
        NanoDuration((t * frac.numerL) / frac.denomL)
      else
        val n = frac.numerL
        val d = frac.denomL
        NanoDuration((t/d)*n + ((t%d)*n)/d)

    @targetName("long_div")
    inline def /(factor: Long): kse.maths.NanoDuration = NanoDuration(dt.unwrap / factor)

    @targetName("frac_div")
    inline def /(frac: kse.maths.Frac): kse.maths.NanoDuration = dt * frac.reciprocal

    inline def <( et: kse.maths.NanoDuration): Boolean = dt.unwrap <  et.unwrap
    inline def <=(et: kse.maths.NanoDuration): Boolean = dt.unwrap <= et.unwrap
    inline def >=(et: kse.maths.NanoDuration): Boolean = dt.unwrap >= et.unwrap
    inline def >( et: kse.maths.NanoDuration): Boolean = dt.unwrap >  et.unwrap

    inline def abs: kse.maths.NanoDuration = if dt.unwrap < 0 then NanoDuration(-dt.unwrap) else dt
    inline def max(et: kse.maths.NanoDuration): kse.maths.NanoDuration = if dt.unwrap < et.unwrap then et else dt
    inline def min(et: kse.maths.NanoDuration): kse.maths.NanoDuration = if dt.unwrap > et.unwrap then et else dt

    inline def D: kse.maths.DoubleDuration = DoubleDuration(dt)
    def duration: Duration =
      val s = dt.unwrap/1000000000
      val n = dt.unwrap - s
      if n >= 0 then Duration.ofSeconds(s, n)
      else           Duration.ofSeconds(s-1, 1000000000+n)

    inline def in: kse.maths.NanoDuration.In       = dt.unwrap

    def pr: String =
      s"${dt.unwrap} ns"
  }

  opaque type In = Long
  object In {
    extension (in: In) {
      inline def ns: Long = in
      inline def us: Long = in/1000
      inline def ms: Long = in/1000000
      inline def  s: Long = in/1000000000

      inline def round: InRound = in
      inline def floor: InFloor = in
      inline def ceil: InCeil   = in
    }
  }

  opaque type InRound = Long
  object InRound {
    extension (round: InRound) {
      def us: Long =
        val l: Long = round
        val ans = l / 1000
        val e = l - 1000 * ans
        if      e >  500 then ans + 1
        else if e < -500 then ans - 1
        else                  ans
      def ms: Long =
        val l: Long = round
        val ans = l / 1000000
        val e = l - 1000000 * ans
        if      e >  500000 then ans + 1
        else if e < -500000 then ans - 1
        else                  ans
      def s: Long =
        val l: Long = round
        val ans = l / 1000000000
        val e = l - 1000000000 * ans
        if      e >  500000000 then ans + 1
        else if e < -500000000 then ans - 1
        else                  ans
    }
  }

  opaque type InFloor = Long
  object InFloor {
    extension (floor: InFloor) {
      def us: Long =
        val l: Long = floor
        if l >= 0 then l / 1000
        else
          val ans = l / 1000
          if ans * 1000 == l then ans else ans-1
      def ms: Long =
        val l: Long = floor
        if l >= 0 then l / 1000000
        else
          val ans = l / 1000000
          if ans * 1000000 == l then ans else ans-1
      def s: Long =
        val l: Long = floor
        if l >= 0 then l / 1000000000
        else
          val ans = l / 1000000000
          if ans * 1000000000 == l then ans else ans-1
    }
  }

  opaque type InCeil = Long
  object InCeil {
    extension (ceil: InCeil) {
      def us: Long =
        val l: Long = ceil
        if l <= 0 then l / 1000
        else
          val ans = l / 1000
          if ans * 1000 == l then ans else ans+1
      def ms: Long =
        val l: Long = ceil
        if l <= 0 then l / 1000000
        else
          val ans = l / 1000000
          if ans * 1000000 == l then ans else ans+1
      def s: Long =
        val l: Long = ceil
        if l <= 0 then l / 1000000000
        else
          val ans = l / 1000000000
          if ans * 1000000000 == l then ans else ans+1
    }
  }

  given Ordering[kse.maths.NanoDuration] = new {
    def compare(a: kse.maths.NanoDuration, b: kse.maths.NanoDuration) = a.unwrap compareTo b.unwrap
  }
}


opaque type DoubleInstant = Double
object DoubleInstant {
  inline def apply(t: Double): DoubleInstant = t
  inline def apply(i: Instant): DoubleInstant = i.getEpochSecond + i.getNano/1e9

  def from(seconds: Long, nanos: Int): DoubleInstant =
    if nanos == 0 then seconds.toDouble
    else if nanos % 1000000 == 0 && jm.abs(seconds) < 9223372036854775L then (1000*seconds + nanos/1000000)/1e3
    else seconds + nanos/1e9

  inline def now: DoubleInstant = apply(Instant.now)

  extension (t: DoubleInstant) {
    inline def unwrap: Double = t
  }

  extension (t: kse.maths.DoubleInstant) {
    inline def +(dt: kse.maths.DoubleDuration): DoubleInstant = DoubleInstant(t.unwrap + dt.unwrap)

    @targetName("instant_minus_duration")
    inline def -(dt: kse.maths.DoubleDuration): DoubleInstant = DoubleInstant(t.unwrap - dt.unwrap)

    @targetName("instant_minus_instant")
    inline def -(u: kse.maths.DoubleInstant): DoubleDuration = DoubleDuration(t.unwrap - u.unwrap)

    inline def to(u: kse.maths.DoubleInstant): DoubleDuration = DoubleDuration(u.unwrap - t.unwrap)

    def <( u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) < 0
    def <=(u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) <= 0
    def >=(u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) >= 0
    def >( u: kse.maths.DoubleInstant): Boolean = java.lang.Double.compare(t.unwrap, u.unwrap) > 0

    inline def max(u: kse.maths.DoubleInstant): kse.maths.DoubleInstant = if DoubleInstant.<(t)(u) then u else t
    inline def min(u: kse.maths.DoubleInstant): kse.maths.DoubleInstant = if DoubleInstant.>(t)(u) then u else t

    def toNow: DoubleDuration = DoubleDuration since t

    def instant: Instant =
      val b = t.unwrap
      if java.lang.Double.isNaN(b) then Instant.ofEpochSecond(Long.MaxValue) // Will probably throw an exception
      else if b >= MaxInstantDouble then Instant.MAX
      else if b <= MinInstantDouble then Instant.MIN
      else
        val a = jm.abs(b)
        if a < 4.19e6 then
          val ns = jm.rint(b*1e9).toLong
          if ns % 1000000 == 0 then Instant.ofEpochMilli(jm.rint(b*1e3).toLong)
          else
            val s = ns/1000000000
            Instant.ofEpochSecond(s, ns - 1000000000*s)
        else if a < 4.29e9 then
          val us = jm.rint(b*1e6).toLong
          if us % 1000 == 0 then Instant.ofEpochMilli(jm.rint(b*1e3).toLong)
          else
            val s = us/1000000
            Instant.ofEpochSecond(s, (us - 1000000*s)*1000)
        else if a < 4.39e12 then Instant.ofEpochMilli(jm.rint(b*1e3).toLong)
        else Instant.ofEpochSecond(jm.rint(b).toLong)
    def filetime: FileTime =
      val b = t.unwrap
      val a = jm.abs(b)
      if      a < 4.29e9  then
        val us = jm.rint(b*1e6).toLong
        if us % 1000 == 0 then FileTime.fromMillis(jm.rint(b*1e3).toLong)
        else FileTime.from(us, TimeUnit.MICROSECONDS)
      else if a < 4.39e12 then FileTime.fromMillis(jm.rint(b*1e3).toLong)
      else                    FileTime.from(jm.rint(b).toLong, TimeUnit.SECONDS)
    inline def local: LocalDateTime   = TemporalConversions.toLocal( DoubleInstant.instant(t))
    inline def offset: OffsetDateTime = TemporalConversions.toOffset(DoubleInstant.instant(t))
    inline def utc: OffsetDateTime    = TemporalConversions.toUTC(   DoubleInstant.instant(t))
    inline def zoned: ZonedDateTime   = TemporalConversions.toZoned( DoubleInstant.instant(t))

    def pr: String = s"epoch + ${t.unwrap} sec"
  }

  given Ordering[kse.maths.DoubleInstant] = new {
    def compare(t: kse.maths.DoubleInstant, u: kse.maths.DoubleInstant) = java.lang.Double.compare(t.unwrap, u.unwrap)
  }

  val MaxInstantDouble = Instant.MAX.getEpochSecond.toDouble
  val MinInstantDouble = Instant.MIN.getEpochSecond.toDouble
}

opaque type DoubleDuration = Double
object DoubleDuration {
  inline def apply(dt: Double): DoubleDuration = dt
  inline def apply(d: Duration): DoubleDuration = d.getSeconds + d.getNano/1e9
  inline def apply(n: kse.maths.NanoDuration): DoubleDuration = n.unwrap/1e9

  def since(dt: kse.maths.DoubleInstant): DoubleDuration = DoubleDuration(DoubleInstant(Instant.now).unwrap - dt)

  extension (dt: DoubleDuration) {
    inline def unwrap: Double = dt
  }

  extension (dt: kse.maths.DoubleDuration) {
    @targetName("duration_plus_instant")
    inline def +(t: kse.maths.DoubleInstant): kse.maths.DoubleInstant = DoubleInstant(t.unwrap + dt.unwrap)

    @targetName("duration_plus_duration")
    inline def +(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap + du.unwrap)

    inline def -(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap - du.unwrap)

    inline def unary_- : kse.maths.DoubleDuration = DoubleDuration(-dt.unwrap)

    inline def *(scale: Double): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap * scale)

    inline def /(scale: Double): kse.maths.DoubleDuration = DoubleDuration(dt.unwrap / scale)

    inline def <( du: kse.maths.DoubleDuration): Boolean = java.lang.Double.compare(dt.unwrap, du.unwrap) < 0
    inline def <=(du: kse.maths.DoubleDuration): Boolean = java.lang.Double.compare(dt.unwrap, du.unwrap) <= 0
    inline def >=(du: kse.maths.DoubleDuration): Boolean = java.lang.Double.compare(dt.unwrap, du.unwrap) >= 0
    inline def >( du: kse.maths.DoubleDuration): Boolean = java.lang.Double.compare(dt.unwrap, du.unwrap) > 0

    inline def abs: kse.maths.DoubleDuration = if dt.unwrap < 0 then DoubleDuration(-dt.unwrap) else dt
    inline def max(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration = if DoubleDuration.<(dt)(du) then du else dt
    inline def min(du: kse.maths.DoubleDuration): kse.maths.DoubleDuration = if DoubleDuration.>(dt)(du) then du else dt

    def nano: kse.maths.NanoDuration = NanoDuration(jm.rint(dt.unwrap * 1e9).toLong)
    def duration: Duration =
      val t = jm.floor(dt.unwrap)
      val n = jm.rint((dt.unwrap - t)*1e9)
      Duration.ofSeconds(t.toLong, n.toInt)

    inline def in: kse.maths.DoubleDuration.In = dt.unwrap
    inline def long: kse.maths.DoubleDuration.InLong = dt.unwrap

    inline def round: kse.maths.DoubleDuration.Round = dt.unwrap
    inline def floor: kse.maths.DoubleDuration.Floor = dt.unwrap
    inline def ceil:  kse.maths.DoubleDuration.Ceil  = dt.unwrap

    def pr: String =
      s"${dt.unwrap} sec"
  }

  opaque type In = Double
  object In {
    extension (in: In) {
      inline def ns: Double  = in * 1e9
      inline def us: Double  = in * 1e6
      inline def ms: Double  = in * 1e3
      inline def s: Double   = in
      inline def m: Double   = in / 60.0
      inline def h: Double   = in / 3600.0
      inline def d: Double   = in / 86400.0

      inline def long:  InLong  = in
      inline def round: InRound = in
      inline def floor: InFloor = in
      inline def ceil:  InCeil  = in
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

      inline def round: InRound = in
      inline def floor: InRound = in
      inline def ceil: InRound  = in
    }
  }

  opaque type InRound = Double
  object InRound {
    extension (in: InRound) {
      inline def ns: Long  = jm.rint(in * 1e9).toLong
      inline def us: Long  = jm.rint(in * 1e6).toLong
      inline def ms: Long  = jm.rint(in * 1e3).toLong
      inline def s: Long   = jm.rint(in).toLong
      inline def m: Long   = jm.rint(in / 60.0).toLong
      inline def h: Long   = jm.rint(in / 3600.0).toLong
      inline def d: Long   = jm.rint(in / 86400.0).toLong
    }
  }

  opaque type InFloor = Double
  object InFloor {
    extension (in: InFloor) {
      inline def ns: Long  = jm.floor(in * 1e9).toLong
      inline def us: Long  = jm.floor(in * 1e6).toLong
      inline def ms: Long  = jm.floor(in * 1e3).toLong
      inline def s: Long   = jm.floor(in).toLong
      inline def m: Long   = jm.floor(in / 60).toLong
      inline def h: Long   = jm.floor(in / 3600).toLong
      inline def d: Long   = jm.floor(in / 86400).toLong
    }
  }

  opaque type InCeil = Double
  object InCeil {
    extension (in: InCeil) {
      inline def ns: Long  = jm.ceil(in * 1e9).toLong
      inline def us: Long  = jm.ceil(in * 1e6).toLong
      inline def ms: Long  = jm.ceil(in * 1e3).toLong
      inline def s: Long   = jm.ceil(in).toLong
      inline def m: Long   = jm.ceil(in / 60.0).toLong
      inline def h: Long   = jm.ceil(in / 3600.0).toLong
      inline def d: Long   = jm.ceil(in / 86400.0).toLong
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
    }
  }

  given Ordering[kse.maths.DoubleDuration] = new {
    def compare(dt: kse.maths.DoubleDuration, du: kse.maths.DoubleDuration) = java.lang.Double.compare(dt.unwrap, du.unwrap)
  }
}


object TemporalConversions {
  def toDouble(instant: Instant): kse.maths.DoubleInstant = DoubleInstant(instant)
  def toDouble(datetime: LocalDateTime): kse.maths.DoubleInstant = toDouble(datetime.atZone(ZoneId.systemDefault))
  def toDouble(datetime: OffsetDateTime): kse.maths.DoubleInstant = DoubleInstant.from(datetime.toEpochSecond, datetime.getNano)
  def toDouble(datetime: ZonedDateTime): kse.maths.DoubleInstant = DoubleInstant.from(datetime.toEpochSecond, datetime.getNano)

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
extension (instant: Instant) {
  inline def double: kse.maths.DoubleInstant = DoubleInstant(instant)
  inline def local: LocalDateTime            = TemporalConversions.toLocal(instant)
  inline def offset: OffsetDateTime          = TemporalConversions.toOffset(instant)
  inline def utc: OffsetDateTime             = TemporalConversions.toUTC(instant)
  inline def zoned: ZonedDateTime            = TemporalConversions.toZoned(instant)

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
  inline def double: kse.maths.DoubleInstant = TemporalConversions.toDouble(zoned)
  inline def instant: Instant                = zoned.toInstant
  inline def local: LocalDateTime            = TemporalConversions.toLocal(zoned)
  inline def offset: OffsetDateTime          = zoned.toOffsetDateTime
  inline def utc: OffsetDateTime             = TemporalConversions.toUTC(zoned)

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
