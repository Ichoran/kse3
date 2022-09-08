// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.maths

import java.time._
import java.nio.file.attribute.FileTime

import scala.annotation.targetName

class NanoTime(val time: Long) extends AnyVal {
  def <(that: NanoTime) = time < that.time
  def <=(that: NanoTime) = time <= that.time
  def >=(that: NanoTime) = time >= that.time
  def >(that: NanoTime) = time > that.time
  def min(that: NanoTime) = if (that.time < time) that else this
  def max(that: NanoTime) = if (that.time > time) that else this

  def -(that: NanoTime): DoubleAsDeltaT = DoubleAsDeltaT.apply((time - that.time)/1e9)

  def elapsed: DoubleAsDeltaT = NanoTime.now - this
}
object NanoTime {
  def now: NanoTime = new NanoTime(System.nanoTime)

  given Ordering[NanoTime] = new {
    def compare(a: NanoTime, b: NanoTime) = a.time compareTo b.time
  }
}

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

object TemporalMaths {
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
