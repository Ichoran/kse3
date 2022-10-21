// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020-22 Rex Kerr and Calico Life Sciences, LLC.

package kse.maths

import java.lang.{Math => jm}
import java.time._
import java.util.concurrent.TimeUnit
import java.nio.file.attribute.FileTime

import scala.annotation.targetName

import kse.maths._

object FileTimeCompanion {
  val MAX = FileTime.from(Long.MaxValue, TimeUnit.SECONDS)
  val MIN = FileTime.from(Long.MinValue, TimeUnit.SECONDS)
}
object DurationCompanion {
  val MAX: Duration = Duration.ofSeconds(Long.MaxValue, 999999999)
  val MIN: Duration = Duration.ofSeconds(Long.MinValue)

  val MaxMicros: Duration  = Duration.ofSeconds(Long.MaxValue, 999000)
  val MaxMillis: Duration  = Duration.ofSeconds(Long.MaxValue, 900000)
  val MaxMinutes: Duration = Duration.ofSeconds( 9223372036854775800L)
  val MinMinutes: Duration = Duration.ofSeconds(-9223372036854775800L)
  val MaxHours: Duration   = Duration.ofSeconds( 9223372036854774000L)
  val MinHours: Duration   = Duration.ofSeconds(-9223372036854774000L)
  val MaxDays: Duration    = Duration.ofSeconds( 9223372036854720000L)
  val MinDays: Duration    = Duration.ofSeconds(-9223372036854720000L)

  private[this] def robustFileTimeFrom(as: Long, an: Int, bs: Long, bn: Int, sub: Boolean, sig: Int): FileTime =
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

  def add(d: Duration, dd: Duration): Duration =
    val ns = d.getNano + dd.getNano
    val cs = d.getSeconds +# dd.getSeconds
    if ns == 0 then Duration.ofSeconds(cs)
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
    if ns == 0 then Duration.ofSeconds(cs)
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
  def mul(d: Duration, scale: Int): Duration =
    if d.getNano == 0 || scale == 0 then Duration.ofSeconds(d.getSeconds *# scale)
    else if scale == 1 then d
    else if scale == -1 then
      if d.getSeconds == Long.MinValue && d.getNano == 0 then DurationCompanion.MAX
      else d.negated
    else
      val ds = d.getSeconds
      if ds > Int.MinValue && ds <= Int.MaxValue then d.multipliedBy(scale)
      else if ds == Long.MinValue then
        if scale > 0 then DurationCompanion.MIN else DurationCompanion.MAX
      else
        val neg = scale < 0 == ds >= 0
        val as = if ds < 0 then -ds else ds
        val sc = if scale < 0 then -scale.toLong else scale.toLong
        var hi = (as >>> 32) * sc
        var md = ((as >>> 1) & 0x7FFFFFFFL) * sc
        var lo = ((1000000000 * (as&1)) + d.getNano) * sc
        val lover = lo / 2000000000
        if lover > 0 then
          lo -= 2000000000*lover
          md += lover
        var mover = md >>> 31
        if mover > 0 then
          mover = mover & 0x7FFFFFFFL
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
    var hi = 0L
    var md = d.getSeconds
    var lo = d.getNano.toLong
    val numer = frac.numer
    val denom = frac.denom
    val neg = md < 0 != numer < 0
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
  def div(d: Duration, factor: Int): Duration =
    if factor == 1 then d
    else if factor == -1 then
      if d.getSeconds == Long.MinValue && d.getNano == 0 then DurationCompanion.MAX
      else d.negated
    else d.dividedBy(factor)
  def div(d: Duration, frac: Frac): Duration =
    if frac.numer == Int.MinValue then
      ???
    else mul(d, frac.reciprocal)

  opaque type Into = Duration
  object Into {
    def apply(d: Duration): kse.maths.DurationCompanion.Into = d

    extension (in: Into) {
      def ns: Long =
        var sec = in.getSeconds
        if sec < 0 then
          var nano = in.getNano
          if nano > 0 then
            nano -= 1000000000
            sec += 1
          (sec *# 1000000000) +# nano
        else
          sec *# 1000000000 +# in.getNano
      def us: Long =
        var sec = in.getSeconds
        if sec < 0 then
          var nano = in.getNano
          if nano > 0 then
            nano -= 1000000000
            sec += 1
          (sec *# 1000000) +# nano/1000
        else
          sec *# 1000000 +# in.getNano/1000
      def ms: Long =
        var sec = in.getSeconds
        if sec < 0 then
          var nano = in.getNano
          if nano > 0 then
            nano -= 1000000000
            sec += 1
          (sec *# 1000) +# nano/1000000
        else sec *# 1000 +# in.getNano/1000000
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

  opaque type Round = Duration
  object Round {
    def apply(d: Duration): kse.maths.DurationCompanion.Round = d

    extension (round: Round) {
      def us: Duration =
        val nano = round.getNano
        if nano == 0 then round
        else
          val mod = nano % 1000
          if mod == 0 then round
          else
            val sec = round.getSeconds
            if sec > 0 then
              if sec < Long.MaxValue || nano < 999999000 then
                Duration.ofSeconds(sec, if mod <= 500 then nano - mod else nano + (1000 - mod))
              else MaxMicros
            else
              Duration.ofSeconds(sec, if mod >= 500 then nano + (1000 - mod) else nano - mod)
      def ms: Duration =
        val nano = round.getNano
        if nano == 0 then round
        else
          val mod = nano % 1000000
          if mod == 0 then round
          else
            val sec = round.getSeconds
            if sec > 0 then
              if sec < Long.MaxValue || nano < 999000000 then
                Duration.ofSeconds(sec, if mod <= 500000 then nano - mod else nano + (1000000 - mod))
              else MaxMillis
            else
              Duration.ofSeconds(sec, if mod >= 500000 then nano + (1000000 - mod) else nano - mod)
      def s: Duration =
        val nano = round.getNano
        if nano == 0 then round
        else
          val sec = round.getSeconds
          if sec < 0 then Duration.ofSeconds(sec + (if sec < 500000000 then 0 else 1))
          else Duration.ofSeconds(sec +# (if sec > 500000000 then 1 else 0))
      def m: Duration =
        val nano = round.getNano
        val sec = round.getSeconds
        val mod = sec % 60
        if nano == 0 && mod == 0 then round
        else if sec < 0 then
          if sec <= MinMinutes.getSeconds then MinMinutes
          else Duration.ofSeconds(if mod < -30 then sec - (60 + mod) else sec - mod)
        else
          if sec >= MaxMinutes.getSeconds then MaxMinutes
          else Duration.ofSeconds(if mod < 30 || (mod == 30 && nano == 0) then sec - mod else sec + (60 - mod))
      def h: Duration =
        val nano = round.getNano
        val sec = round.getSeconds
        val mod = sec % 3600
        if nano == 0 && mod == 0 then round
        else if sec < 0 then
          if sec <= MinHours.getSeconds then MinHours
          else Duration.ofSeconds(if mod < -1800 then sec - (3600 + mod) else sec - mod)
        else
          if sec >= MaxHours.getSeconds then MaxHours
          else Duration.ofSeconds(if mod < 1800 || (mod == 1800 && nano == 0) then sec - mod else sec + (3600 - mod))
      def d: Duration =
        val nano = round.getNano
        val sec = round.getSeconds
        val mod = sec % 86400
        if nano == 0 && mod == 0 then round
        else if sec < 0 then
          if sec <= MinDays.getSeconds then MinDays
          else Duration.ofSeconds(if mod < -43200 then sec - (86400 + mod) else sec - mod)
        else
          if sec >= MaxDays.getSeconds then MaxDays
          else Duration.ofSeconds(if mod < 43200 || (mod == 43200 && nano == 0) then sec - mod else sec + (86400 - mod))
      inline def days = round.d


      inline def into: kse.maths.DurationCompanion.InRound = round
    }
  }

  opaque type Floor = Duration
  object Floor {
    def apply(d: Duration): kse.maths.DurationCompanion.Floor = d

    extension (floor: Floor) {
      def us: Duration =
        val nano = floor.getNano
        if nano == 0 then floor
        else
          val mod = nano % 1000
          if mod == 0 then floor else Duration.ofSeconds(floor.getSeconds, nano - mod)
      def ms: Duration =
        val nano = floor.getNano
        if nano == 0 then floor
        else
          val mod = nano % 1000000
          if mod == 0 then floor else Duration.ofSeconds(floor.getSeconds, nano - mod)
      def s: Duration =
        if floor.getNano == 0 then floor else Duration.ofSeconds(floor.getSeconds)
      def m: Duration =
        val sec = floor.getSeconds
        val mod = sec % 60
        if mod == 0 then
          if floor.getNano == 0 then floor else Duration.ofSeconds(sec)
        else if sec >= 0 then Duration.ofSeconds(sec - mod)
        else if sec > MinMinutes.getSeconds then Duration.ofSeconds((sec - mod) - 60)
        else MinMinutes
      def h: Duration =
        val sec = floor.getSeconds
        val mod = sec % 3600
        if mod == 0 then
          if floor.getNano == 0 then floor else Duration.ofSeconds(sec)
        else if sec >= 0 then Duration.ofSeconds(sec - mod)
        else if sec > MinHours.getSeconds then Duration.ofSeconds((sec - mod) - 3600)
        else MinHours
      def d: Duration =
        val sec = floor.getSeconds
        val mod = sec % 86400
        if mod == 0 then
          if floor.getNano == 0 then floor else Duration.ofSeconds(sec)
        else if sec >= 0 then Duration.ofSeconds(sec - mod)
        else if sec > MinDays.getSeconds then Duration.ofSeconds((sec - mod) - 86400)
        else MinDays
      inline def days = floor.d

      inline def into: kse.maths.DurationCompanion.InFloor = floor
    }
  }

  opaque type Ceil = Duration
  object Ceil {
    def apply(d: Duration): kse.maths.DurationCompanion.Ceil = d

    extension (ceil: Ceil) {
      def us: Duration =
        val nano = ceil.getNano
        if nano == 0 then ceil
        else
          val mod = nano % 1000
          if mod == 0 then ceil
          else
            val sec = ceil.getSeconds
            if sec < Long.MaxValue || nano < 999999000 then Duration.ofSeconds(ceil.getSeconds, nano + (1000 - mod))
            else MaxMicros
      def ms: Duration =
        val nano = ceil.getNano
        if nano == 0 then ceil
        else
          val mod = nano % 1000000
          if mod == 0 then ceil
          else
            val sec = ceil.getSeconds
            if sec < Long.MaxValue || nano < 999000000 then Duration.ofSeconds(ceil.getSeconds, nano + (1000000 - mod))
            else MaxMillis
      def s: Duration =
        if ceil.getNano == 0 then ceil else Duration.ofSeconds(ceil.getSeconds +# 1)
      def m: Duration =
        val sec = ceil.getSeconds
        val mod = sec % 60
        if mod == 0 then
          if ceil.getNano == 0 then ceil else Duration.ofSeconds((sec +# 60) min 9223372036854775800L)
        else if sec < 0 then Duration.ofSeconds(sec - mod)
        else if sec < MaxMinutes.getSeconds then Duration.ofSeconds(sec + (60 - mod))
        else MaxMinutes
      def h: Duration =
        val sec = ceil.getSeconds
        val mod = sec % 3600
        if mod == 0 then
          if ceil.getNano == 0 then ceil else Duration.ofSeconds((sec +# 3600) min 9223372036854774000L)
        else if sec < 0 then Duration.ofSeconds(sec - mod)
        else if sec < MaxHours.getSeconds then Duration.ofSeconds(sec + (3600 - mod))
        else MaxHours
      def d: Duration =
        val sec = ceil.getSeconds
        val mod = sec % 86400
        if mod == 0 then
          if ceil.getNano == 0 then ceil else Duration.ofSeconds((sec +# 86400) min 9223372036854720000L)
        else if sec < 0 then Duration.ofSeconds(sec - mod)
        else if sec < MaxDays.getSeconds then Duration.ofSeconds(sec + (86400 - mod))
        else MaxDays
      inline def days: Duration = ceil.d

      inline def into: kse.maths.DurationCompanion.InCeil = ceil
    }
  }

  opaque type Trunc = Duration
  object Trunc {
    def apply(d: Duration): kse.maths.DurationCompanion.Trunc = d

    extension (trunc: Trunc) {
      def us: Duration =
        val nano = trunc.getNano
        if nano == 0 then trunc
        else
          val mod = nano % 1000
          if mod == 0 then trunc
          else
            val sec = trunc.getSeconds
            Duration.ofSeconds(sec, if sec < 0 then nano + (1000 - mod) else nano - mod)
      def ms: Duration =
        val nano = trunc.getNano
        if nano == 0 then trunc
        else
          val mod = nano % 1000000
          if mod == 0 then trunc
          else
            val sec = trunc.getSeconds
            Duration.ofSeconds(sec, if sec < 0 then nano + (1000000 - mod) else nano - mod)
      def s: Duration =
        val nano = trunc.getNano
        if nano == 0 then trunc
        else
          val sec = trunc.getSeconds
          Duration.ofSeconds(if sec < 0 then sec + 1 else sec)
      def m: Duration =
        val nano = trunc.getNano
        val sec = trunc.getSeconds
        val mod = sec % 60
        if nano == 0 && mod == 0 then trunc
        else Duration.ofSeconds(if sec < 0 then sec + (60 + mod) else sec - mod)
      def h: Duration =
        val nano = trunc.getNano
        val sec = trunc.getSeconds
        val mod = sec % 3600
        if nano == 0 && mod == 0 then trunc
        else Duration.ofSeconds(if sec < 0 then sec + (3600 + mod) else sec - mod)
      def d: Duration =
        val nano = trunc.getNano
        val sec = trunc.getSeconds
        val mod = sec % 86400
        if nano == 0 && mod == 0 then trunc
        else Duration.ofSeconds(if sec < 0 then sec + (86400 + mod) else sec - mod)
      inline def days = trunc.d
    }
  }

  opaque type InRound = Duration
  object InRound {
    extension (round: InRound) {
      def us: Long =
        var sec = round.getSeconds
        if sec == Long.MinValue then Long.MinValue
        else
          var nano = round.getNano
          if sec < 0 && nano != 0 then
            sec += 1
            nano -= 1000000000
          (sec *# 1000000) +# (nano + (if sec < 0 then -499 else 499))/1000
      def ms: Long =
        var sec = round.getSeconds
        if sec == Long.MinValue then Long.MinValue
        else
          var nano = round.getNano
          if sec < 0 && nano != 0 then
            sec += 1
            nano -= 1000000000
          (sec *# 1000) +# (nano + (if sec < 0 then -499999 else 499999))/1000000
      def s: Long =
        val sec = round.getSeconds
        val nano = round.getNano
        if sec < 0 then
          if nano < 500000000 then sec
          else sec + 1
        else
          if nano > 500000000 && sec < Long.MaxValue then sec + 1
          else sec
      def m: Long =
        val sec = round.getSeconds
        if sec < MinMinutes.getSeconds then MinMinutes.getSeconds/60
        else if sec > MaxMinutes.getSeconds then MaxMinutes.getSeconds/60
        else
          val minutes = sec/60
          val err = sec - minutes*60
          if sec < 0 then
            if err < -30 then minutes - 1 else minutes
          else
            if err > 30 || (err == 30 && round.getNano > 0) then minutes + 1 else minutes
      def h: Long =
        val sec = round.getSeconds
        if sec < MinHours.getSeconds then MinHours.getSeconds/3600
        else if sec > MaxHours.getSeconds then MaxHours.getSeconds/3600
        else
          val hours = sec/3600
          val err = sec - hours*3600
          if sec < 0 then
            if err < -1800 then hours - 1 else hours
          else
            if err > 1800 || (err == 1800 && round.getNano > 0) then hours + 1 else hours
      def d: Long =
        val sec = round.getSeconds
        if sec < MinDays.getSeconds then MinDays.getSeconds/86400
        else if sec > MaxDays.getSeconds then MaxDays.getSeconds/86400
        else
          val dayz = sec/86400
          val err = sec - dayz*86400
          if sec < 0 then
            if err < -43200 then dayz - 1 else dayz
          else
            if err > 43200 || (err == 43200 && round.getNano > 0) then dayz + 1 else dayz
      def days: Long = round.d
    }
  }

  opaque type InFloor = Duration
  object InFloor {
    extension (floor: InFloor) {
      def us: Long =
        var sec = floor.getSeconds
        if sec == Long.MinValue then Long.MinValue
        else
          var nano = floor.getNano
          if sec < 0 && nano != 0 then
            sec += 1
            nano -= 1000000000
          (sec *# 1000000) +# nano/1000
      def ms: Long =
        var sec = floor.getSeconds
        if sec == Long.MinValue then Long.MinValue
        else
          var nano = floor.getNano
          if sec < 0 && nano != 0 then
            sec += 1
            nano -= 1000000000
          (sec *# 1000) +# nano/1000000
      def s: Long = floor.getSeconds
      def m: Long =
        val sec = floor.getSeconds
        if sec <= MinMinutes.getSeconds then MinMinutes.getSeconds/60
        else if sec > MaxMinutes.getSeconds then MaxMinutes.getSeconds/60
        else if sec >= 0 then sec/60
        else
          val minutes = sec/60
          val error = sec - minutes*60
          if error == 0 then minutes else minutes - 1
      def h: Long =
        val sec = floor.getSeconds
        if sec <= MinHours.getSeconds then MinHours.getSeconds/3600
        else if sec > MaxHours.getSeconds then MaxHours.getSeconds/3600
        else if sec >= 0 then sec/3600
        else
          val hours = sec/3600
          val error = sec - hours*3600
          if error == 0 then hours else hours - 1
      def d: Long =
        val sec = floor.getSeconds
        if sec <= MinDays.getSeconds then MinDays.getSeconds/86400
        else if sec > MaxDays.getSeconds then MaxDays.getSeconds/86400
        else if sec >= 0 then sec/86400
        else
          val dayz = sec/86400
          val error = sec - dayz*86400
          if error == 0 then dayz else dayz - 1
      inline def days = floor.d
    }
  }

  opaque type InCeil = Duration
  object InCeil {
    extension (ceil: InCeil) {
      def us: Long =
        val sec = ceil.getSeconds
        if sec == Long.MaxValue then Long.MaxValue
        else
          (sec *# 1000000) +# (ceil.getNano + 999)/1000
      def ms: Long =
        val sec = ceil.getSeconds
        if sec == Long.MaxValue then Long.MaxValue
        else
          (sec *# 1000) +# (ceil.getNano + 999999)/1000000
      def s: Long =
        val sec = ceil.getSeconds
        if sec == Long.MaxValue then Long.MaxValue
        else sec + (if ceil.getNano > 0 then 1 else 0)
      def m: Long =
        val sec = ceil.getSeconds
        if sec >= MaxMinutes.getSeconds then MaxMinutes.getSeconds / 60
        else
          val minutes = sec / 60
          val err = sec - minutes*60
          if sec < 0 then
            if err == 0 && ceil.getNano > 0 then minutes + 1 else minutes
          else
            if err == 0 && ceil.getNano == 0 then minutes else minutes + 1
      def h: Long =
        val sec = ceil.getSeconds
        if sec >= MaxHours.getSeconds then MaxHours.getSeconds / 3600
        else
          val hours = sec / 3600
          val err = sec - hours*3600
          if sec < 0 then
            if err == 0 && ceil.getNano > 0 then hours + 1 else hours
          else
            if err == 0 && ceil.getNano == 0 then hours else hours + 1
      def d: Long =
        val sec = ceil.getSeconds
        if sec >= MaxDays.getSeconds then MaxDays.getSeconds / 86400
        else
          val dayz = sec / 86400
          val err = sec - dayz*86400
          if sec < 0 then
            if err == 0 && ceil.getNano > 0 then dayz + 1 else dayz
          else
            if err == 0 && ceil.getNano == 0 then dayz else dayz + 1        
      inline def days: Long = ceil.d
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
  // +(Instant) in OverloadedExtensions
  // +(LocalDateTime) in OverloadedExtensions
  // +(OffsetDateTime) in OverloadedExtensions
  // +(ZonedDateTime) in OverloadedExtensions
  // +(FileTime) in OverloadedExtensions
  // trunc in OverloadedExtensions

  def nano: kse.maths.NanoDuration =
    if d.getSeconds < 0 then
      if d.getSeconds == Long.MinValue then NanoDuration(Long.MinValue)
      else if d.getNano == 0 then NanoDuration(d.getSeconds *# 1000000000L)
      else NanoDuration((d.getSeconds + 1) *# 1000000000L +# (d.getNano - 1000000000L))
    else
      NanoDuration(d.getSeconds *# 1000000000L +# d.getNano)
  inline def D: kse.maths.DoubleDuration = DoubleDuration(d.getNano/1e9 + d.getSeconds)

  inline def into:  kse.maths.DurationCompanion.Into  = DurationCompanion.Into (d)
  inline def round: kse.maths.DurationCompanion.Round = DurationCompanion.Round(d)
  inline def floor: kse.maths.DurationCompanion.Floor = DurationCompanion.Floor(d)
  inline def ceil:  kse.maths.DurationCompanion.Ceil  = DurationCompanion.Ceil (d)
}
extension (inline t: Byte | Short | Int | Long | Float | Double) {
  transparent inline def days: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofDays(i)
    case d: Double => DoubleDuration(d * 86400)
    case _ => compiletime.error("Use .days on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.days or .toDouble.days to specify which.")

  transparent inline def day: Duration | DoubleDuration = inline t match
    case _: 1   => Duration.ofDays(1)
    case _: 1.0 => DoubleDuration(86400.0)
    case _      => compiletime.error("Use .day on 1 to get a Duration or on 1.0 to get a DoubleDuration.\nInput has neither type; use .toInt.days or .toDouble.days to specify which.")

  transparent inline def h: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofHours(i)
    case d: Double => DoubleDuration(d * 3600)
    case _ => compiletime.error("Use .h on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.hr or .toDouble.hr to specify which.")

  transparent inline def m: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofMinutes(i)
    case d: Double => DoubleDuration(d * 60)
    case _ => compiletime.error("Use .m on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.m or .toDouble.m to specify which.")

  transparent inline def s: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofSeconds(i)
    case d: Double => DoubleDuration(d)
    case _ => compiletime.error("Use .s on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.s or .toDouble.s to specify which.")

  transparent inline def ms: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofMillis(i)
    case d: Double => DoubleDuration(d / 1e3)
    case _ => compiletime.error("Use .ms on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.ms or .toDouble.ms to specify which.")

  transparent inline def us: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofNanos(i*1000L)
    case d: Double => DoubleDuration(d / 1e6)
    case _ => compiletime.error("Use .us on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.us or .toDouble.us to specify which.")

  transparent inline def ns: Duration | DoubleDuration = inline t match
    case i: Int => Duration.ofNanos(i)
    case d: Double => DoubleDuration(d / 1e9)
    case _ => compiletime.error("Use .ns on Int to get a Duration or on Double to get a DoubleDuration.\nInput has neither type; use .toInt.ns or .toDouble.ns to specify which.")
}

/*
case class CalendarDuration(months: Int, duration: Duration) {
  def +(cd: CalendarDuration) = new CalendarDuration(months +# cd.months, duration plus cd.duration)
  def +(md: MonthDuration) = new CalendarDuration(months +# md.unwrap, duration)
  def +(d: Duration) = new CalendarDuration(months, duration plus d)
  def -(cd: CalendarDuration) = new CalendarDuration(months -# cd.months, duration minus cd.duration)
  def -(md: MonthDuration) = new CalendarDuration(months -# md, duration)
  def -(d: Duration) = new CalendarDuation(months, duration minus d)
  def *(scale: Int) = new CalendarDuration(months *# scale, duration mulitpliedBy scale)
}

opaque type MonthDuration = Int
object MonthDuration {
  inline def apply(months: Int): MonthDuration = months
  inline def apply(years: Int, months: Int): MonthDuration = 12 *# years +# months

  extension (md: MonthDuration) {
    inline def unwrap: Long = md
  }

  extension (md: kse.maths.MonthDuration) {
    inline def +(cd: CalendarDuration): CalendarDuration = cd + md
    inline def +(d: kse.maths.MonthDuration): kse.maths.MonthDuration = MonthDuration(md.unwrap +# d.unwrap)
    inline def +(d: Duration): CalendarDuation = new CalendarDuration(md.unwrap, d)
    inline def -(cd: CalendarDuration): CalendarDuration = new CalendarDuration(md.unwrap -# cd.months, cd.duration.negated)
    inline def -(d: kse.maths.MonthDuration): kse.maths.MonthDuration = MonthDuration(md.unwrap -# d.unwrap)
    inline def -(d: Duration): CalendarDuration = new CalendarDuration(md.unwrap, duration.negated)
    @targetName("monthDuration_timesInt") inline def *(scale: Int) = MonthDuration(md.unwrap *# scale)
  }
}
extension (i: Int) {
  inline def months: kse.maths.MonthDuration = MonthDuration(i)
  inline def mo: kse.maths.MonthDuration = MonthDuration(i)
  inline def years: kse.maths.MonthDuration = MonthDuration(i *# 12)
  inline def yr: kse.maths.MonthDuration = MonthDuration(i *# 12)
}
*/

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

    inline def age: kse.maths.NanoDuration = NanoDuration(System.nanoTime - nt.unwrap)

    def pr: String =
      s"timestamp ${nt.unwrap} ns"
  }

  given Ordering[NanoInstant] = new {
    def compare(a: NanoInstant, b: NanoInstant) = a.unwrap compareTo b.unwrap
  }
}

opaque type NanoDuration = Long
object NanoDuration {
  inline def apply(nanos: Long): kse.maths.NanoDuration = nanos
  inline def since(nt: kse.maths.NanoInstant): kse.maths.NanoDuration = nt.age

  extension (dt: NanoDuration) {
    inline def unwrap: Long = dt
  }

  extension (dt: kse.maths.NanoDuration) {
    @targetName("duration_plus_duration")
    inline def +(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap +# et.unwrap)

    @targetName("duration_plus_instant")
    inline def +(nt: kse.maths.NanoInstant): kse.maths.NanoInstant = NanoInstant(nt.unwrap +# dt.unwrap)

    inline def -(et: kse.maths.NanoDuration): kse.maths.NanoDuration = NanoDuration(dt.unwrap -# et.unwrap)

    inline def unary_- : kse.maths.NanoDuration = NanoDuration(0L -# dt.unwrap)

    @targetName("long_mul")
    inline def *(factor: Long): kse.maths.NanoDuration = NanoDuration(dt.unwrap *# factor)

    @targetName("frac_mul")
    def *(frac: kse.maths.Frac): kse.maths.NanoDuration = NanoDuration(Frac.scaleClamped(dt.unwrap, frac))

    @targetName("long_div")
    inline def /(factor: Long): kse.maths.NanoDuration = NanoDuration(dt.unwrap /# factor)

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

      inline def into: kse.maths.NanoDuration.InCeil = ceil
    }
  }

  opaque type Trunc = Long
  object Trunc {
    extension (trunc: Trunc) {
      def us: kse.maths.NanoDuration = NanoDuration(1000L * (trunc/1000))

      def ms: kse.maths.NanoDuration = NanoDuration(1000000L * (trunc/1000000))

      def  s: kse.maths.NanoDuration = NanoDuration(1000000000L * (trunc/1000000000))
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
  inline def apply(t: Double): kse.maths.DoubleInstant = t
  inline def apply(i: Instant): kse.maths.DoubleInstant = i.getEpochSecond + i.getNano/1e9

  def from(seconds: Long, nanos: Int): kse.maths.DoubleInstant =
    if nanos == 0 then seconds.toDouble
    else if nanos % 1000000 == 0 && jm.abs(seconds) < 9223372036854775L then (1000*seconds + nanos/1000000)/1e3
    else seconds + nanos/1e9

  inline def now: kse.maths.DoubleInstant = apply(Instant.now)

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

    def age: kse.maths.DoubleDuration = DoubleDuration(DoubleInstant(Instant.now).unwrap - t)

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
  inline def apply(dt: Double): kse.maths.DoubleDuration = dt
  inline def apply(d: Duration): kse.maths.DoubleDuration = d.getSeconds + d.getNano/1e9
  inline def apply(n: kse.maths.NanoDuration): kse.maths.DoubleDuration = n.unwrap/1e9

  def since(dt: kse.maths.DoubleInstant): kse.maths.DoubleDuration = dt.age

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
      Duration.ofSeconds(t.toLong, if t > Long.MaxValue then 999999999 else n.toInt)

    inline def into: kse.maths.DoubleDuration.Into = dt.unwrap
    inline def long: kse.maths.DoubleDuration.InLong = dt.unwrap

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

      inline def long:  kse.maths.DoubleDuration.InLong  = in
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

      inline def into: kse.maths.DoubleDuration.InRound     = round
      inline def long: kse.maths.DoubleDuration.InLongRound = round
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

      inline def into: kse.maths.DoubleDuration.InFloor     = floor
      inline def long: kse.maths.DoubleDuration.InLongFloor = floor
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

      inline def into: kse.maths.DoubleDuration.InCeil     = ceil
      inline def long: kse.maths.DoubleDuration.InLongCeil = ceil
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

      inline def into: kse.maths.DoubleDuration.InTrunc = trunc
    }
  }

  opaque type InRound = Double
  object InRound {
    extension (round: InRound) {
      inline def ns: kse.maths.DoubleDuration = jm.rint(round * 1e9) / 1e9
      inline def us: kse.maths.DoubleDuration = jm.rint(round * 1e6) / 1e6
      inline def ms: kse.maths.DoubleDuration = jm.rint(round * 1e3) / 1e3
      inline def s:  kse.maths.DoubleDuration = jm.rint(round)
      inline def m:  kse.maths.DoubleDuration = jm.rint(round / 60) * 60
      inline def h:  kse.maths.DoubleDuration = jm.rint(round / 3600) * 3600
      inline def d:  kse.maths.DoubleDuration = jm.rint(round / 86400) * 86400

      inline def long: kse.maths.DoubleDuration.InLongRound = round
    }
  }

  opaque type InFloor = Double
  object InFloor {
    extension (floor: InFloor) {
      inline def ns: kse.maths.DoubleDuration = jm.floor(floor * 1e9) / 1e9
      inline def us: kse.maths.DoubleDuration = jm.floor(floor * 1e6) / 1e6
      inline def ms: kse.maths.DoubleDuration = jm.floor(floor * 1e3) / 1e3
      inline def s:  kse.maths.DoubleDuration = jm.floor(floor)
      inline def m:  kse.maths.DoubleDuration = jm.floor(floor / 60) * 60
      inline def h:  kse.maths.DoubleDuration = jm.floor(floor / 3600) * 3600
      inline def d:  kse.maths.DoubleDuration = jm.floor(floor / 86400) * 86400

      inline def long: kse.maths.DoubleDuration.InLongFloor = floor
    }
  }

  opaque type InCeil = Double
  object InCeil {
    extension (ceil: InCeil) {
      inline def ns: kse.maths.DoubleDuration = jm.ceil(ceil * 1e9) / 1e9
      inline def us: kse.maths.DoubleDuration = jm.ceil(ceil * 1e6) / 1e6
      inline def ms: kse.maths.DoubleDuration = jm.ceil(ceil * 1e3) / 1e3
      inline def s:  kse.maths.DoubleDuration = jm.ceil(ceil)
      inline def m:  kse.maths.DoubleDuration = jm.ceil(ceil / 60) * 60
      inline def h:  kse.maths.DoubleDuration = jm.ceil(ceil / 3600) * 3600
      inline def d:  kse.maths.DoubleDuration = jm.ceil(ceil / 86400) * 86400

      inline def long: kse.maths.DoubleDuration.InLongCeil = ceil
    }
  }

  opaque type InTrunc = Double
  object InTrunc {
    extension (trunc: InTrunc) {
      def ns: kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc * 1e9)   else jm.floor(trunc * 1e9) ) / 1e9
      def us: kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc * 1e6)   else jm.floor(trunc * 1e6) ) / 1e6
      def ms: kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc * 1e3)   else jm.floor(trunc * 1e3) ) / 1e3
      def s:  kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc)         else jm.floor(trunc) )
      def m:  kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc / 60)    else jm.floor(trunc / 60) ) * 60
      def h:  kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc / 3600)  else jm.floor(trunc / 3600) ) * 3600
      def d:  kse.maths.DoubleDuration = ( if trunc < 0 then jm.ceil(trunc / 86400) else jm.floor(trunc / 86400) ) * 86400
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

      inline def round: kse.maths.DoubleDuration.InLongRound = in
      inline def floor: kse.maths.DoubleDuration.InLongFloor = in
      inline def ceil:  kse.maths.DoubleDuration.InLongRound = in
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
