// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

/*
This file "shouldn't" exist.

Because Scala 3 doesn't have first-class method overloading, and because extension methods
are just bare method names, not parameterized by the type of the thing you're extending,
you have to define all "overloaded" extensions in the same package in the same file.

Even worse, dispatch-type-plus-method-name shadows even across packages, so even the
packages that are supposed to be optional add-ins get pulled in here.  Which means that
we shouldn't have them be optional at all.  So no .temporal for the time stuff.  Sorry!

Annoying, but at least it's possible.
*/

package kse.maths

import java.lang.{Math => jm}
import java.time._
import java.util.concurrent.TimeUnit
import java.nio.file.attribute.FileTime

import scala.annotation.targetName



extension (value: Byte) {
  /////////////////////////////////
  // Overflow-throwing operators //
  /////////////////////////////////
  def +!(b: Byte): Byte =
    val x = value + b
    if x < -128 || x > 127 then throw new ArithmeticException("byte overflow")
    else x.toByte
  def -!(b: Byte): Byte =
    val x = value - b
    if x < -128 || x > 127 then throw new ArithmeticException("byte overflow")
    else x.toByte
  def *!(b: Byte): Byte =
    val x = value * b
    if x < -128 || x > 127 then throw new ArithmeticException("byte overflow")
    else x.toByte
  def /!(b: Byte): Byte =
    if b == -1 && value == Byte.MinValue then throw new ArithmeticException("byte overflow")
    else (value/b).toByte

  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  inline def clamp(lo: Byte, hi: Byte) =
    if lo <= value then
      if value <= hi then value
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Byte, hi: Byte) = lo <= value && value <= hi
  inline def checkIn(lo: Byte, hi: Byte) =
    if value < lo || value > hi then throw new ArithmeticException("byte out of range")
    else value

}



extension (value: Short) {
  /////////////////////////////////
  // Overflow-throwing operators //
  /////////////////////////////////
  def +!(s: Short): Short =
    val x = value + s
    if x < -32768 || x > 32767 then throw new ArithmeticException("short overflow")
    else x.toShort
  def -!(s: Short): Short =
    val x = value - s
    if x < -32768 || x > 32767 then throw new ArithmeticException("short overflow")
    else x.toShort
  def *!(s: Short): Short =
    val x = value * s
    if x < -32768 || x > 32767 then throw new ArithmeticException("short overflow")
    else x.toShort
  def /!(s: Short): Short =
    if s == -1 && value == Short.MinValue then throw new ArithmeticException("short overflow")
    else (value/s).toShort

  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  inline def clamp(lo: Short, hi: Short) =
    if lo <= value then
      if value <= hi then value
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Short, hi: Short) = lo <= value && value <= hi
  inline def checkIn(lo: Short, hi: Short): Short =
    if value < lo || value > hi then throw new ArithmeticException("short out of range")
    else value
}



extension (value: Char) {
  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  inline def clamp(lo: Char, hi: Char) =
    if lo <= value then
      if value <= hi then value
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Char, hi: Char) = lo <= value && value <= hi
  inline def checkIn(lo: Char, hi: Char) =
    if value < lo || value > hi then throw new ArithmeticException("char out of range")
    else value
}



extension (value: Int) {
  /////////////////////////////////
  // Overflow-throwing operators //
  /////////////////////////////////
  inline def +!(i: Int) = jm.addExact(value, i)
  inline def -!(i: Int) = jm.subtractExact(value, i)
  inline def *!(i: Int) = jm.multiplyExact(value, i)
  inline def /!(i: Int) =
    if i == -1 && value == Int.MinValue then throw new ArithmeticException("int overflow")
    else value / i

  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  inline def clamp(lo: Int, hi: Int) =
    if lo <= value then
      if value <= hi then value
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Int, hi: Int) = lo <= value && value <= hi
  inline def checkIn(lo: Int, hi: Int) =
    if value < lo || value > hi then throw new ArithmeticException("int out of range")
    else value

  ////////////////////////////////////////
  // Int _ Frac Operators (Maths.scala) //
  ////////////////////////////////////////
  @targetName("Int_add_Frac")
  inline def +(f: kse.maths.Frac): kse.maths.Frac = f + value

  @targetName("Int_sub_Frac")
  inline def -(f: kse.maths.Frac): kse.maths.Frac =
    (-f) + value

  @targetName("Int_mul_Frac")
  inline def *(f: kse.maths.Frac): kse.maths.Frac =
    f * value

  @targetName("Int_div_Frac")
  inline def /(f: kse.maths.Frac): kse.maths.Frac =
    Frac.divide(value, f)


  /////////////////////////////////////
  // Time operators (Temporal.scala) //
  /////////////////////////////////////
  @targetName("Int_mul_NanoDuration")
  inline def *(nd: NanoDuration): NanoDuration = nd * value.toLong

  @targetName("Int_mul_Duration")
  inline def *(d: Duration): Duration = DurationCompanion.mul(d, value)
}



extension (value: Long) {
  /////////////////////////////////
  // Overflow-throwing operators //
  /////////////////////////////////
  inline def +!(l: Long): Long = jm.addExact(value, l)
  inline def -!(l: Long): Long = jm.subtractExact(value, l)
  inline def *!(l: Long): Long = jm.multiplyExact(value, l)
  inline def /!(l: Long): Long =
    if l == -1 && value == Long.MinValue then throw new ArithmeticException("long overflow")
    else value / l

  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  inline def clamp(lo: Long, hi: Long) =
    if lo <= value then
      if value <= hi then value
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Long, hi: Long) = lo <= value && value <= hi
  inline def checkIn(lo: Long, hi: Long) =
    if value < lo || value > hi then throw new ArithmeticException("long overflow")
    else value

  /////////////////////////////////////
  // Time operators (Temporal.scala) //
  /////////////////////////////////////
  @targetName("Long_mul_NanoDuration")
  inline def *(nd: NanoDuration): NanoDuration = nd * value
}



extension (value: Float) {
  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  @targetName("float_trunc")
  inline def trunc = if value < 0 then jm.ceil(value) else jm.floor(value)

  inline def clamp(lo: Float, hi: Float) =
    if lo <= value && value <= hi then value
    else if value < lo then lo
    else if value > hi then
      if lo <= hi then hi
      else lo
    else Float.NaN

  inline def in(lo: Float, hi: Float) = lo <= value && value <= hi

  final def closeTo(that: Float, abstol: Float, fractol: Float): Boolean = 
    jm.abs(value - that) match
      case x if x <= abstol =>
        val big = jm.max(jm.abs(value), jm.abs(that))
        big <= 1 || x <= big*fractol
      case _ => false
  inline final def closeTo(that: Float): Boolean = closeTo(that, 1e-6f, 1e-6f)


  ////////////////////////////////////////
  // Float _ Vc Operators (Maths.scala) //
  ////////////////////////////////////////
  @targetName("Float_add_Vc")
  inline def +(v: kse.maths.Vc): kse.maths.Vc = Vc(value + v.x, value + v.y)

  @targetName("Float_sub_Vc")
  inline def -(v: kse.maths.Vc): kse.maths.Vc = Vc(value - v.x, value - v.y)

  @targetName("Float_mul_Vc")
  inline def *(v: kse.maths.Vc): kse.maths.Vc = Vc(value * v.x, value * v.y)


  ///////////////////////////////////////////////
  // Float _ PlusMinus Operators (Maths.scala) //
  ///////////////////////////////////////////////
  @targetName("Float_add_PlusMinus")
  inline def +(pm: kse.maths.PlusMinus): kse.maths.PlusMinus = pm.valueTo(value + pm.value)

  @targetName("Float_sub_PlusMinus")
  inline def -(pm: kse.maths.PlusMinus): kse.maths.PlusMinus = pm.valueTo(value - pm.value)

  @targetName("Float_mul_PlusMinus")
  inline def *(pm: kse.maths.PlusMinus): kse.maths.PlusMinus = PlusMinus(value * pm.value, value * pm.error)

  @targetName("Float_div_PlusMinus")
  inline def /(pm: kse.maths.PlusMinus): kse.maths.PlusMinus =
    val v = pm.value.toDouble
    val u = value.toDouble
    val r = 1.0/v
    PlusMinus.D(u*r, pm.error.toDouble*u*r*r)
}



extension (value: Double) {
  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  @targetName("float_trunc")
  inline def trunc = if value < 0 then jm.ceil(value) else jm.floor(value)

  inline def clamp(lo: Double, hi: Double) =
    if lo <= value && value <= hi then value
    else if value < lo then lo
    else if value > hi then
      if lo <= hi then hi
      else lo
    else Double.NaN

  inline def in(lo: Double, hi: Double) = lo <= value && value <= hi

  final def closeTo(that: Double, abstol: Double = 1e-12, fractol: Double = 1e-12) = 
    jm.abs(value - that) match
      case x if x <= abstol =>
        val big = jm.max(jm.abs(value), jm.abs(that))
        big <= 1 || x <= big*fractol
      case _ => false
}



extension (d: Duration) {
  /////////////////////////
  // Range-aware methods //
  /////////////////////////
  def clamp(early: Duration, late: Duration): Duration =
    if d.compareTo(early) >= 0 then
      if d.compareTo(late) <= 0 then d
      else if early.compareTo(late) <= 0 then late
      else early
    else early

  inline def in(early: Duration, late: Duration): Boolean = d.compareTo(early) >= 0 && d.compareTo(late) <= 0

  inline def checkIn(early: Duration, late: Duration): Duration =
    if d.compareTo(early) >= 0 && d.compareTo(late) <= 0 then d
    else throw new ArithmeticException("Duration out of range")


  /////////////////////////////////////////////
  // Duration arithmetic from Temporal.scala //
  /////////////////////////////////////////////
  @targetName("Duration_add_Duration")
  inline def +(dd: Duration): Duration = DurationCompanion.add(d, dd)

  @targetName("Duration_sub_Duration")
  inline def -(dd: Duration): Duration = DurationCompanion.sub(d, dd)

  @targetName("Duration_mul_Int")
  inline def *(scale: Int): Duration = DurationCompanion.mul(d, scale)

  @targetName("Duration_mul_Frac")
  inline def *(frac: Frac): Duration = DurationCompanion.mul(d, frac)

  @targetName("Duration_div_Int")
  inline def /(factor: Int): Duration = DurationCompanion.div(d, factor)

  @targetName("Duration_div_Frac")
  inline def /(frac: Frac): Duration = DurationCompanion.mul(d, frac.reciprocal)

  @targetName("Duration_div_Duration")
  inline def /(duration: Duration): Long = DurationCompanion.div(d, duration, exact = false)

  @targetName("Duration_mod_Duration")
  inline def %(duration: Duration): Duration = DurationCompanion.mod(d, duration)

  @targetName("Duration_add_Instant")
  def +(i: Instant): Instant =
    try i plus d
    catch
      case _: DateTimeException => if d.getSeconds < 0 then Instant.MIN else Instant.MAX

  @targetName("Duration_add_LocalDateTime")
  def +(ldt: LocalDateTime): LocalDateTime =
    try ldt plus d
    catch
      case _: DateTimeException => if d.getSeconds < 0 then LocalDateTime.MIN else LocalDateTime.MAX

  @targetName("Duration_add_OffsetDateTime")
  def +(odt: OffsetDateTime): OffsetDateTime =
    try odt plus d
    catch
      case _: DateTimeException => OffsetDateTime.of(if d.getSeconds < 0 then LocalDateTime.MIN else LocalDateTime.MAX, odt.getOffset)

  @targetName("Duration_add_ZonedDateTime")
  def +(zdt: ZonedDateTime): ZonedDateTime =
    try zdt plus d
    catch
      case _: DateTimeException => ZonedDateTime.of(if d.getSeconds < 0 then LocalDateTime.MIN else LocalDateTime.MAX, zdt.getZone)

  @targetName("Duration_add_FileTime")
  def +(ft: FileTime): FileTime =
    DurationCompanion.robustAddition(ft, d, subtract = false)

  inline def +!(dd: Duration): Duration = d plus dd
  inline def -!(dd: Duration): Duration = d minus dd
  inline def *!(scale: Int): Duration = d multipliedBy scale
  inline def /!(factor: Int): Duration = d dividedBy factor
  inline def /!(duration: Duration): Long = DurationCompanion.div(d, duration, exact = true)

  inline def +!(i: Instant): Instant = i plus d
  inline def +!(ldt: LocalDateTime): LocalDateTime = ldt plus d
  inline def +!(odt: OffsetDateTime): OffsetDateTime = odt plus d
  inline def +!(zdt: ZonedDateTime): ZonedDateTime = zdt plus d
  inline def +!(ft: FileTime): FileTime = DurationCompanion.exactAddition(ft, d, subtract = false)


  ////////////////////////////////////////
  // Duration trunc from Temporal.scala //
  ////////////////////////////////////////
  @targetName("Duration_trunc")
  inline def trunc: kse.maths.DurationCompanion.Trunc = DurationCompanion.Trunc(d)
}

