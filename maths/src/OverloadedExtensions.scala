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

  /////////////////////////////////////
  // Time operators (Temporal.scala) //
  /////////////////////////////////////
  @targetName("Long_mul_NanoDuration")
  inline def *(nd: NanoDuration): NanoDuration = nd * value
}



extension (value: Float) {
  ////////////////////////////
  // trunc from Maths.scala //
  ////////////////////////////
  @targetName("float_trunc")
  inline def trunc = if value < 0 then jm.ceil(value) else jm.floor(value)


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
  ////////////////////////////
  // trunc from Maths.scala //
  ////////////////////////////
  @targetName("float_trunc")
  inline def trunc = if value < 0 then jm.ceil(value) else jm.floor(value)
}



extension (d: Duration) {
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

