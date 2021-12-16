// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2021 Rex Kerr and Calico Life Sciences, LLC.


package kse.jsonal

import java.time._

import scala.util.control.NonFatal

import kse.flow._
import kse.maths._


class AnyJsonDate private (
  private[jsonal] val i: Instant, 
  private[jsonal] val ldt: LocalDateTime, 
  private[jsonal] val odt: OffsetDateTime, 
  private[jsonal] val zdt: ZonedDateTime
) extends AsJson {
  def isInstant = i ne null
  def isLocal = ldt ne null
  def isOffset = odt ne null
  def isZoned = zdt ne null
  def instant = if (i ne null)   i     else if (odt ne null) odt.instant else if (zdt ne null) zdt.instant else ldt.instant
  def local   = if (ldt ne null) ldt   else if (i ne null)   i.local     else if (odt ne null) odt.local   else zdt.local
  def utc     = if (i ne null)   i.utc else if (odt ne null) odt.utc     else if (zdt ne null) zdt.utc     else ldt.utc
  def zoned   = if (zdt ne null) zdt   else if (i ne null)   i.zoned     else if (odt ne null) odt.zoned   else ldt.zoned
  def offset  = zoned.toOffsetDateTime

  def json =
    if      (i   ne null) i.json
    else if (ldt ne null) ldt.json
    else if (odt ne null) odt.json
    else if (zdt ne null) zdt.json
    else                  Json.Null

  override def toString =
    if      (i ne null)   i.toString
    else if (ldt ne null) ldt.toString
    else if (odt ne null) odt.toString
    else if (zdt ne null) zdt.toString
    else                  "(not a date)"

  override def equals(that: Any) = that match
    case x: AnyJsonDate =>
      if (i ne null) i == x.i
      else (x.i eq null) && (
        if (ldt ne null) ldt == x.ldt
        else (x.ldt eq null) && (
          if (odt ne null) odt == x.odt
          else (x.odt eq null) && zdt == x.zdt
        )
      )
    case y: SimpleJsonDate =>
      if (i ne null) i == y.i
      else (y.i eq null) && (
        if (ldt ne null) ldt == y.ldt
        else (y.ldt eq null) && (odt eq null) && (zdt eq null)
      )
    case _ => false

  override def hashCode =
    if      (i ne null)   i.hashCode
    else if (ldt ne null) ldt.hashCode
    else if (odt ne null) odt.hashCode
    else if (zdt ne null) zdt.hashCode
    else                  912385718  // Completely arbitrary
}
object AnyJsonDate {
  def apply(i: Instant)          = new AnyJsonDate(i,    null, null, null)
  def apply(ldt: LocalDateTime)  = new AnyJsonDate(null, ldt,  null, null)
  def apply(odt: OffsetDateTime) = new AnyJsonDate(null, null, odt,  null)
  def apply(zdt: ZonedDateTime)  = new AnyJsonDate(null, null, null, zdt)

  given FromJson[AnyJsonDate] = new {
    def apply(j: Json): Jast.To[AnyJsonDate] = JsonConversion.fromStr(j)("date"){ s =>
      val text = s.text
      if (JsonConversion.patternForInstant.matcher(text).matches)
        try { return Yes(AnyJsonDate.apply(java.time.Instant.parse(text))) }
        catch { case t if NonFatal(t) => }
      if (JsonConversion.patternForLocalDateTime.matcher(text).matches)
        try { return Yes(AnyJsonDate.apply(java.time.LocalDateTime.parse(text))) }
        catch { case t if NonFatal(t) => }
      if (JsonConversion.patternForOffsetDateTime.matcher(text).matches)
        try { return Yes(AnyJsonDate.apply(java.time.OffsetDateTime.parse(text))) }
        catch { case t if NonFatal(t) => }
      if (JsonConversion.patternForZonedDateTime.matcher(text).matches)
        try { return Yes(AnyJsonDate.apply(java.time.ZonedDateTime.parse(text))) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(s"Expected date but format not compatible: ${JsonConversion.quote(text, 36)}")
    }
  }
}

class SimpleJsonDate private (
  private[jsonal] val i: Instant, 
  private[jsonal] val ldt: LocalDateTime
) extends AsJson {
  def isInstant = i ne null
  def isLocal = ldt ne null
  def instant = if (i ne null)   i   else ldt.instant
  def local   = if (ldt ne null) ldt else i.local

  def json =
    if      (i   ne null) i.json
    else if (ldt ne null) ldt.json
    else                  Json.Null

  override def toString =
    if      (i ne null)   i.toString
    else if (ldt ne null) ldt.toString
    else                  "(not a date)"

  override def equals(that: Any) = that match
    case x: SimpleJsonDate =>
      if (i ne null) i == x.i
      else (x.i eq null) && ldt == x.ldt
    case y: AnyJsonDate =>
      if (i ne null) i == y.i
      else (y.i eq null) && (
        if (ldt ne null) ldt == y.ldt
        else (y.ldt eq null) && (y.odt eq null) && (y.zdt eq null)
      )
    case _ => false

  override def hashCode =
    if      (i ne null)   i.hashCode
    else if (ldt ne null) ldt.hashCode
    else                  912385718  // Completely arbitrary
}
object SimpleJsonDate {
  def apply(i: Instant)          = new SimpleJsonDate(i,    null)
  def apply(ldt: LocalDateTime)  = new SimpleJsonDate(null, ldt)

  given FromJson[SimpleJsonDate] = new {
    def apply(j: Json): Jast.To[SimpleJsonDate] = JsonConversion.fromStr(j)("simple date"){ s =>
      val text = s.text
      if (JsonConversion.patternForInstant.matcher(text).matches)
        try { return Yes(SimpleJsonDate.apply(java.time.Instant.parse(text))) }
        catch { case t if NonFatal(t) => }
      if (JsonConversion.patternForLocalDateTime.matcher(text).matches)
        try { return Yes(SimpleJsonDate.apply(java.time.LocalDateTime.parse(text))) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(s"Expected simple date but format not compatible: ${JsonConversion.quote(text, 29)}")
    }
  }
}
