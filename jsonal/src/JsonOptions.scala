// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import kse.flow._
import kse.maths.packed._

sealed trait JsonOptions extends Copy[JsonOptions] {
  def lossy: Boolean
  def trim: Boolean
  def complete: Boolean
  def outcome: Option[JsonOptions.Outcome]
}
object JsonOptions {
  final class Outcome(var complete: Boolean = false, var error: Boolean = false, var consumed: Long = 0L) extends Copy[Outcome] {
    override def equals(that: Any) = that match
      case outcome: Outcome => complete == outcome.complete && error == outcome.error && consumed == outcome.consumed
      case _ => false
    override def hashCode: Int =
      import scala.util.hashing.MurmurHash3._
      val a = mix(0x1981523, (if (complete) 1 else 0) + (if (error) 2 else 0))
      val b = mix(a, consumed.asInts.i1)
      val c = mixLast(b, consumed.asInts.i0)
      finalizeHash(c, 3)
    override def toString = s"Outcome(complete = $complete, error = $error, consumed = $consumed)"

    def copy = new Outcome(complete, error, consumed)
  }

  private[JsonOptions] final case class Impl(lossy: Boolean, trim: Boolean, complete: Boolean, outcome: Option[JsonOptions.Outcome])
  extends JsonOptions {
    def copy: JsonOptions = outcome match
      case Some(x) => Impl(lossy, trim, complete, Some(x.copy))
      case _       => this
  }

  val Default: JsonOptions = Impl(false, false, false, None)
  val Lossy: JsonOptions = Impl(true, false, false, None)
  val Trim: JsonOptions = Impl(false, true, false, None)
  val Complete: JsonOptions = Impl(false, false, true, None)
  val LossyTrim: JsonOptions = Impl(true, true, false, None)
  val LossyComplete: JsonOptions = Impl(true, false, true, None)
  val TrimComplete: JsonOptions = Impl(false, true, true, None)
  val LossyTrimComplete: JsonOptions = Impl(true, true, true, None)

  def withOutcome(lossy: Boolean, trim: Boolean, complete: Boolean): JsonOptions = Impl(lossy, trim, complete, Some(new Outcome()))

  def apply(lossy: Boolean, trim: Boolean, complete: Boolean) =
    if (lossy) { 
      if (trim) { if (complete) LossyTrimComplete else LossyTrim }
      else { if (complete) LossyComplete else Lossy }
    }
    else { 
      if (trim) { if (complete) TrimComplete else Trim }
      else { if (complete) Complete else Default }
    }
}
