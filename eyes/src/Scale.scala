// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


/** Which family of scale an aesthetic value belongs to.  Decided at compile time from the
  * value's type via `ScaleOf`; never re-inferred from runtime data.
  */
enum ScaleKind:
  case Continuous, Discrete, Temporal, Identity


/** Compile-time evidence of how a value type maps onto a scale.  This is the heart of the
  * categorical-vs-continuous story: `color = s.region` (String) resolves a discrete scale,
  * `color = s.temp` (Double) a continuous one, and mismatches are caught at interpretation
  * with the kinds already known.  Users may provide instances for their own types.
  */
sealed trait ScaleOf[V]:
  def kind: ScaleKind

object ScaleOf:
  /** Values that map onto a continuous axis via a Double. */
  trait AsContinuous[V] extends ScaleOf[V]:
    final def kind = ScaleKind.Continuous
    def toDouble(v: V): Double

  /** Values that form discrete levels, labeled for display; `ordered` if the levels have an
    * inherent order (otherwise level order is a presentation policy).
    */
  trait AsDiscrete[V] extends ScaleOf[V]:
    final def kind = ScaleKind.Discrete
    def label(v: V): String
    def ordered: Boolean = false

  /** Continuous time: values map to (fractional) seconds since the epoch, so tick logic can
    * be calendar-aware while columns stay packed doubles.
    */
  trait AsTemporal[V] extends ScaleOf[V]:
    final def kind = ScaleKind.Temporal
    def epochSeconds(v: V): Double

  /** Pre-resolved values (e.g. literal colours): no scale is fit and no guide is generated. */
  trait AsIdentity[V] extends ScaleOf[V]:
    final def kind = ScaleKind.Identity

  given ScaleOf.AsContinuous[Double] = d => d
  given ScaleOf.AsContinuous[Float] = f => f.toDouble
  given ScaleOf.AsContinuous[Long] = l => l.toDouble
  given ScaleOf.AsContinuous[Int] = i => i.toDouble  // continuous by default; .discrete modifier is the planned escape (DESIGN 14)

  given ScaleOf.AsDiscrete[String] = s => s
  given ScaleOf.AsDiscrete[Boolean] = b => if b then "true" else "false"
  given ScaleOf.AsDiscrete[Char] = c => c.toString

  given ScaleOf.AsTemporal[java.time.Instant] = t => t.getEpochSecond.toDouble + t.getNano*1e-9
  given ScaleOf.AsTemporal[java.time.LocalDate] = d => d.toEpochDay*86400.0
  given ScaleOf.AsTemporal[java.time.LocalDateTime] = dt => dt.toEpochSecond(java.time.ZoneOffset.UTC).toDouble + dt.getNano*1e-9

  given ScaleOf.AsIdentity[kse.maths.colours.Rgb] = new ScaleOf.AsIdentity[kse.maths.colours.Rgb] {}
  given ScaleOf.AsIdentity[kse.maths.colours.Argb] = new ScaleOf.AsIdentity[kse.maths.colours.Argb] {}
