// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.

package kse.maths


import java.lang.{Math => jm}

import kse.maths._

final case class Frame1D(title: String, data: Array[Double]) {}


/*
final class Frame1D[A] private (val size: Int, preload: Array[Double] | (Array[Int], Array[Double]) = null) {
  private[maths] var content: Array[Double] | (Array[Int], Array[Double]) = preload match
    case null => Frame1D.emptySparse
    case ad: Array[Double] =>
      if ad.length != size throw new IllegalArgumentException(s"Input size ${ad.length} does not match Frame size $size")
      ad
    case aiad: (Array[Int], Array[Double]) => Frame1D.verifySparse(aiad)

      val (ai, ad) = aiad
      if ai.length == 0 && ad.length == 0 then aiad
      else
        Frame1D.verifiedSparse(ai, ad)
        if !(ai.length - 1 == ad.length) then throw new IllegalArgumentException(s"Mismatch in sparse array: 1+${ai.length-1} indices but ${ad.length} data points")

}
object Frame1D {
  val noDoubles = Array.empty[Double]
  val noInts = Array.empty[Int]
  val emptySparse = (noInts, noDoubles)

  def verifySparse(ai: )
}


final class Frame2D[A, B] private (val rowsize: Int, val colsize: Int, preload: Content = null) {
  private var content: Content =
    if preload eq null then Array[(Array[Int], Array[Double])].fill(rowsize)(emptySparse)
}
object Frame {
  type Content = Array[Double] | Array[Array[Double] | (Array[Int], Array[Double])]

}
*/
