// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import kse.flow._

object JsonGenericParser {
  private[jsonal] val smallPowersOfTen = Array.tabulate(23)(i => s"1e$i".toDouble)

  private[jsonal] val yesNull = Yes(kse.jsonal.Json.Null)
  private[jsonal] val yesTrue = Yes(kse.jsonal.Json.Bool.True)
  private[jsonal] val yesFalse = Yes(kse.jsonal.Json.Bool.False)

  private[jsonal] val wouldNotFitInDouble = JastError("Text number would not fit in a Double")
}
