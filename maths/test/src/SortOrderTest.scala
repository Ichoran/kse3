// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.maths


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._


@RunWith(classOf[JUnit4])
class SortOrderTest() {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import kse.basics.{given, _}
  import kse.maths.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  @Test
  def opaqueOrderTest(): Unit =
    // Unsigned types sort unsigned: values with the top bit set are the largest, though negative underneath
    val ub = Array(UByte(200), UByte(5), UByte(127), UByte(255))
    T ~ ub.indicesInOrder() =**= Array(1, 2, 0, 3)
    ub.sortInOrder()
    T ~ ub.map(_.signed & 0xFF) =**= Array(5, 127, 200, 255)
    val us = Array(UShort(40000.toShort), UShort(9), UShort(32767))
    T ~ us.indicesInOrder() =**= Array(1, 2, 0)
    val ui = Array(UInt(0xF0000000), UInt(1), UInt(0x7FFFFFFF), UInt(0))
    T ~ ui.indicesInOrder() =**= Array(3, 1, 2, 0)
    ui.sortInOrder()
    T ~ ui.map(_.signed) =**= Array(0, 1, 0x7FFFFFFF, 0xF0000000)
    val ul = Array(ULong(-1L), ULong(1L), ULong(0L))
    T ~ ul.indicesInOrder() =**= Array(2, 1, 0)
    T ~ ul.sortWithIndices() =**= Array(2, 1, 0)
    T ~ ul.map(_.signed)     =**= Array(0L, 1L, -1L)

    // Types with NaN put it last
    val h = Array(Bf16(2f), Bf16.NaN, Bf16(1f), Bf16(-1f))
    T ~ h.indicesInOrder() =**= Array(3, 2, 0, 1)
    val dd = Array(DoubleDuration(2.0), DoubleDuration(Double.NaN), DoubleDuration(1.0))
    T ~ dd.indicesInOrder() =**= Array(2, 0, 1)
    dd.sortInOrder()
    T ~ dd(0)              ==== DoubleDuration(1.0)
    T ~ dd(1)              ==== DoubleDuration(2.0)
    T ~ dd(2).unwrap.isNaN ==== true

    // The rest sort by their own order
    val nd = Array(NanoDuration(30L), NanoDuration(-10L), NanoDuration(20L))
    T ~ nd.indicesInOrder() =**= Array(1, 2, 0)
    val ni = Array(NanoInstant(912835798134L), NanoInstant(1L), NanoInstant(912835798000L))
    T ~ ni.indicesInOrder() =**= Array(1, 2, 0)
    val di = Array(DoubleInstant(3.0), DoubleInstant(1.0), DoubleInstant(2.0))
    T ~ di.indicesInOrder() =**= Array(1, 2, 0)
    val fr = Array(Frac(1, 2), Frac(1, 3), Frac(2, 3), Frac(-18, 35))
    T ~ fr.indicesInOrder() =**= Array(3, 1, 0, 2)
    fr.sortInOrder()
    T ~ fr(0) ==== Frac(-18, 35)
    T ~ fr(3) ==== Frac(2, 3)
}
