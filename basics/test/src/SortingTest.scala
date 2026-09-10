// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.basics


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{_, given}


// An opaque type with its own order, reversed, to show that the order supplied is the one used
object Rev {
  opaque type Rev = Int
  def apply(i: Int): Rev = i
  def arr(xs: Int*): Array[Rev] = xs.toArray
  extension (r: Rev) def value: Int = r
  given Translucent[Rev, Int] with {}
  object Ord extends Sorting.Total[Rev] {
    inline def leq(a: Rev, b: Rev): Boolean = a >= b
    val kernels = build()
  }
  given Ord.type = Ord
}

// A descending order for doubles, with NaN still last, in the three-line form
object DescDoubles extends Sorting.Partial[Double] {
  inline def leq(a: Double, b: Double): Boolean = a >= b
  val kernels = build()
}


@RunWith(classOf[JUnit4])
class SortingTest() {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import kse.basics.intervals._

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def bits(a: Array[Double]): Array[Long] = a.map(java.lang.Double.doubleToLongBits)

  @Test
  def sortingBasicsTest(): Unit =
    val a = Array(3, 1, 2, 1)
    T ~ a.indicesInOrder()           =**= Array(1, 3, 2, 0)
    T ~ a.indicesInOrder(1, 3)       =**= Array(1, 2)
    T ~ a.indicesInOrder(1 to 3)     =**= Array(1, 3, 2)
    T ~ a.indicesInOrder(Iv(1, 4))   =**= Array(1, 3, 2)
    T ~ a.indicesInOrder(1 to End)   =**= Array(1, 3, 2)
    T ~ Array[Int]().indicesInOrder() =**= Array[Int]()
    T ~ Array(5).indicesInOrder()     =**= Array(0)
    val ix = new Array[Int](8)
    val tmp = new Array[Int](8)
    T ~ a.indicesInOrder(0, 4, ix, tmp) ==== 4
    T ~ ix.take(4)                      =**= Array(1, 3, 2, 0)

    T ~ Array(3L, 1L, 2L, 1L).indicesInOrder()                       =**= Array(1, 3, 2, 0)
    T ~ Array(3.toShort, 1.toShort, 2.toShort, 1.toShort).indicesInOrder() =**= Array(1, 3, 2, 0)
    T ~ Array(3.toByte, 1.toByte, 2.toByte, 1.toByte).indicesInOrder()     =**= Array(1, 3, 2, 0)
    T ~ Array('c', 'a', 'b', 'a').indicesInOrder()                   =**= Array(1, 3, 2, 0)
    T ~ Array("cod", "eel", "bass", "cod").indicesInOrder()          =**= Array(2, 0, 3, 1)
    T ~ Array(Integer.valueOf(3), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(1)).indicesInOrder() =**= Array(1, 3, 2, 0)
    T ~ (summon[Sorting.Order[String]] eq Sorting.Order.Strings)     ==== true
    T ~ (summon[Sorting.Order[Integer]] eq summon[Sorting.Order[java.time.Instant]]) ==== true

    // Values that do not compare with themselves go last, in original order; -0.0 and 0.0 are equal, so stable
    val d = Array(3.0, Double.NaN, 1.0, Double.NaN, 2.0, -0.0, 0.0)
    T ~ d.indicesInOrder()               =**= Array(5, 6, 2, 4, 0, 1, 3)
    T ~ d.indicesInOrder(0, 7, ix, tmp)  ==== 5
    T ~ ix.take(7)                       =**= Array(5, 6, 2, 4, 0, 1, 3)
    T ~ d.indicesInOrder(1, 4)           =**= Array(2, 1, 3)
    T ~ Array(2f, Float.NaN, 1f).indicesInOrder() =**= Array(2, 0, 1)
    T ~ Array(Double.NaN, Double.NaN).indicesInOrder() =**= Array(0, 1)

    // An opaque type sorts by its own order
    T ~ Rev.arr(5, 4, 6, 5).indicesInOrder() =**= Array(2, 0, 3, 1)

    // Anything indexable, through an accessor
    val s = "salmon"
    T ~ Sorting.indicesInOrder(0, s.length)(i => s.charAt(i)) =**= Array(1, 2, 3, 5, 4, 0)
    T ~ Sorting.indexSort(0, 7, ix, tmp)(i => d(i))             ==== 5
    T ~ ix.take(7)                                              =**= Array(5, 6, 2, 4, 0, 1, 3)

  @Test
  def sortingValuesTest(): Unit =
    val v = Array(3, 1, 2, 1)
    v.sortInOrder()
    T ~ v =**= Array(1, 1, 2, 3)
    val w = Array(3, 1, 2, 1)
    T ~ w.sortWithIndices() =**= Array(1, 3, 2, 0)
    T ~ w                   =**= Array(1, 1, 2, 3)
    val x = Array(9, 3, 1, 2, 1, 0)
    x.sortInOrder(1, 5)
    T ~ x =**= Array(9, 1, 1, 2, 3, 0)
    val y = Array(9, 3, 1, 2, 1, 0)
    y.sortInOrder(1 to 3)
    T ~ y =**= Array(9, 1, 2, 3, 1, 0)
    val z = Array(9, 3, 1, 2, 1, 0)
    T ~ z.sortInOrder(0, 6, new Array[Int](6)) ==== 6
    T ~ z                               =**= Array(0, 1, 1, 2, 3, 9)
    val q = Array(9, 3, 1, 2, 1, 0)
    T ~ q.sortWithIndices(1, 5) =**= Array(2, 4, 3, 1)
    T ~ q                       =**= Array(9, 1, 1, 2, 3, 0)
    val ix = new Array[Int](8)
    val e = Array[Int]()
    e.sortInOrder()
    T ~ e.sortWithIndices() =**= Array[Int]()
    val one = Array(7)
    T ~ one.sortWithIndices() =**= Array(0)

    // NaN last in original order, -0.0 and 0.0 equal and so stable, seen through the indices
    val d = Array(3.0, Double.NaN, 1.0, Double.NaN, 2.0, -0.0, 0.0)
    val dv = d.clone()
    T ~ dv.sortWithIndices()  =**= Array(5, 6, 2, 4, 0, 1, 3)
    T ~ bits(dv)              =**= bits(Array(-0.0, 0.0, 1.0, 2.0, 3.0, Double.NaN, Double.NaN))
    val dw = d.clone()
    T ~ dw.sortWithIndices(0, 7, ix, new Array[Double](7), new Array[Int](7)) ==== 5
    T ~ ix.take(7)                                                            =**= Array(5, 6, 2, 4, 0, 1, 3)
    T ~ bits(dw)                                                              =**= bits(dv)
    val du = d.clone()
    T ~ du.sortInOrder(0, 7, new Array[Double](7)) ==== 5
    T ~ bits(du)                            =**= bits(dv)
    val f = Array(2f, Float.NaN, 1f)
    f.sortInOrder()
    T ~ f.map(java.lang.Float.floatToIntBits) =**= Array(1f, 2f, Float.NaN).map(java.lang.Float.floatToIntBits)

    val s = Array("cod", "eel", "bass", "cod")
    T ~ s.sortWithIndices() =**= Array(2, 0, 3, 1)
    T ~ s                   =**= Array("bass", "cod", "cod", "eel")
    val c = Array(Integer.valueOf(3), Integer.valueOf(1), Integer.valueOf(2))
    c.sortInOrder()
    T ~ c.map(_.intValue) =**= Array(1, 2, 3)
    val r = Rev.arr(5, 4, 6, 5)
    T ~ r.sortWithIndices() =**= Array(2, 0, 3, 1)
    T ~ r.map(_.value)      =**= Array(6, 5, 5, 4)
    val l = Array(3L, 1L, 2L)
    l.sortInOrder()
    T ~ l =**= Array(1L, 2L, 3L)
    val ch = Array('c', 'a', 'b')
    ch.sortInOrder()
    T ~ ch =**= Array('a', 'b', 'c')

  @Test
  def sortingReorderTest(): Unit =
    val a = Array(10, 20, 30, 40)
    val p = Array(2, 0, 3, 1)
    a.reorder(p)
    T ~ a =**= Array(30, 10, 40, 20)
    T ~ p =**= Array(2, 0, 3, 1)
    val b = Array(0, 10, 20, 30, 40, 50)
    b.reorder(Array(3, 1, 2), 1, 4)
    T ~ b =**= Array(0, 30, 10, 20, 40, 50)
    val c = Array(1, 2, 3)
    c.reorder(Array(0, 1, 2))
    T ~ c =**= Array(1, 2, 3)
    val d = Array(1, 2)
    d.reorder(Array(1, 0))
    T ~ d =**= Array(2, 1)
    val e = Array[Int]()
    e.reorder(Array[Int]())
    T ~ e =**= Array[Int]()
    val sb = new java.lang.StringBuilder("salmon")
    val ix = Sorting.indicesInOrder(0, 6)(i => sb.charAt(i))
    Sorting.reorder(ix, 0, 6)(i => sb.charAt(i))((i, ch) => sb.setCharAt(i, ch))
    T ~ sb.toString ==== "almnos"
    T ~ ix          =**= Array(1, 2, 3, 5, 4, 0)
    // Sorting one array by the index order of another
    val ages = Array(31, 25, 40, 25)
    val names = Array("eel", "cod", "gar", "ide")
    names.reorder(ages.indicesInOrder())
    T ~ names =**= Array("cod", "ide", "eel", "gar")

  @Test
  def sortingMemTest(): Unit =
    // Mem of an array shares its storage, so the array shows what the in-place operations did
    val a = Array(9, 3, 1, 2, 1, 0)
    val b = a.clone()
    val m = Mem of b
    T ~ m.indicesInOrder()     =**= Array(5L, 2L, 4L, 3L, 1L, 0L)
    T ~ m.indicesInOrder(1, 5) =**= Array(2L, 4L, 3L, 1L)
    val ix = new Array[Int](8)
    val tmp = new Array[Int](8)
    T ~ m.indicesInOrder(1, 5, ix, tmp) ==== 4
    T ~ ix.take(4)                      =**= Array(1, 3, 2, 0)
    m.sortInOrder(1, 5)
    T ~ b =**= Array(9, 1, 1, 2, 3, 0)
    T ~ m.sortWithIndices() =**= Array(5L, 1L, 2L, 3L, 4L, 0L)
    T ~ b                   =**= Array(0, 1, 1, 2, 3, 9)
    val keys = new Array[Int](8)
    val tmpK = new Array[Int](8)
    val c = Array(5, 4, 4, 3)
    T ~ (Mem of c).sortInOrder(0, 4, keys, tmpK) ==== 4
    T ~ c                                        =**= Array(3, 4, 4, 5)
    val c2 = Array(5, 4, 4, 3)
    T ~ (Mem of c2).sortWithIndices(0, 4, ix, keys, tmpK, tmp) ==== 4
    T ~ ix.take(4)                                             =**= Array(3, 1, 2, 0)
    T ~ c2                                                     =**= Array(3, 4, 4, 5)
    val empty = Array[Int]()
    T ~ (Mem of empty).indicesInOrder()  =**= Array[Long]()
    T ~ (Mem of empty).sortWithIndices() =**= Array[Long]()

    // Reordering by absolute indices, by absolute indices within a range, and by relative indices
    val d = Array(10, 20, 30, 40)
    val p = Array(2L, 0L, 3L, 1L)
    (Mem of d).reorder(p)
    T ~ d =**= Array(30, 10, 40, 20)
    T ~ p =**= Array(2L, 0L, 3L, 1L)
    val e = Array(0, 10, 20, 30, 40, 50)
    (Mem of e).reorder(Array(3L, 1L, 2L), 1, 4)
    T ~ e =**= Array(0, 30, 10, 20, 40, 50)
    val e2 = Array(0, 10, 20, 30, 40, 50)
    val rel = Array(2, 0, 1)
    (Mem of e2).reorder(rel, 1, 4)
    T ~ e2  =**= Array(0, 30, 10, 20, 40, 50)
    T ~ rel =**= Array(2, 0, 1)

    // NaN last, -0.0 and 0.0 stable, through Mem
    val dd = Array(3.0, Double.NaN, 1.0, Double.NaN, 2.0, -0.0, 0.0)
    val mdd = Mem of dd
    T ~ mdd.indicesInOrder() =**= Array(5L, 6L, 2L, 4L, 0L, 1L, 3L)
    T ~ mdd.sortInOrder(0, 7, new Array[Double](7), new Array[Double](7)) ==== 5
    T ~ bits(dd) =**= bits(Array(-0.0, 0.0, 1.0, 2.0, 3.0, Double.NaN, Double.NaN))

    // Mem.As over an opaque type sorts by that type's own (reversed) order, and over a primitive as Mem does
    val r = Rev.arr(5, 4, 6, 5)
    val mr = Mem.As of r
    T ~ mr.indicesInOrder()  =**= Array(2L, 0L, 3L, 1L)
    T ~ mr.sortWithIndices() =**= Array(2L, 0L, 3L, 1L)
    T ~ r.map(_.value)       =**= Array(6, 5, 5, 4)
    val r2 = Rev.arr(1, 3, 2)
    (Mem.As of r2).reorder(Array(1L, 2L, 0L))
    T ~ r2.map(_.value) =**= Array(3, 2, 1)
    val s = Array(2.0, Double.NaN, 1.0)
    val ms = Mem.As of s
    T ~ ms.indicesInOrder() =**= Array(2L, 0L, 1L)
    ms.sortInOrder()
    T ~ bits(s) =**= bits(Array(1.0, 2.0, Double.NaN))

    // Larger, against the array forms
    val rng = new java.util.Random(5551212L)
    val n = 1000
    val big = Array.fill(n)(rng.nextInt(50))
    T ~ (Mem of big.clone()).indicesInOrder() =**= big.indicesInOrder().map(_.toLong)
    val big2 = big.clone()
    T ~ (Mem of big2).sortWithIndices(100, 900) =**= Array.range(100, 900).sortBy(i => big(i)).map(_.toLong)
    T ~ big2.slice(100, 900) =**= big.slice(100, 900).sorted
    T ~ big2.take(100)       =**= big.take(100)
    T ~ big2.drop(900)       =**= big.drop(900)
    val big3 = big.clone()
    val order = (Mem of big3).indicesInOrder()
    (Mem of big3).reorder(order)
    T ~ big3 =**= big.sorted
    T ~ Sorting.rangeSize(5L, 5L + Int.MaxValue.toLong + 1L) ==== thrown[IllegalArgumentException]
    T ~ Sorting.rangeSize(7L, 3L)                            ==== 0

  inline def viewed[A <: Mem.Type](v: Mem.OrderAware[A])(using Mem.Order, scala.reflect.ClassTag[A]): Array[A] =
    Array.tabulate(v.length.toInt)(i => v(i))

  @Test
  def sortingOrderAwareTest(): Unit =
    // On this little-endian host a big-endian view reads each word byte-reversed, so the two orders differ
    val raw = Array(0x00000002, 0x01000000, 0x00000001)
    T ~ (Mem of raw).indicesInOrder() =**= Array(2L, 0L, 1L)
    val v = (Mem of raw).orderAware
    locally {
      import Mem.BE
      T ~ viewed(v)           =**= Array(0x02000000, 0x00000001, 0x01000000)
      T ~ v.indicesInOrder()  =**= Array(1L, 2L, 0L)
      val ix = new Array[Int](4)
      T ~ v.indicesInOrder(0, 3, ix, new Array[Int](4)) ==== 3
      T ~ ix.take(3)                                     =**= Array(1, 2, 0)
      T ~ v.sortWithIndices() =**= Array(1L, 2L, 0L)
      T ~ viewed(v)           =**= Array(0x00000001, 0x01000000, 0x02000000)
      T ~ raw                 =**= Array(0x01000000, 0x00000001, 0x00000002)

      // NaN last and -0.0 before 0.0 stably, all read and written through the swapped view
      val d = new Array[Double](7)
      val vd = (Mem of d).orderAware
      val src = Array(3.0, Double.NaN, 1.0, Double.NaN, 2.0, -0.0, 0.0)
      var i = 0
      while i < 7 do
        vd(i) = src(i)
        i += 1
      T ~ vd.indicesInOrder() =**= Array(5L, 6L, 2L, 4L, 0L, 1L, 3L)
      T ~ vd.sortInOrder(0, 7, new Array[Double](7), new Array[Double](7)) ==== 5
      T ~ bits(viewed(vd))    =**= bits(Array(-0.0, 0.0, 1.0, 2.0, 3.0, Double.NaN, Double.NaN))
      T ~ (d(0) == -0.0)      ==== false

      // Ranges and reorders, absolute and relative, under the byte order
      val w = new Array[Int](6)
      val vw = (Mem of w).orderAware
      i = 0
      while i < 6 do
        vw(i) = Array(9, 3, 1, 2, 1, 0)(i)
        i += 1
      T ~ vw.indicesInOrder(1, 5) =**= Array(2L, 4L, 3L, 1L)
      vw.sortInOrder(1, 5)
      T ~ viewed(vw) =**= Array(9, 1, 1, 2, 3, 0)
      vw.reorder(Array(5L, 0L, 1L, 2L, 3L, 4L))
      T ~ viewed(vw) =**= Array(0, 9, 1, 1, 2, 3)
      vw.reorder(Array(3L, 1L, 2L), 1, 4)
      T ~ viewed(vw) =**= Array(0, 1, 9, 1, 2, 3)
      vw.reorder(Array(1, 0), 2, 4)
      T ~ viewed(vw) =**= Array(0, 1, 1, 9, 2, 3)
    }
    locally {
      import Mem.Native
      T ~ v.indicesInOrder() =**= (Mem of raw).indicesInOrder()
      val u = (Mem of Array(5, 1, 4)).orderAware
      u.sortInOrder()
      T ~ viewed(u) =**= Array(1, 4, 5)
    }

  @Test
  def sortingCompiledOrderTest(): Unit =
    // A Partial order built by Total/Partial: descending, NaN last, through every kernel and the accessor path
    val d = Array(1.0, Double.NaN, 3.0, 2.0, 3.0)
    T ~ d.indicesInOrder()(using DescDoubles) =**= Array(2, 4, 3, 0, 1)
    T ~ Sorting.indicesInOrder(0, 5)(i => d(i))(using DescDoubles) =**= Array(2, 4, 3, 0, 1)
    val dv = d.clone()
    T ~ dv.sortWithIndices()(using DescDoubles) =**= Array(2, 4, 3, 0, 1)
    T ~ bits(dv) =**= bits(Array(3.0, 3.0, 2.0, 1.0, Double.NaN))
    val dw = d.clone()
    T ~ dw.sortInOrder(0, 5, new Array[Double](5))(using DescDoubles) ==== 4
    T ~ bits(dw) =**= bits(dv)
    T ~ DescDoubles.leqRt(1.0, 2.0) ==== false
    T ~ DescDoubles.leqRt(2.0, 1.0) ==== true
    T ~ DescDoubles.newKeys(3).length ==== 3
    T ~ (Mem of d.clone()).indicesInOrder()(using DescDoubles) =**= Array(2L, 4L, 3L, 0L, 1L)
    // A local given overrides the companion's order for the call sites in its scope
    locally {
      given DescDoubles.type = DescDoubles
      T ~ Array(1.0, 2.0, 3.0).indicesInOrder() =**= Array(2, 1, 0)
    }
    T ~ Array(1.0, 2.0, 3.0).indicesInOrder() =**= Array(0, 1, 2)
    // The compiler insists on the kernels line, and accepts the three-line form
    T ! """{ object Bad extends Sorting.Total[Int] { inline def leq(a: Int, b: Int): Boolean = a <= b }; Bad.leqRt(1, 2) }"""
    T \ """{ object Ok extends Sorting.Total[Int] { inline def leq(a: Int, b: Int): Boolean = a <= b; val kernels = build() }; Ok.leqRt(1, 2) }"""

  @Test
  def sortingByComparatorTest(): Unit =
    // Strings by length, then alphabetically, through a comparison on slots alone
    val s = Array("eel", "cod", "bass", "gar", "ide", "pike")
    val byLen = Sorting.indicesInOrderBy(0, s.length, false)((i, j) => s(i).length < s(j).length || (s(i).length == s(j).length && s(i) <= s(j)))
    T ~ byLen =**= Array(1, 0, 3, 4, 2, 5)
    T ~ Sorting.indicesInOrderBy(0, s.length, false)((i, j) => s(i).length <= s(j).length) =**= Array(0, 1, 3, 4, 2, 5)
    T ~ Sorting.indicesInOrderBy(2, 5, false)((i, j) => s(i) <= s(j)) =**= Array(2, 3, 4)
    // A partial comparison puts the slots that fail against themselves last
    val d = Array(3.0, Double.NaN, 1.0, Double.NaN, 2.0)
    T ~ Sorting.indicesInOrderBy(0, 5, true)((i, j) => d(i) <= d(j)) =**= Array(2, 4, 0, 1, 3)
    val ix = new Array[Int](5)
    T ~ Sorting.indexSortBy(0, 5, ix, new Array[Int](5))((i, j) => d(i) <= d(j), true) ==== 3
    T ~ ix =**= Array(2, 4, 0, 1, 3)
    T ~ Sorting.indicesInOrderBy(0, 0, false)((i, j) => true) =**= Array[Int]()
    // Larger, against the key-based sort, with heavy ties
    val rng = new java.util.Random(31337L)
    val n = 1500
    val a = Array.fill(n)(rng.nextInt(40))
    T ~ Sorting.indicesInOrderBy(0, n, false)((i, j) => a(i) <= a(j)) =**= a.indicesInOrder()
    T ~ Sorting.indicesInOrderBy(0, n, true)((i, j) => a(i) <= a(j))  =**= a.indicesInOrder()
    val b = Array.fill(n)(rng.nextInt(40))
    val ref = Array.range(0, n).sortBy(i => (a(i), b(i)))
    T ~ Sorting.indicesInOrderBy(0, n, false)((i, j) => a(i) < a(j) || (a(i) == a(j) && b(i) <= b(j))) =**= ref

    // An array of structs sorted by one field, by two, and with a NaN field last, without touching a record
    val xs = Mem.AoS.alloc[(name: Int, score: Double)](6)
    val names = Array(3, 1, 2, 1, 3, 2)
    val scores = Array(0.5, 2.0, Double.NaN, 1.0, 0.5, 4.0)
    var i = 0
    while i < 6 do
      xs.name(i) = names(i)
      xs.score(i) = scores(i)
      i += 1
    T ~ xs.indicesInOrderBy()((i, j) => xs.name(i) <= xs.name(j))   =**= Array(1L, 3L, 2L, 5L, 0L, 4L)
    T ~ xs.indicesInOrderBy()((i, j) => xs.score(i) <= xs.score(j)) =**= Array(0L, 4L, 3L, 1L, 5L, 2L)
    // The NaN-scored record fails the two-field comparison against itself, so it goes last rather than staying with its name
    T ~ xs.indicesInOrderBy()((i, j) => xs.name(i) < xs.name(j) || (xs.name(i) == xs.name(j) && xs.score(i) <= xs.score(j))) =**= Array(3L, 1L, 5L, 0L, 4L, 2L)
    T ~ xs.indicesInOrderBy(1, 5)((i, j) => xs.name(i) <= xs.name(j)) =**= Array(1L, 3L, 2L, 4L)
    val rx = new Array[Int](6)
    T ~ xs.indicesInOrderBy(1, 5, rx, new Array[Int](6))((i, j) => xs.score(i) <= xs.score(j)) ==== 3
    T ~ rx.take(4) =**= Array(3, 2, 0, 1)
    T ~ xs.name(2) ==== 2
    T ~ xs.score(2).isNaN ==== true
    locally {
      import Mem.BE
      // Through the order-aware twin the same records read byte-swapped, so the order is by the swapped values
      val v = xs.orderAware
      val swapped = names.map(Integer.reverseBytes)
      T ~ v.indicesInOrderBy()((i, j) => v.name(i) <= v.name(j)) =**= Array.range(0, 6).sortBy(i => swapped(i)).map(_.toLong)
    }

    // Records follow an index order in place, moved whole, one held aside at a time
    val two = xs.indicesInOrderBy()((i, j) => xs.name(i) < xs.name(j) || (xs.name(i) == xs.name(j) && xs.score(i) <= xs.score(j)))
    xs.reorder(two)
    T ~ Array.tabulate(6)(i => xs.name(i))        =**= Array(1, 1, 2, 3, 3, 2)
    T ~ bits(Array.tabulate(6)(i => xs.score(i))) =**= bits(Array(1.0, 2.0, 4.0, 0.5, 0.5, Double.NaN))
    T ~ two                                       =**= Array(3L, 1L, 5L, 0L, 4L, 2L)
    // A range, with relative indices from the buffered form and a supplied scratch record
    val scratch = Mem.Struct.of[(name: Int, score: Double)]
    val rix2 = new Array[Int](6)
    T ~ xs.indicesInOrderBy(2, 6, rix2, new Array[Int](6))((i, j) => xs.score(i) <= xs.score(j)) ==== 3
    T ~ rix2.take(4) =**= Array(1, 2, 0, 3)
    xs.reorder(rix2, 2, 6, scratch)
    T ~ Array.tabulate(6)(i => xs.name(i))        =**= Array(1, 1, 3, 3, 2, 2)
    T ~ bits(Array.tabulate(6)(i => xs.score(i))) =**= bits(Array(1.0, 2.0, 0.5, 0.5, 4.0, Double.NaN))
    T ~ rix2.take(4)                              =**= Array(1, 2, 0, 3)
    // Absolute indices within a range
    xs.reorder(Array(4L, 3L, 2L), 2, 5)
    T ~ Array.tabulate(6)(i => xs.name(i))        =**= Array(1, 1, 2, 3, 3, 2)
    T ~ bits(Array.tabulate(6)(i => xs.score(i))) =**= bits(Array(1.0, 2.0, 4.0, 0.5, 0.5, Double.NaN))
    // The order-aware twin moves records as bytes, whatever the order in scope
    locally {
      import Mem.BE
      xs.orderAware.reorder(Array(5L, 4L, 3L, 2L, 1L, 0L))
    }
    T ~ Array.tabulate(6)(i => xs.name(i))        =**= Array(2, 3, 3, 2, 1, 1)
    T ~ bits(Array.tabulate(6)(i => xs.score(i))) =**= bits(Array(Double.NaN, 0.5, 0.5, 4.0, 2.0, 1.0))
    // Many records, against an array reference
    val big = Mem.AoS.alloc[(key: Int, tag: Long)](1200)
    val bk = Array.fill(1200)(rng.nextInt(30))
    i = 0
    while i < 1200 do
      big.key(i) = bk(i)
      big.tag(i) = i.toLong
      i += 1
    val bo = big.indicesInOrderBy()((i, j) => big.key(i) <= big.key(j))
    T ~ bo =**= Array.range(0, 1200).sortBy(i => bk(i)).map(_.toLong)
    big.reorder(bo)
    T ~ Array.tabulate(1200)(i => big.tag(i)) =**= bo
    T ~ Array.tabulate(1200)(i => big.key(i)) =**= bk.sorted

  @Test
  def sortingStressTest(): Unit =
    val rng = new java.util.Random(8675309L)
    val sizes = Array(2, 3, 15, 16, 17, 31, 32, 33, 47, 64, 100, 255, 256, 257, 1000, 1023, 1024, 1025, 5000)
    var si = 0
    while si < sizes.length do
      val n = sizes(si)
      si += 1
      // Few distinct values, so ties are everywhere and stability is tested throughout
      val a = Array.fill(n)(rng.nextInt(1 + n/7))
      val ref = Array.range(0, n).sortBy(i => a(i))
      T ~ a.indicesInOrder() =**= ref
      T ~ a.indicesInOrder(n/3, n - n/5) =**= Array.range(n/3, n - n/5).sortBy(i => a(i))
      val asc = Array.range(0, n).map(_ / 3)
      T ~ asc.indicesInOrder() =**= Array.range(0, n)
      val desc = Array.tabulate(n)(i => (n - i) / 3)
      T ~ desc.indicesInOrder() =**= Array.range(0, n).sortBy(i => desc(i))
      val d = Array.fill(n){ val x = rng.nextInt(1 + n/7); if x % 11 == 3 then Double.NaN else x.toDouble }
      val nan = Array.range(0, n).filter(i => d(i).isNaN)
      val dref = Array.range(0, n).filter(i => !d(i).isNaN).sortBy(i => d(i))(using Ordering.Double.TotalOrdering) ++ nan
      T ~ d.indicesInOrder() =**= dref
      val ix = new Array[Int](n)
      T ~ d.indicesInOrder(0, n, ix, new Array[Int](n)) ==== n - nan.length
      T ~ ix =**= dref
      val l = a.map(x => x.toLong - 3)
      T ~ l.indicesInOrder() =**= ref
      val str = a.map(x => "fish" + x)
      T ~ str.indicesInOrder() =**= Array.range(0, n).sortBy(i => str(i))

      // Value sorts agree with the index sorts and with a stable reference
      val v = a.clone()
      T ~ v.sortWithIndices() =**= ref
      T ~ v                   =**= a.select(ref)
      val v2 = a.clone()
      v2.sortInOrder()
      T ~ v2 =**= v
      val v3 = a.clone()
      v3.sortInOrder(n/3, n - n/5)
      T ~ v3.take(n/3)              =**= a.take(n/3)
      T ~ v3.slice(n/3, n - n/5)    =**= a.slice(n/3, n - n/5).sorted
      T ~ v3.drop(n - n/5)          =**= a.drop(n - n/5)
      val v4 = a.clone()
      T ~ v4.sortWithIndices(n/3, n - n/5) =**= Array.range(n/3, n - n/5).sortBy(i => a(i))
      T ~ v4 =**= v3
      val dv = d.clone()
      T ~ dv.sortWithIndices() =**= dref
      T ~ bits(dv)             =**= bits(dref.map(i => d(i)))
      val dv2 = d.clone()
      T ~ dv2.sortInOrder(0, n, new Array[Double](n)) ==== n - nan.length
      T ~ bits(dv2)                            =**= bits(dv)
      val sv = str.clone()
      sv.sortInOrder()
      T ~ sv =**= str.sorted
      val lv = l.clone()
      T ~ lv.sortWithIndices() =**= ref
      T ~ lv                   =**= l.select(ref)
      val av = asc.clone()
      av.sortInOrder()
      T ~ av =**= asc
      val dsv = desc.clone()
      dsv.sortInOrder()
      T ~ dsv =**= desc.sorted

      // Reordering by an index order matches select, and leaves the indices intact
      val r = a.clone()
      r.reorder(ref)
      T ~ r   =**= a.select(ref)
      T ~ ref =**= Array.range(0, n).sortBy(i => a(i))
      val perm = Array.range(0, n)
      var k = n - 1
      while k > 0 do
        val j = rng.nextInt(k + 1)
        val t = perm(k)
        perm(k) = perm(j)
        perm(j) = t
        k -= 1
      val r2 = str.clone()
      r2.reorder(perm)
      T ~ r2 =**= str.select(perm)
      val r3 = a.clone()
      val sub = Array.range(n/3, n - n/5).sortBy(i => a(i))
      r3.reorder(sub, n/3, n - n/5)
      T ~ r3 =**= v3
}
