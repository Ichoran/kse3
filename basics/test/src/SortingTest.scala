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
  object Ord extends Sorting.Order[Rev] {
    inline def leq(a: Rev, b: Rev): Boolean = a >= b
    inline def partial: Boolean = false
    def leqRt(a: Rev, b: Rev): Boolean = a >= b
    def newKeys(n: Int): Array[Rev] = new Array[Rev](n)
    def indexSort(keys: Array[Rev], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
      Sorting.indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
    def sort(keys: Array[Rev], i0: Int, iN: Int, tmp: Array[Rev]): Int =
      Sorting.valueSortImpl(keys, i0, iN, Sorting.noIndices, tmp, Sorting.noIndices)((a, b) => leq(a, b), partial, false)
    def sortIx(keys: Array[Rev], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Rev], tmpIx: Array[Int]): Int =
      Sorting.valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
  }
  given Ord.type = Ord
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
