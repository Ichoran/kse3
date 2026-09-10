// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-26 Rex Kerr

package kse.basics


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.foreign.MemorySegment

import scala.annotation.targetName
import scala.reflect.ClassTag

import scala.collection.immutable.{Range => Rg}

import kse.basics.intervals._


/** Stable sorting, by index or by value.
  *
  * The primary operation is an index sort: given keys reachable by index, produce the indices in ascending key order,
  * ties keeping their original order, without moving the keys.  Comparison is supplied by a `Sorting.Order`, whose
  * comparison is `inline` so that every sort kernel is compiled with the comparison in place--no boxing, no virtual
  * call per comparison, for primitives, opaque types with their own `<=`, and `Comparable` objects alike.
  *
  * Arrays also have a value sort, `sortInOrder`, which is faster because it moves keys and indices together in sequential
  * passes rather than reaching back into the array for every comparison; it sorts in place, and can be given a
  * scratch buffer so repeated sorts allocate nothing.  `reorder` applies an index order in place to anything with
  * get and set, by walking the permutation's cycles, so data that must stay put can still be rearranged.
  *
  * Arrays have `indicesInOrder`; anything else indexable goes through `Sorting.indexSort` with an accessor, which
  * compiles the kernel at the call site (into its own method), so keep such call sites few.
  */
object Sorting {
  /** How keys of type `A` compare for sorting, and the sort kernels compiled for that comparison.
    *
    * `leq` is `inline` and abstract, so an implementation must be an object (or a final class) whose `leq` is itself
    * `inline`, and a given must be declared with that singleton type, e.g. `given Ints.type = Ints`.  A given declared
    * as `Order[Int]` will be rejected wherever its comparison is needed, which is deliberate: it cannot quietly fall back
    * to a boxed virtual call.  The sort kernels are compiled once per `Order`, inside `indexSort`, `sort` and `sortIx`,
    * from `Sorting.indexSortImpl` and `Sorting.valueSortImpl` with `leq` in place; an implementation for a new type
    * is those three one-liners plus `leq`, `partial`, `leqRt` and `newKeys`, as in `Order.Ints`.
    *
    * `leq` must be a total preorder on the keys that satisfy `leq(k, k)`; if `partial` is `true`, keys that fail
    * `leq(k, k)` (e.g. `NaN`) are allowed and sort after all others, in their original order.
    */
  trait Order[A] {
    /** `true` if `a` sorts no later than `b`. */
    inline def leq(a: A, b: A): Boolean

    /** `true` if some keys fail `leq(k, k)`; such keys are sorted to the end in original order. */
    inline def partial: Boolean

    /** `leq` as an ordinary method, for generic code that cannot inline; boxes primitives when called through `Order[A]`. */
    def leqRt(a: A, b: A): Boolean

    /** A new array of `n` keys, for use as a scratch buffer. */
    def newKeys(n: Int): Array[A]

    /** Fills `ix(0 until iN - i0)` with the indices `i0 until iN` arranged so `keys` are ascending, ties in original
      * order; `tmp` is scratch.  Both must hold at least `iN - i0` entries.  Returns how many keys compare (all of
      * them unless `partial`); the rest are at the end of the filled part of `ix` in original order.
      */
    def indexSort(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int

    /** Sorts `keys(i0 until iN)` in place, ties in original order, using `tmp` (at least `iN - i0` long) as scratch.
      * Returns how many keys compare; the rest are at the end, in original order.
      */
    def sort(keys: Array[A], i0: Int, iN: Int, tmp: Array[A]): Int

    /** As `sort`, and also fills `ix(0 until iN - i0)` with the original index of each key in its sorted position,
      * using `tmpIx` (at least `iN - i0` long) as scratch.
      */
    def sortIx(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[A], tmpIx: Array[Int]): Int
  }

  /** The `Comparable` fallback lives here so that any given in `Order` itself is preferred to it. */
  trait OrderLowPriority {
    /** Natural order of anything `Comparable`; one instance serves every such type, so its scratch keys are `Object`s. */
    final class Comparables[A <: Comparable[? >: A]] extends Order[A] {
      inline def leq(a: A, b: A): Boolean = a.compareTo(b) <= 0
      inline def partial: Boolean = false
      def leqRt(a: A, b: A): Boolean = a.compareTo(b) <= 0
      def newKeys(n: Int): Array[A] = (new Array[Comparable[?]](n)).asInstanceOf[Array[A]]
      def indexSort(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[A], i0: Int, iN: Int, tmp: Array[A]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[A], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    private val theComparables = new Comparables[Nothing]
    given comparables[A <: Comparable[? >: A]]: Comparables[A] = theComparables.asInstanceOf[Comparables[A]]
  }
  object Order extends OrderLowPriority {
    object Ints extends Order[Int] {
      inline def leq(a: Int, b: Int): Boolean = a <= b
      inline def partial: Boolean = false
      def leqRt(a: Int, b: Int): Boolean = a <= b
      def newKeys(n: Int): Array[Int] = new Array[Int](n)
      def indexSort(keys: Array[Int], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Int], i0: Int, iN: Int, tmp: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Int], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given ints: Ints.type = Ints

    object Longs extends Order[Long] {
      inline def leq(a: Long, b: Long): Boolean = a <= b
      inline def partial: Boolean = false
      def leqRt(a: Long, b: Long): Boolean = a <= b
      def newKeys(n: Int): Array[Long] = new Array[Long](n)
      def indexSort(keys: Array[Long], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Long], i0: Int, iN: Int, tmp: Array[Long]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Long], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Long], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given longs: Longs.type = Longs

    object Doubles extends Order[Double] {
      inline def leq(a: Double, b: Double): Boolean = a <= b
      inline def partial: Boolean = true
      def leqRt(a: Double, b: Double): Boolean = a <= b
      def newKeys(n: Int): Array[Double] = new Array[Double](n)
      def indexSort(keys: Array[Double], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Double], i0: Int, iN: Int, tmp: Array[Double]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Double], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Double], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given doubles: Doubles.type = Doubles

    object Floats extends Order[Float] {
      inline def leq(a: Float, b: Float): Boolean = a <= b
      inline def partial: Boolean = true
      def leqRt(a: Float, b: Float): Boolean = a <= b
      def newKeys(n: Int): Array[Float] = new Array[Float](n)
      def indexSort(keys: Array[Float], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Float], i0: Int, iN: Int, tmp: Array[Float]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Float], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Float], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given floats: Floats.type = Floats

    object Shorts extends Order[Short] {
      inline def leq(a: Short, b: Short): Boolean = a <= b
      inline def partial: Boolean = false
      def leqRt(a: Short, b: Short): Boolean = a <= b
      def newKeys(n: Int): Array[Short] = new Array[Short](n)
      def indexSort(keys: Array[Short], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Short], i0: Int, iN: Int, tmp: Array[Short]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Short], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Short], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given shorts: Shorts.type = Shorts

    object Bytes extends Order[Byte] {
      inline def leq(a: Byte, b: Byte): Boolean = a <= b
      inline def partial: Boolean = false
      def leqRt(a: Byte, b: Byte): Boolean = a <= b
      def newKeys(n: Int): Array[Byte] = new Array[Byte](n)
      def indexSort(keys: Array[Byte], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Byte], i0: Int, iN: Int, tmp: Array[Byte]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Byte], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Byte], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given bytes: Bytes.type = Bytes

    object Chars extends Order[Char] {
      inline def leq(a: Char, b: Char): Boolean = a <= b
      inline def partial: Boolean = false
      def leqRt(a: Char, b: Char): Boolean = a <= b
      def newKeys(n: Int): Array[Char] = new Array[Char](n)
      def indexSort(keys: Array[Char], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[Char], i0: Int, iN: Int, tmp: Array[Char]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[Char], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Char], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given chars: Chars.type = Chars

    object Strings extends Order[String] {
      inline def leq(a: String, b: String): Boolean = a.compareTo(b) <= 0
      inline def partial: Boolean = false
      def leqRt(a: String, b: String): Boolean = a.compareTo(b) <= 0
      def newKeys(n: Int): Array[String] = new Array[String](n)
      def indexSort(keys: Array[String], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int =
        indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial)
      def sort(keys: Array[String], i0: Int, iN: Int, tmp: Array[String]): Int =
        valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false)
      def sortIx(keys: Array[String], i0: Int, iN: Int, ix: Array[Int], tmp: Array[String], tmpIx: Array[Int]): Int =
        valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    }
    given strings: Strings.type = Strings
  }

  /** The kernels of an `Order`, as `Total` and `Partial` build them from an inline `leq`. */
  final class Kernels[A](
    leqFn: (A, A) => Boolean,
    newKeysFn: Int => Array[A],
    indexSortFn: (Array[A], Int, Int, Array[Int], Array[Int]) => Int,
    sortFn: (Array[A], Int, Int, Array[A]) => Int,
    sortIxFn: (Array[A], Int, Int, Array[Int], Array[A], Array[Int]) => Int
  ) {
    def leqRt(a: A, b: A): Boolean = leqFn(a, b)
    def newKeys(n: Int): Array[A] = newKeysFn(n)
    def indexSort(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int = indexSortFn(keys, i0, iN, ix, tmp)
    def sort(keys: Array[A], i0: Int, iN: Int, tmp: Array[A]): Int = sortFn(keys, i0, iN, tmp)
    def sortIx(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[A], tmpIx: Array[Int]): Int = sortIxFn(keys, i0, iN, ix, tmp, tmpIx)
  }

  /** An `Order` that compiles its own kernels from its `leq`.  An implementation is an object defining
    * `inline def leq` and `val kernels = build()`, and nothing else: `build` is inline, so called in the object's own
    * body it sees that object's `leq` and stamps the kernels with it in place, and the abstract `kernels` makes the
    * compiler insist on that line.  `Total` and `Partial` supply `partial`.  The one difference from a hand-written
    * `Order` is that `leqRt` goes through a function value, which boxes for `Float`, `Short`, `Byte`, `Char` and
    * opaque types over them; override it if that path matters.
    */
  abstract class Compiled[A](using ClassTag[A]) extends Order[A] {
    protected val kernels: Kernels[A]

    /** Compiles this order's kernels; call it exactly as `val kernels = build()` in the object body. */
    protected inline def build(): Kernels[A] = new Kernels[A](
      (a, b) => leq(a, b),
      n => new Array[A](n),
      (keys, i0, iN, ix, tmp) => indexSortImpl(i0, iN, ix, tmp)(keys(_))((a, b) => leq(a, b), partial),
      (keys, i0, iN, tmp) => valueSortImpl(keys, i0, iN, noIndices, tmp, noIndices)((a, b) => leq(a, b), partial, false),
      (keys, i0, iN, ix, tmp, tmpIx) => valueSortImpl(keys, i0, iN, ix, tmp, tmpIx)((a, b) => leq(a, b), partial, true)
    )

    def leqRt(a: A, b: A): Boolean = kernels.leqRt(a, b)
    def newKeys(n: Int): Array[A] = kernels.newKeys(n)
    def indexSort(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int]): Int = kernels.indexSort(keys, i0, iN, ix, tmp)
    def sort(keys: Array[A], i0: Int, iN: Int, tmp: Array[A]): Int = kernels.sort(keys, i0, iN, tmp)
    def sortIx(keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[A], tmpIx: Array[Int]): Int = kernels.sortIx(keys, i0, iN, ix, tmp, tmpIx)
  }

  /** An order under which every key compares with itself.  Define `inline def leq` and `val kernels = build()`. */
  abstract class Total[A](using ClassTag[A]) extends Compiled[A] {
    inline def partial: Boolean = false
  }

  /** An order under which some keys (e.g. `NaN`) fail `leq(k, k)` and so sort last.  Define `inline def leq` and
    * `val kernels = build()`.
    */
  abstract class Partial[A](using ClassTag[A]) extends Compiled[A] {
    inline def partial: Boolean = true
  }


  /** Length of the blocks that are sorted by binary insertion before merging starts. */
  inline val MinRun = 16

  /** Stands in for an index array in a kernel that is not tracking indices. */
  val noIndices: Array[Int] = new Array[Int](0)

  /** The kernel of the index sort, as a template.  Fills `ix(0 until iN - i0)` with the indices `i0 until iN` in
    * ascending order of `key`, ties in original order, using `tmp` (at least `iN - i0` long) as scratch.  Returns how
    * many keys compare; when `partial`, keys with `!leq(k, k)` come after them in `ix`, in original order.
    *
    * Every use compiles a copy with `key` and `leq` in place, so use it only to build an `Order`'s `indexSort` or,
    * through `Sorting.indexSort`, at a few call sites that read keys from something other than an array.
    */
  inline def indexSortImpl[A](i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int])(inline key: Int => A)(inline leq: (A, A) => Boolean, inline partial: Boolean): Int =
    val n = iN - i0
    var nc = 0
    inline if partial then
      var ni = 0
      var i = i0
      while i < iN do
        val k = key(i)
        if leq(k, k) then
          ix(nc) = i
          nc += 1
        else
          tmp(ni) = i
          ni += 1
        i += 1
      if ni > 0 then System.arraycopy(tmp, 0, ix, nc, ni)
    else
      while nc < n do
        ix(nc) = i0 + nc
        nc += 1
    if nc > 1 then
      var lo = 0
      while lo < nc do
        val hi = if nc - lo < MinRun then nc else lo + MinRun
        var i = lo + 1
        while i < hi do
          val p = ix(i)
          val k = key(p)
          var a = lo
          var b = i
          while a < b do
            val m = (a + b) >>> 1
            if leq(key(ix(m)), k) then a = m + 1
            else b = m
          if a < i then
            System.arraycopy(ix, a, ix, a + 1, i - a)
            ix(a) = p
          i += 1
        lo = hi
      var width = MinRun
      var src = ix
      var dst = tmp
      while width < nc do
        lo = 0
        while lo < nc do
          val mid = if nc - lo < width then nc else lo + width
          val hi = if nc - mid < width then nc else mid + width
          var i = lo
          var j = mid
          var k = lo
          if mid < hi && !leq(key(src(mid - 1)), key(src(mid))) then
            var ki = key(src(i))
            var kj = key(src(j))
            while i < mid && j < hi do
              if leq(ki, kj) then
                dst(k) = src(i)
                i += 1
                if i < mid then ki = key(src(i))
              else
                dst(k) = src(j)
                j += 1
                if j < hi then kj = key(src(j))
              k += 1
          if i < mid then
            System.arraycopy(src, i, dst, k, mid - i)
            k += mid - i
          if j < hi then System.arraycopy(src, j, dst, k, hi - j)
          lo = hi
        val t = src
        src = dst
        dst = t
        width <<= 1
        if width <= 0 then width = nc
      if src ne ix then System.arraycopy(src, 0, ix, 0, nc)
    nc

  /** The kernel of the value sort, as a template.  Sorts `keys(i0 until iN)` in place, ties in original order, using
    * `tmp` (at least `iN - i0` long) as scratch; when `withIx`, also fills `ix(0 until iN - i0)` with each key's
    * original index, using `tmpIx` as scratch, and otherwise never touches `ix` or `tmpIx`.  Returns how many keys
    * compare; when `partial`, keys with `!leq(k, k)` come after them, in original order.
    *
    * Every use compiles a copy with `leq` in place, so use it only to build an `Order`'s `sort` and `sortIx`.
    */
  inline def valueSortImpl[A](keys: Array[A], i0: Int, iN: Int, ix: Array[Int], tmp: Array[A], tmpIx: Array[Int])(inline leq: (A, A) => Boolean, inline partial: Boolean, inline withIx: Boolean): Int =
    val n = iN - i0
    var nc = 0
    inline if partial then
      var ni = 0
      var i = i0
      while i < iN do
        val k = keys(i)
        if leq(k, k) then
          keys(i0 + nc) = k
          inline if withIx then ix(nc) = i
          nc += 1
        else
          tmp(ni) = k
          inline if withIx then tmpIx(ni) = i
          ni += 1
        i += 1
      if ni > 0 then
        System.arraycopy(tmp, 0, keys, i0 + nc, ni)
        inline if withIx then System.arraycopy(tmpIx, 0, ix, nc, ni)
    else
      nc = n
      inline if withIx then
        var i = 0
        while i < n do
          ix(i) = i0 + i
          i += 1
    if nc > 1 then
      var lo = 0
      while lo < nc do
        val hi = if nc - lo < MinRun then nc else lo + MinRun
        var i = lo + 1
        while i < hi do
          val k = keys(i0 + i)
          var a = lo
          var b = i
          while a < b do
            val m = (a + b) >>> 1
            if leq(keys(i0 + m), k) then a = m + 1
            else b = m
          if a < i then
            System.arraycopy(keys, i0 + a, keys, i0 + a + 1, i - a)
            keys(i0 + a) = k
            inline if withIx then
              val p = ix(i)
              System.arraycopy(ix, a, ix, a + 1, i - a)
              ix(a) = p
          i += 1
        lo = hi
      var width = MinRun
      var src = keys
      var srcOff = i0
      var srcIx = ix
      var dst = tmp
      var dstOff = 0
      var dstIx = tmpIx
      while width < nc do
        lo = 0
        while lo < nc do
          val mid = if nc - lo < width then nc else lo + width
          val hi = if nc - mid < width then nc else mid + width
          var i = lo
          var j = mid
          var k = lo
          if mid < hi && !leq(src(srcOff + mid - 1), src(srcOff + mid)) then
            var ki = src(srcOff + i)
            var kj = src(srcOff + j)
            while i < mid && j < hi do
              if leq(ki, kj) then
                dst(dstOff + k) = ki
                inline if withIx then dstIx(k) = srcIx(i)
                i += 1
                if i < mid then ki = src(srcOff + i)
              else
                dst(dstOff + k) = kj
                inline if withIx then dstIx(k) = srcIx(j)
                j += 1
                if j < hi then kj = src(srcOff + j)
              k += 1
          if i < mid then
            System.arraycopy(src, srcOff + i, dst, dstOff + k, mid - i)
            inline if withIx then System.arraycopy(srcIx, i, dstIx, k, mid - i)
            k += mid - i
          if j < hi then
            System.arraycopy(src, srcOff + j, dst, dstOff + k, hi - j)
            inline if withIx then System.arraycopy(srcIx, j, dstIx, k, hi - j)
          lo = hi
        val t = src
        src = dst
        dst = t
        val to = srcOff
        srcOff = dstOff
        dstOff = to
        val ti = srcIx
        srcIx = dstIx
        dstIx = ti
        width <<= 1
        if width <= 0 then width = nc
      if src ne keys then
        System.arraycopy(src, 0, keys, i0, nc)
        inline if withIx then System.arraycopy(srcIx, 0, ix, 0, nc)
    nc

  /** Index sort of keys read by `key` for indices `i0 until iN`, into `ix` with scratch `tmp` (each at least
    * `iN - i0` long).  Returns how many keys compare; see `Order.indexSort`.  The kernel is compiled here, in a method
    * of its own, with `key` and the order's comparison in place.
    */
  inline def indexSort[A](i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int])(inline key: Int => A)(using ord: Order[A]): Int =
    def sortWork(): Int = indexSortImpl(i0, iN, ix, tmp)(key)((a, b) => ord.leq(a, b), ord.partial)
    sortWork()

  /** Indices `i0 until iN` in ascending order of `key`, ties in original order, in a new array.  Keys that do not
    * compare come last.  The kernel is compiled here; see `indexSort`.
    */
  inline def indicesInOrder[A](i0: Int, iN: Int)(inline key: Int => A)(using ord: Order[A]): Array[Int] =
    val n = iN - i0
    val ix = new Array[Int](n)
    indexSort(i0, iN, ix, new Array[Int](n))(key) __ Unit
    ix

  /** The kernel of the index sort by a comparison on slots, as a template.  As `indexSortImpl`, but `leqAt(i, j)`
    * says whether whatever is at slot `i` sorts no later than whatever is at slot `j`, so no key is ever extracted;
    * when `partial`, a slot with `!leqAt(i, i)` comes after all others, in original order.  A merge step compares
    * afresh on every comparison, since there are no key values to hold.
    *
    * Every use compiles a copy with `leqAt` in place, so prefer `Sorting.indexSortBy`, which puts it in a method of
    * its own.
    */
  inline def indexSortByImpl(i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int])(inline leqAt: (Int, Int) => Boolean, inline partial: Boolean): Int =
    val n = iN - i0
    var nc = 0
    inline if partial then
      var ni = 0
      var i = i0
      while i < iN do
        if leqAt(i, i) then
          ix(nc) = i
          nc += 1
        else
          tmp(ni) = i
          ni += 1
        i += 1
      if ni > 0 then System.arraycopy(tmp, 0, ix, nc, ni)
    else
      while nc < n do
        ix(nc) = i0 + nc
        nc += 1
    if nc > 1 then
      var lo = 0
      while lo < nc do
        val hi = if nc - lo < MinRun then nc else lo + MinRun
        var i = lo + 1
        while i < hi do
          val p = ix(i)
          var a = lo
          var b = i
          while a < b do
            val m = (a + b) >>> 1
            if leqAt(ix(m), p) then a = m + 1
            else b = m
          if a < i then
            System.arraycopy(ix, a, ix, a + 1, i - a)
            ix(a) = p
          i += 1
        lo = hi
      var width = MinRun
      var src = ix
      var dst = tmp
      while width < nc do
        lo = 0
        while lo < nc do
          val mid = if nc - lo < width then nc else lo + width
          val hi = if nc - mid < width then nc else mid + width
          var i = lo
          var j = mid
          var k = lo
          if mid < hi && !leqAt(src(mid - 1), src(mid)) then
            while i < mid && j < hi do
              if leqAt(src(i), src(j)) then
                dst(k) = src(i)
                i += 1
              else
                dst(k) = src(j)
                j += 1
              k += 1
          if i < mid then
            System.arraycopy(src, i, dst, k, mid - i)
            k += mid - i
          if j < hi then System.arraycopy(src, j, dst, k, hi - j)
          lo = hi
        val t = src
        src = dst
        dst = t
        width <<= 1
        if width <= 0 then width = nc
      if src ne ix then System.arraycopy(src, 0, ix, 0, nc)
    nc

  /** Index sort of slots `i0 until iN` by a comparison on slots, into `ix` with scratch `tmp` (each at least
    * `iN - i0` long): `leqAt(i, j)` says whether slot `i` sorts no later than slot `j`.  Returns how many slots
    * compare; when `partial`, a slot with `!leqAt(i, i)` comes after all others, in original order.  The kernel is
    * compiled here, in a method of its own, with `leqAt` in place.
    */
  inline def indexSortBy(i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int])(inline leqAt: (Int, Int) => Boolean, inline partial: Boolean): Int =
    def sortWork(): Int = indexSortByImpl(i0, iN, ix, tmp)(leqAt, partial)
    sortWork()

  /** Slots `i0 until iN` in ascending order under `leqAt`, ties in original order, in a new array; when `partial`,
    * slots with `!leqAt(i, i)` come last.  The kernel is compiled here; see `indexSortBy`.
    */
  inline def indicesInOrderBy(i0: Int, iN: Int, inline partial: Boolean)(inline leqAt: (Int, Int) => Boolean): Array[Int] =
    val n = iN - i0
    val ix = new Array[Int](n)
    indexSortBy(i0, iN, ix, new Array[Int](n))(leqAt, partial) __ Unit
    ix

  /** Rearranges positions `i0 until iN` of something with `get` and `set` so that position `i0 + k` receives what
    * was at `ix(k)`, in place, by following the cycles of the permutation.  `ix(0 until iN - i0)` must be a
    * permutation of `i0 until iN`, such as `indicesInOrder` produces; it is used for bookkeeping while the walk runs
    * but is intact when it returns.
    */
  inline def reorder[A](ix: Array[Int], i0: Int, iN: Int)(inline get: Int => A)(inline set: (Int, A) => Unit): Unit =
    val n = iN - i0
    var k = 0
    while k < n do
      if ix(k) >= 0 then
        val v = get(i0 + k)
        var j = k
        var src = ix(j)
        ix(j) = -1 - src
        while src != i0 + k do
          set(i0 + j, get(src))
          j = src - i0
          src = ix(j)
          ix(j) = -1 - src
        set(i0 + j, v)
      k += 1
    k = 0
    while k < n do
      ix(k) = -1 - ix(k)
      k += 1

  /** As `reorder` with `Int` indices, for positions and permutations given as `Long`s. */
  inline def reorder[A](ix: Array[Long], i0: Long, iN: Long)(inline get: Long => A)(inline set: (Long, A) => Unit): Unit =
    val n = (iN - i0).toInt
    var k = 0
    while k < n do
      if ix(k) >= 0 then
        val v = get(i0 + k)
        var j = k
        var src = ix(j)
        ix(j) = -1L - src
        while src != i0 + k do
          set(i0 + j, get(src))
          j = (src - i0).toInt
          src = ix(j)
          ix(j) = -1L - src
        set(i0 + j, v)
      k += 1
    k = 0
    while k < n do
      ix(k) = -1L - ix(k)
      k += 1

  /** Applies an index order to records that can only be copied whole: as `reorder`, but `copy(from, to)` copies the
    * record at `from` over slot `to`, `save(i)` sets the record at `i` aside, and `restore(i)` puts the set-aside record
    * at slot `i`.  Only one record is set aside at a time, and nothing is compared, so a one-record scratch suffices.
    */
  inline def reorderRecords(ix: Array[Long], i0: Long, iN: Long)(inline copy: (Long, Long) => Unit)(inline save: Long => Unit)(inline restore: Long => Unit): Unit =
    val n = (iN - i0).toInt
    var k = 0
    while k < n do
      if ix(k) >= 0 then
        save(i0 + k)
        var j = k
        var src = ix(j)
        ix(j) = -1L - src
        while src != i0 + k do
          copy(src, i0 + j)
          j = (src - i0).toInt
          src = ix(j)
          ix(j) = -1L - src
        restore(i0 + j)
      k += 1
    k = 0
    while k < n do
      ix(k) = -1L - ix(k)
      k += 1

  /** As `reorderRecords` with `Long` indices, for `Int` positions and permutations. */
  inline def reorderRecords(ix: Array[Int], i0: Int, iN: Int)(inline copy: (Int, Int) => Unit)(inline save: Int => Unit)(inline restore: Int => Unit): Unit =
    val n = iN - i0
    var k = 0
    while k < n do
      if ix(k) >= 0 then
        save(i0 + k)
        var j = k
        var src = ix(j)
        ix(j) = -1 - src
        while src != i0 + k do
          copy(src, i0 + j)
          j = src - i0
          src = ix(j)
          ix(j) = -1 - src
        restore(i0 + j)
      k += 1
    k = 0
    while k < n do
      ix(k) = -1 - ix(k)
      k += 1

  /** The length of `i0 until iN` as an `Int`, or an exception if it does not fit; an empty or inverted range is 0. */
  def rangeSize(i0: Long, iN: Long): Int =
    if iN <= i0 then 0
    else if iN - i0 > Int.MaxValue then throw new IllegalArgumentException(s"Range $i0 until $iN is too long to sort")
    else (iN - i0).toInt

  /** The first `n` indices of `ix`, each offset by `i0`, as `Long`s. */
  def absoluteIndices(ix: Array[Int], n: Int, i0: Long): Array[Long] =
    val a = new Array[Long](n)
    var k = 0
    while k < n do
      a(k) = i0 + ix(k)
      k += 1
    a
}


/** Sorting for off-heap `Mem`, with the same verbs as arrays.  Indices are `Long`s here: the allocating forms return
  * absolute `Long` indices, while the buffered forms fill `Int` indices relative to `i0` and return how many values
  * compared.  A range must be shorter than `Int.MaxValue`.  The index sort reads keys through the `Mem` at the call
  * site, compiling the kernel there; the value sorts copy the range into a key array, sort that with the order's
  * compiled kernel, and copy it back.
  */
extension [A <: Mem.Type](m: Mem[A]) {
  /** Absolute indices of `m` in ascending order of value, ties in original order; values that do not compare come last. */
  inline def indicesInOrder()(using ord: Sorting.Order[A]): Array[Long] =
    indicesInOrder(0L, m.length)

  /** Absolute indices `i0 until iN` of `m` in ascending order of value, ties in original order. */
  inline def indicesInOrder(i0: Long, iN: Long)(using ord: Sorting.Order[A]): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    Sorting.indexSort(0, n, ix, new Array[Int](n))(k => m(i0 + k)) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Fills `ix(0 until iN - i0)` with indices relative to `i0`, in ascending order of value, ties in original order,
    * with `tmp` as scratch (each at least `iN - i0` long).  Returns how many values compare; any that do not come last.
    */
  inline def indicesInOrder(i0: Long, iN: Long, ix: Array[Int], tmp: Array[Int])(using ord: Sorting.Order[A]): Int =
    Sorting.indexSort(0, Sorting.rangeSize(i0, iN), ix, tmp)(k => m(i0 + k))

  /** Sorts `m` in place, ascending, ties in original order; values that do not compare come last, in original order. */
  inline def sortInOrder()(using ord: Sorting.Order[A]): Unit =
    sortInOrder(0L, m.length)

  /** Sorts `m(i0 until iN)` in place, ascending, ties in original order. */
  inline def sortInOrder(i0: Long, iN: Long)(using ord: Sorting.Order[A]): Unit =
    val n = Sorting.rangeSize(i0, iN)
    sortInOrder(i0, iN, ord.newKeys(n), ord.newKeys(n)) __ Unit

  /** Sorts `m(i0 until iN)` in place, ascending, ties in original order, through `keys` and `tmp` (each at least
    * `iN - i0` long) so repeated sorts need not allocate.  Returns how many values compare; any that do not come last.
    */
  inline def sortInOrder(i0: Long, iN: Long, keys: Array[A], tmp: Array[A])(using ord: Sorting.Order[A]): Int =
    val n = Sorting.rangeSize(i0, iN)
    m.inject(keys)(i0, iN) __ Unit
    val nc = ord.sort(keys, 0, n, tmp)
    MemorySegment.copy(keys, 0, m.segment, Mem.layoutOf[A], i0 * Mem.bytesOf[A], n)
    nc

  /** Sorts `m` in place as `sortInOrder()` does, and returns the original absolute index of the value now at each position. */
  inline def sortWithIndices()(using ord: Sorting.Order[A]): Array[Long] =
    sortWithIndices(0L, m.length)

  /** Sorts `m(i0 until iN)` in place and returns the original absolute index of the value now at each position. */
  inline def sortWithIndices(i0: Long, iN: Long)(using ord: Sorting.Order[A]): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    sortWithIndices(i0, iN, ix, ord.newKeys(n), ord.newKeys(n), new Array[Int](n)) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Sorts `m(i0 until iN)` in place and fills `ix(0 until iN - i0)` with the original index, relative to `i0`, of the
    * value now at each position, through `keys`, `tmp` and `tmpIx` (each at least `iN - i0` long) so repeated sorts
    * need not allocate.  Returns how many values compare; any that do not come last.
    */
  inline def sortWithIndices(i0: Long, iN: Long, ix: Array[Int], keys: Array[A], tmp: Array[A], tmpIx: Array[Int])(using ord: Sorting.Order[A]): Int =
    val n = Sorting.rangeSize(i0, iN)
    m.inject(keys)(i0, iN) __ Unit
    val nc = ord.sortIx(keys, 0, n, ix, tmp, tmpIx)
    MemorySegment.copy(keys, 0, m.segment, Mem.layoutOf[A], i0 * Mem.bytesOf[A], n)
    nc

  /** Rearranges `m` in place so that position `k` receives what was at `ix(k)`; `ix` must be a permutation of the
    * indices of `m`, such as `indicesInOrder` produces, and is intact afterwards.
    */
  inline def reorder(ix: Array[Long]): Unit =
    Sorting.reorder(ix, 0L, m.length)(i => m(i))((i, v) => m(i) = v)

  /** Rearranges `m(i0 until iN)` in place so that position `i0 + k` receives what was at `ix(k)`; `ix(0 until iN - i0)`
    * must be a permutation of `i0 until iN`, and is intact afterwards.
    */
  inline def reorder(ix: Array[Long], i0: Long, iN: Long): Unit =
    Sorting.reorder(ix, i0, iN)(i => m(i))((i, v) => m(i) = v)

  /** Rearranges `m(i0 until iN)` in place so that position `i0 + k` receives what was at `i0 + ix(k)`, for indices
    * relative to `i0` as the buffered forms produce; `ix(0 until iN - i0)` must be a permutation of `0 until iN - i0`,
    * and is intact afterwards.
    */
  inline def reorder(ix: Array[Int], i0: Long, iN: Long): Unit =
    Sorting.reorder(ix, 0, Sorting.rangeSize(i0, iN))(k => m(i0 + k))((k, v) => m(i0 + k) = v)
}


/** Sorting for `Mem.As`, as for `Mem`. */
extension [O](m: Mem.As[O]) {
  /** Absolute indices of `m` in ascending order of value, ties in original order; values that do not compare come last. */
  @targetName("asIndicesInOrder")
  inline def indicesInOrder()(using ord: Sorting.Order[O]): Array[Long] =
    indicesInOrder(0L, m.length)

  /** Absolute indices `i0 until iN` of `m` in ascending order of value, ties in original order. */
  @targetName("asIndicesInOrder")
  inline def indicesInOrder(i0: Long, iN: Long)(using ord: Sorting.Order[O]): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    Sorting.indexSort(0, n, ix, new Array[Int](n))(k => m(i0 + k)) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Fills `ix(0 until iN - i0)` with indices relative to `i0`, in ascending order of value, ties in original order,
    * with `tmp` as scratch (each at least `iN - i0` long).  Returns how many values compare; any that do not come last.
    */
  @targetName("asIndicesInOrder")
  inline def indicesInOrder(i0: Long, iN: Long, ix: Array[Int], tmp: Array[Int])(using ord: Sorting.Order[O]): Int =
    Sorting.indexSort(0, Sorting.rangeSize(i0, iN), ix, tmp)(k => m(i0 + k))

  /** Sorts `m` in place, ascending, ties in original order; values that do not compare come last, in original order. */
  @targetName("asSortInOrder")
  inline def sortInOrder()(using ord: Sorting.Order[O]): Unit =
    sortInOrder(0L, m.length)

  /** Sorts `m(i0 until iN)` in place, ascending, ties in original order. */
  @targetName("asSortInOrder")
  inline def sortInOrder(i0: Long, iN: Long)(using ord: Sorting.Order[O]): Unit =
    val n = Sorting.rangeSize(i0, iN)
    sortInOrder(i0, iN, ord.newKeys(n), ord.newKeys(n)) __ Unit

  /** Sorts `m(i0 until iN)` in place, ascending, ties in original order, through `keys` and `tmp` (each at least
    * `iN - i0` long) so repeated sorts need not allocate.  Returns how many values compare; any that do not come last.
    */
  @targetName("asSortInOrder")
  inline def sortInOrder(i0: Long, iN: Long, keys: Array[O], tmp: Array[O])(using ord: Sorting.Order[O]): Int =
    val n = Sorting.rangeSize(i0, iN)
    m.inject(keys)(i0, iN) __ Unit
    val nc = ord.sort(keys, 0, n, tmp)
    MemorySegment.copy(keys, 0, m.segment, Mem.As.layoutOf[O], i0 * Mem.As.bytesOf[O], n)
    nc

  /** Sorts `m` in place as `sortInOrder()` does, and returns the original absolute index of the value now at each position. */
  @targetName("asSortWithIndices")
  inline def sortWithIndices()(using ord: Sorting.Order[O]): Array[Long] =
    sortWithIndices(0L, m.length)

  /** Sorts `m(i0 until iN)` in place and returns the original absolute index of the value now at each position. */
  @targetName("asSortWithIndices")
  inline def sortWithIndices(i0: Long, iN: Long)(using ord: Sorting.Order[O]): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    sortWithIndices(i0, iN, ix, ord.newKeys(n), ord.newKeys(n), new Array[Int](n)) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Sorts `m(i0 until iN)` in place and fills `ix(0 until iN - i0)` with the original index, relative to `i0`, of the
    * value now at each position, through `keys`, `tmp` and `tmpIx` (each at least `iN - i0` long) so repeated sorts
    * need not allocate.  Returns how many values compare; any that do not come last.
    */
  @targetName("asSortWithIndices")
  inline def sortWithIndices(i0: Long, iN: Long, ix: Array[Int], keys: Array[O], tmp: Array[O], tmpIx: Array[Int])(using ord: Sorting.Order[O]): Int =
    val n = Sorting.rangeSize(i0, iN)
    m.inject(keys)(i0, iN) __ Unit
    val nc = ord.sortIx(keys, 0, n, ix, tmp, tmpIx)
    MemorySegment.copy(keys, 0, m.segment, Mem.As.layoutOf[O], i0 * Mem.As.bytesOf[O], n)
    nc

  /** Rearranges `m` in place so that position `k` receives what was at `ix(k)`; `ix` must be a permutation of the
    * indices of `m`, such as `indicesInOrder` produces, and is intact afterwards.
    */
  @targetName("asReorder")
  inline def reorder(ix: Array[Long]): Unit =
    Sorting.reorder(ix, 0L, m.length)(i => m(i))((i, v) => m(i) = v)

  /** Rearranges `m(i0 until iN)` in place so that position `i0 + k` receives what was at `ix(k)`; `ix(0 until iN - i0)`
    * must be a permutation of `i0 until iN`, and is intact afterwards.
    */
  @targetName("asReorder")
  inline def reorder(ix: Array[Long], i0: Long, iN: Long): Unit =
    Sorting.reorder(ix, i0, iN)(i => m(i))((i, v) => m(i) = v)

  /** Rearranges `m(i0 until iN)` in place so that position `i0 + k` receives what was at `i0 + ix(k)`, for indices
    * relative to `i0` as the buffered forms produce; `ix(0 until iN - i0)` must be a permutation of `0 until iN - i0`,
    * and is intact afterwards.
    */
  @targetName("asReorder")
  inline def reorder(ix: Array[Int], i0: Long, iN: Long): Unit =
    Sorting.reorder(ix, 0, Sorting.rangeSize(i0, iN))(k => m(i0 + k))((k, v) => m(i0 + k) = v)
}


/** Sorting for `Mem.OrderAware`, as for `Mem`, with every value read and written under the byte order in scope.
  * A swapped view has no bulk copy, so the value sorts move values through the key array one at a time.
  */
extension [A <: Mem.Type](m: Mem.OrderAware[A]) {
  /** Absolute indices of `m` in ascending order of value, ties in original order; values that do not compare come last. */
  @targetName("orderAwareIndicesInOrder")
  inline def indicesInOrder()(using ord: Sorting.Order[A], o: Mem.Order): Array[Long] =
    indicesInOrder(0L, m.length)

  /** Absolute indices `i0 until iN` of `m` in ascending order of value, ties in original order. */
  @targetName("orderAwareIndicesInOrder")
  inline def indicesInOrder(i0: Long, iN: Long)(using ord: Sorting.Order[A], o: Mem.Order): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    Sorting.indexSort(0, n, ix, new Array[Int](n))(k => m(i0 + k)) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Fills `ix(0 until iN - i0)` with indices relative to `i0`, in ascending order of value, ties in original order,
    * with `tmp` as scratch (each at least `iN - i0` long).  Returns how many values compare; any that do not come last.
    */
  @targetName("orderAwareIndicesInOrder")
  inline def indicesInOrder(i0: Long, iN: Long, ix: Array[Int], tmp: Array[Int])(using ord: Sorting.Order[A], o: Mem.Order): Int =
    Sorting.indexSort(0, Sorting.rangeSize(i0, iN), ix, tmp)(k => m(i0 + k))

  /** Sorts `m` in place, ascending, ties in original order; values that do not compare come last, in original order. */
  @targetName("orderAwareSortInOrder")
  inline def sortInOrder()(using ord: Sorting.Order[A], o: Mem.Order): Unit =
    sortInOrder(0L, m.length)

  /** Sorts `m(i0 until iN)` in place, ascending, ties in original order. */
  @targetName("orderAwareSortInOrder")
  inline def sortInOrder(i0: Long, iN: Long)(using ord: Sorting.Order[A], o: Mem.Order): Unit =
    val n = Sorting.rangeSize(i0, iN)
    sortInOrder(i0, iN, ord.newKeys(n), ord.newKeys(n)) __ Unit

  /** Sorts `m(i0 until iN)` in place, ascending, ties in original order, through `keys` and `tmp` (each at least
    * `iN - i0` long) so repeated sorts need not allocate.  Returns how many values compare; any that do not come last.
    */
  @targetName("orderAwareSortInOrder")
  inline def sortInOrder(i0: Long, iN: Long, keys: Array[A], tmp: Array[A])(using ord: Sorting.Order[A], o: Mem.Order): Int =
    val n = Sorting.rangeSize(i0, iN)
    var k = 0
    while k < n do
      keys(k) = m(i0 + k)
      k += 1
    val nc = ord.sort(keys, 0, n, tmp)
    k = 0
    while k < n do
      m(i0 + k) = keys(k)
      k += 1
    nc

  /** Sorts `m` in place as `sortInOrder()` does, and returns the original absolute index of the value now at each position. */
  @targetName("orderAwareSortWithIndices")
  inline def sortWithIndices()(using ord: Sorting.Order[A], o: Mem.Order): Array[Long] =
    sortWithIndices(0L, m.length)

  /** Sorts `m(i0 until iN)` in place and returns the original absolute index of the value now at each position. */
  @targetName("orderAwareSortWithIndices")
  inline def sortWithIndices(i0: Long, iN: Long)(using ord: Sorting.Order[A], o: Mem.Order): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    sortWithIndices(i0, iN, ix, ord.newKeys(n), ord.newKeys(n), new Array[Int](n)) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Sorts `m(i0 until iN)` in place and fills `ix(0 until iN - i0)` with the original index, relative to `i0`, of the
    * value now at each position, through `keys`, `tmp` and `tmpIx` (each at least `iN - i0` long) so repeated sorts
    * need not allocate.  Returns how many values compare; any that do not come last.
    */
  @targetName("orderAwareSortWithIndices")
  inline def sortWithIndices(i0: Long, iN: Long, ix: Array[Int], keys: Array[A], tmp: Array[A], tmpIx: Array[Int])(using ord: Sorting.Order[A], o: Mem.Order): Int =
    val n = Sorting.rangeSize(i0, iN)
    var k = 0
    while k < n do
      keys(k) = m(i0 + k)
      k += 1
    val nc = ord.sortIx(keys, 0, n, ix, tmp, tmpIx)
    k = 0
    while k < n do
      m(i0 + k) = keys(k)
      k += 1
    nc

  /** Rearranges `m` in place so that position `k` receives what was at `ix(k)`; `ix` must be a permutation of the
    * indices of `m`, such as `indicesInOrder` produces, and is intact afterwards.
    */
  @targetName("orderAwareReorder")
  inline def reorder(ix: Array[Long])(using o: Mem.Order): Unit =
    Sorting.reorder(ix, 0L, m.length)(i => m(i))((i, v) => m(i) = v)

  /** Rearranges `m(i0 until iN)` in place so that position `i0 + k` receives what was at `ix(k)`; `ix(0 until iN - i0)`
    * must be a permutation of `i0 until iN`, and is intact afterwards.
    */
  @targetName("orderAwareReorder")
  inline def reorder(ix: Array[Long], i0: Long, iN: Long)(using o: Mem.Order): Unit =
    Sorting.reorder(ix, i0, iN)(i => m(i))((i, v) => m(i) = v)

  /** Rearranges `m(i0 until iN)` in place so that position `i0 + k` receives what was at `i0 + ix(k)`, for indices
    * relative to `i0` as the buffered forms produce; `ix(0 until iN - i0)` must be a permutation of `0 until iN - i0`,
    * and is intact afterwards.
    */
  @targetName("orderAwareReorder")
  inline def reorder(ix: Array[Int], i0: Long, iN: Long)(using o: Mem.Order): Unit =
    Sorting.reorder(ix, 0, Sorting.rangeSize(i0, iN))(k => m(i0 + k))((k, v) => m(i0 + k) = v)
}


/** Index ordering for `Mem.AoS`: records are compared by a function on their slots, e.g.
  * `xs.indicesInOrderBy()((i, j) => xs.age(i) <= xs.age(j))`, so no key is extracted and no record is materialized.
  * A record for which the comparison is false against itself (e.g. one with a `NaN` field it compares by) comes
  * last, in original order.  Indices are `Long`s as for `Mem`: the allocating forms return absolute indices, the
  * buffered form fills `Int` indices relative to `i0` and returns how many records compared.
  */
extension [T <: NamedTuple.AnyNamedTuple](xs: Mem.AoS[T]) {
  /** Absolute indices of the records of `xs` in ascending order under `leqAt`, ties in original order. */
  inline def indicesInOrderBy()(inline leqAt: (Long, Long) => Boolean): Array[Long] =
    indicesInOrderBy(0L, xs.length)(leqAt)

  /** Absolute indices of records `i0 until iN` of `xs` in ascending order under `leqAt`, ties in original order. */
  inline def indicesInOrderBy(i0: Long, iN: Long)(inline leqAt: (Long, Long) => Boolean): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    Sorting.indexSortBy(0, n, ix, new Array[Int](n))((a, b) => leqAt(i0 + a, i0 + b), true) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Fills `ix(0 until iN - i0)` with record indices relative to `i0` in ascending order under `leqAt`, ties in
    * original order, with `tmp` as scratch (each at least `iN - i0` long).  Returns how many records compare.
    */
  inline def indicesInOrderBy(i0: Long, iN: Long, ix: Array[Int], tmp: Array[Int])(inline leqAt: (Long, Long) => Boolean): Int =
    Sorting.indexSortBy(0, Sorting.rangeSize(i0, iN), ix, tmp)((a, b) => leqAt(i0 + a, i0 + b), true)

  /** Rearranges the records of `xs` in place so that slot `k` receives the record that was at `ix(k)`; `ix` must be
    * a permutation of the record indices, such as `indicesInOrderBy` produces, and is intact afterwards.  Records
    * move as whole byte ranges, one held aside at a time in a fresh one-record scratch.
    */
  @targetName("aosReorder")
  inline def reorder(ix: Array[Long]): Unit =
    reorder(ix, 0L, xs.length, Mem.Struct.of[T])

  /** Rearranges records `i0 until iN` of `xs` in place so that slot `i0 + k` receives the record that was at `ix(k)`;
    * `ix(0 until iN - i0)` must be a permutation of `i0 until iN`, and is intact afterwards.
    */
  @targetName("aosReorder")
  inline def reorder(ix: Array[Long], i0: Long, iN: Long): Unit =
    reorder(ix, i0, iN, Mem.Struct.of[T])

  /** As `reorder(ix, i0, iN)`, holding records aside in `scratch` so repeated reorders allocate nothing. */
  @targetName("aosReorder")
  inline def reorder(ix: Array[Long], i0: Long, iN: Long, scratch: Mem.Struct[T]): Unit =
    Sorting.reorderRecords(ix, i0, iN)((from, to) => xs.copyRecord(from, to))(i => xs.copyRecordTo(i, scratch))(i => xs.copyRecordFrom(scratch, i))

  /** Rearranges records `i0 until iN` of `xs` in place so that slot `i0 + k` receives the record that was at
    * `i0 + ix(k)`, for indices relative to `i0` as the buffered form produces; `ix(0 until iN - i0)` must be a
    * permutation of `0 until iN - i0`, and is intact afterwards.
    */
  @targetName("aosReorder")
  inline def reorder(ix: Array[Int], i0: Long, iN: Long): Unit =
    reorder(ix, i0, iN, Mem.Struct.of[T])

  /** As `reorder(ix, i0, iN)` with relative indices, holding records aside in `scratch`. */
  @targetName("aosReorder")
  inline def reorder(ix: Array[Int], i0: Long, iN: Long, scratch: Mem.Struct[T]): Unit =
    Sorting.reorderRecords(ix, 0, Sorting.rangeSize(i0, iN))((from, to) => xs.copyRecord(i0 + from, i0 + to))(k => xs.copyRecordTo(i0 + k, scratch))(k => xs.copyRecordFrom(scratch, i0 + k))
}


/** Index ordering for `Mem.AoS.OrderAware`, as for `Mem.AoS`; the comparison reads fields under whatever byte order
  * it has in scope.  Records move as bytes, so `reorder` needs no order.
  */
extension [T <: NamedTuple.AnyNamedTuple](xs: Mem.AoS.OrderAware[T]) {
  /** Absolute indices of the records of `xs` in ascending order under `leqAt`, ties in original order. */
  @targetName("orderAwareIndicesInOrderBy")
  inline def indicesInOrderBy()(inline leqAt: (Long, Long) => Boolean): Array[Long] =
    indicesInOrderBy(0L, xs.length)(leqAt)

  /** Absolute indices of records `i0 until iN` of `xs` in ascending order under `leqAt`, ties in original order. */
  @targetName("orderAwareIndicesInOrderBy")
  inline def indicesInOrderBy(i0: Long, iN: Long)(inline leqAt: (Long, Long) => Boolean): Array[Long] =
    val n = Sorting.rangeSize(i0, iN)
    val ix = new Array[Int](n)
    Sorting.indexSortBy(0, n, ix, new Array[Int](n))((a, b) => leqAt(i0 + a, i0 + b), true) __ Unit
    Sorting.absoluteIndices(ix, n, i0)

  /** Fills `ix(0 until iN - i0)` with record indices relative to `i0` in ascending order under `leqAt`, ties in
    * original order, with `tmp` as scratch (each at least `iN - i0` long).  Returns how many records compare.
    */
  @targetName("orderAwareIndicesInOrderBy")
  inline def indicesInOrderBy(i0: Long, iN: Long, ix: Array[Int], tmp: Array[Int])(inline leqAt: (Long, Long) => Boolean): Int =
    Sorting.indexSortBy(0, Sorting.rangeSize(i0, iN), ix, tmp)((a, b) => leqAt(i0 + a, i0 + b), true)

  /** Rearranges the records of `xs` in place so that slot `k` receives the record that was at `ix(k)`; `ix` is intact afterwards. */
  @targetName("aosOrderAwareReorder")
  inline def reorder(ix: Array[Long]): Unit =
    reorder(ix, 0L, xs.length, Mem.Struct.of[T])

  /** Rearranges records `i0 until iN` of `xs` in place so that slot `i0 + k` receives the record that was at `ix(k)`. */
  @targetName("aosOrderAwareReorder")
  inline def reorder(ix: Array[Long], i0: Long, iN: Long): Unit =
    reorder(ix, i0, iN, Mem.Struct.of[T])

  /** As `reorder(ix, i0, iN)`, holding records aside in `scratch` so repeated reorders allocate nothing. */
  @targetName("aosOrderAwareReorder")
  inline def reorder(ix: Array[Long], i0: Long, iN: Long, scratch: Mem.Struct[T]): Unit =
    Sorting.reorderRecords(ix, i0, iN)((from, to) => xs.copyRecord(from, to))(i => xs.copyRecordTo(i, scratch))(i => xs.copyRecordFrom(scratch, i))

  /** Rearranges records `i0 until iN` of `xs` in place so that slot `i0 + k` receives the record that was at
    * `i0 + ix(k)`, for indices relative to `i0` as the buffered form produces.
    */
  @targetName("aosOrderAwareReorder")
  inline def reorder(ix: Array[Int], i0: Long, iN: Long): Unit =
    reorder(ix, i0, iN, Mem.Struct.of[T])

  /** As `reorder(ix, i0, iN)` with relative indices, holding records aside in `scratch`. */
  @targetName("aosOrderAwareReorder")
  inline def reorder(ix: Array[Int], i0: Long, iN: Long, scratch: Mem.Struct[T]): Unit =
    Sorting.reorderRecords(ix, 0, Sorting.rangeSize(i0, iN))((from, to) => xs.copyRecord(i0 + from, i0 + to))(k => xs.copyRecordTo(i0 + k, scratch))(k => xs.copyRecordFrom(scratch, i0 + k))
}


extension [A](a: Array[A]) {
  /** Indices of `a` in ascending order of value, ties in original order, in a new array.  Values that do not compare
    * (e.g. `NaN`) come last, in original order.
    */
  inline def indicesInOrder()(using ord: Sorting.Order[A]): Array[Int] =
    val ix = new Array[Int](a.length)
    ord.indexSort(a, 0, a.length, ix, new Array[Int](a.length)) __ Unit
    ix

  /** Indices `i0 until iN` of `a` in ascending order of value, ties in original order, in a new array of length `iN - i0`. */
  inline def indicesInOrder(i0: Int, iN: Int)(using ord: Sorting.Order[A]): Array[Int] =
    val n = iN - i0
    val ix = new Array[Int](n)
    ord.indexSort(a, i0, iN, ix, new Array[Int](n)) __ Unit
    ix

  /** Indices of `a` within `r` in ascending order of value, ties in original order, in a new array. */
  inline def indicesInOrder[R <: Iv.X | Rg](inline r: R)(using ord: Sorting.Order[A]): Array[Int] =
    Iv.dispatch(r, a)((i0, iN) => indicesInOrder(i0, iN))

  /** Indices `i0 until iN` of `a` in ascending order of value, ties in original order, filled into `ix(0 until iN - i0)`
    * with `tmp` as scratch (each at least `iN - i0` long), so repeated sorts need not allocate.  Returns how many
    * values compare; any that do not come after them, in original order.
    */
  inline def indicesInOrder(i0: Int, iN: Int, ix: Array[Int], tmp: Array[Int])(using ord: Sorting.Order[A]): Int =
    ord.indexSort(a, i0, iN, ix, tmp)

  /** Sorts `a` in place, ascending, ties in original order; values that do not compare (e.g. `NaN`) come last, in
    * original order.
    */
  inline def sortInOrder()(using ord: Sorting.Order[A]): Unit =
    ord.sort(a, 0, a.length, ord.newKeys(a.length)) __ Unit

  /** Sorts `a(i0 until iN)` in place, ascending, ties in original order. */
  inline def sortInOrder(i0: Int, iN: Int)(using ord: Sorting.Order[A]): Unit =
    ord.sort(a, i0, iN, ord.newKeys(iN - i0)) __ Unit

  /** Sorts `a` within `r` in place, ascending, ties in original order. */
  inline def sortInOrder[R <: Iv.X | Rg](inline r: R)(using ord: Sorting.Order[A]): Unit =
    Iv.dispatch(r, a)((i0, iN) => sortInOrder(i0, iN))

  /** Sorts `a(i0 until iN)` in place, ascending, ties in original order, with `tmp` (at least `iN - i0` long) as
    * scratch so repeated sorts need not allocate.  Returns how many values compare; any that do not come last.
    */
  inline def sortInOrder(i0: Int, iN: Int, tmp: Array[A])(using ord: Sorting.Order[A]): Int =
    ord.sort(a, i0, iN, tmp)

  /** Sorts `a` in place as `sortInOrder()` does, and returns the original index of the value now at each position, so
    * that other data can be rearranged to match (e.g. with `reorder`).
    */
  inline def sortWithIndices()(using ord: Sorting.Order[A]): Array[Int] =
    val ix = new Array[Int](a.length)
    ord.sortIx(a, 0, a.length, ix, ord.newKeys(a.length), new Array[Int](a.length)) __ Unit
    ix

  /** Sorts `a(i0 until iN)` in place and returns the original index of the value now at each position, as an array
    * of length `iN - i0`.
    */
  inline def sortWithIndices(i0: Int, iN: Int)(using ord: Sorting.Order[A]): Array[Int] =
    val n = iN - i0
    val ix = new Array[Int](n)
    ord.sortIx(a, i0, iN, ix, ord.newKeys(n), new Array[Int](n)) __ Unit
    ix

  /** Sorts `a(i0 until iN)` in place and fills `ix(0 until iN - i0)` with the original index of the value now at
    * each position, with `tmp` and `tmpIx` (each at least `iN - i0` long) as scratch so repeated sorts need not
    * allocate.  Returns how many values compare; any that do not come last.
    */
  inline def sortWithIndices(i0: Int, iN: Int, ix: Array[Int], tmp: Array[A], tmpIx: Array[Int])(using ord: Sorting.Order[A]): Int =
    ord.sortIx(a, i0, iN, ix, tmp, tmpIx)

  /** Rearranges `a` in place so that position `k` receives what was at `ix(k)`; `ix` must be a permutation of the
    * indices of `a`, such as `indicesInOrder` produces, and is intact afterwards.
    */
  inline def reorder(ix: Array[Int]): Unit =
    Sorting.reorder(ix, 0, a.length)(i => a(i))((i, v) => a(i) = v)

  /** Rearranges `a(i0 until iN)` in place so that position `i0 + k` receives what was at `ix(k)`; `ix(0 until iN - i0)`
    * must be a permutation of `i0 until iN`, and is intact afterwards.
    */
  inline def reorder(ix: Array[Int], i0: Int, iN: Int): Unit =
    Sorting.reorder(ix, i0, iN)(i => a(i))((i, v) => a(i) = v)
}
