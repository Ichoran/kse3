// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-25 Rex Kerr

package kse.basics


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.util.boundary


object Sorting {
  // Algorithm may fail if this is set below 4 due to inadequate swap space
  inline val MinRunSize = 8

  def shiftIncomparablesImpl(indices: Array[Int], i: Int, m: Int)(swap: Array[Int], si: Int, sm: Int): Int =
    // At the start, the first `m` indices are a mix of comparable and incomparable targets, and we have
    // checked them down to i (so we can't start below i).
    // The comparable ones are in ranges swap(0)..swap(1), swap(2)..swap(3), ..., swap(si-2)..swap(si-1)
    // in reverse order, so swap(si-2)..swap(si-1) are the lowest, and swap(1) == m.
    // The incomparable indices have already been stored in swap(sm), ..., swap(swap.length-1)
    // Our task is move the known comparable indices adjacent to each other and put the incomparables
    // back from swap.  We return the new value for m (which is just m - (swap.length - sm)).
    var k = si - 2
    var gap = 0
    var j = m
    if i < swap(k) then
      gap = swap(k) - i
      j = i
    else
      while k > 1 && { gap = swap(k-2) - swap(k+1); gap == 0 } do k -= 2
      j = swap(k+1)
    var h = j + gap
    // j is the first index in indices where we have incomparables that got copied out and we need
    // to shift comparables into their space by moving them forwards from h
    while h < swap(1) do
      if h == swap(k+1) then
        h += swap(k-2) - swap(k+1)
        swap(k) -= gap
        swap(k+1) -= gap
        gap = h-j
        k -= 2
      indices(j) = indices(h)
      j += 1
      h += 1
    // Last range doesn't get moved in the loop above, so we do it afterwards
    swap(0) -= gap
    swap(1) -= gap
    // Now we need to move all the incomparables over from swap
    j = sm
    while h < m do
      indices(h) = swap(j)
      h += 1
      j += 1
    // Return new position of m for indices (sm = swap.length by design; i and si not changed; swap pairs updated to correct values)
    m - (swap.length - sm)


  /*
  inline def indicesInOrderImpl[A](inline a: Int => A, indices: Array[Int])(inline leq: (A, A) => Boolean, inline stable: Boolean, inline incomparable: Boolean): Unit =
    boundary:
      if indices.length < 2 then boundary.break()
      // Invariant: r is always the value of `a` at `indices(k)`
      var k = n-1
      var r = a(indices(k))
      // Pseudo-invariant: l is either r or the value of `a` at `indices(k-1)` (we will say which)
      var l = r
      // Any values after m are known to be incomparable even with themselves
      var m = n
      boundary.break:
        // Step 1: Find the last forward run
        // When we're done, either k = 0 or k > 0, l == a(indices(k-1))
        inline if incomparable then
          // Find the last forward run using normal comparisons
          while k > 0 && { l = a(ixs(k-1)); leq(l, r) } do
            r = l
            k -= 1
          if k == n-1 then
            // We made no progress, but that might be because NaN or other self-unequal values are already at the end
            while k > 0 && !leq(r, r) do
              k -= 1
              m -= 1
              r = ixs(k-1)
            if k < n-1 then
              // If we've found some self-unequal values, fill out the rest of the forward run
              while k > 0 && { l = a(ixs(k-1)); leq(l, r) } do
                r = l
                k -= 1
        else
          // Find the last forward run, assuming everyone is comparable
          while k > 0 && { l = a(ixs(k-1)); leq(l, r) } do
            r = l
            k -= 1
        if k == 0 then boundary.break()   // Forward run covered everything

        // Step 2: Build forward runs of at least the minimum size, moving any incomparable values to swap
        val swap = new Array[Int](1 + indices.length / 2)
        // From smi to swap.length are incomparable
        var smi = swap.length
        // From 0 to si are start/end pairs for forward runs
        var si = 0


  // TimSort-like sort; details are slightly different, due mostly to needing to handle values that always compare false (e.g. NaN, but Option can be encoded that way too)
  inline def indicesInOrderImpl[A](a: Array[A], i0: Int, iN: Int)(inline leq: (A, A) => Boolean, inline stable: Boolean, inline incomparable: Boolean): Array[Int] =
    if iN > i0 then
      if i0 < 0 || iN > a.length then throw new NoSuchElementException(s"Interval $i0..$iN does not fit in array of size 0..${a.length}")
      val ixs = arrayed(iN - i0)(i => i+i0)
      indicesInOrderImpl((i: Int) => a(i), ixs)(leq, stable, incomparable)
    else if iN == i0+1 then
      val ixs = new Array[Int](1)
      ixs(0) = i0
      ixs
    else
      new Array[Int](0)
  */
}
