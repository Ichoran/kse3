// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-23 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.flow


////////////////////////////////////////////////
/// Packaging and wrappers to alter behavior ///
////////////////////////////////////////////////

/** Typeclass to enable generic copying of mutable things to a decent replica of themselves. */
trait Copies[A] {
  /** Creates a copy of a presumably mutable object. */
  def copy(a: A): A
}
object Copies {
  given copiesArrayByte:   Copies[Array[Byte  ]] with
    def copy(a: Array[Byte  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayShort:  Copies[Array[Short ]] with
    def copy(a: Array[Short ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayChar:   Copies[Array[Char  ]] with
    def copy(a: Array[Char  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayInt:    Copies[Array[Int   ]] with
    def copy(a: Array[Int   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayLong:   Copies[Array[Long  ]] with
    def copy(a: Array[Long  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayFloat:  Copies[Array[Float ]] with
    def copy(a: Array[Float ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayDouble: Copies[Array[Double]] with
    def copy(a: Array[Double]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayGeneric[A <: AnyRef]: Copies[Array[A]] with
    def copy(a: Array[A]) = java.util.Arrays.copyOf[A & AnyRef](a, a.length)

  given copiesAnon[A](using Copies[A]): Copies[Anon[A]] with
    def copy(a: Anon[A]) = new Anon[A](summon[Copies[A]] copy a.value)
}

/** Use copier typeclasses, if available, to copy a (presumably mutable) object. */
extension [A](a: A)
  inline def copy(using copier: Copies[A]): A = copier copy a


opaque type PythonIndexedBytes = Array[Byte]
object PythonIndexedBytes {
  extension (a: PythonIndexedBytes)
    inline def underlying: Array[Byte] = a

  extension (a: kse.flow.PythonIndexedBytes)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Byte) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedShorts = Array[Short]
object PythonIndexedShorts {
  extension (a: PythonIndexedShorts)
    inline def underlying: Array[Short] = a

  extension (a: kse.flow.PythonIndexedShorts)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Short) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedChars = Array[Char]
object PythonIndexedChars {
  extension (a: PythonIndexedChars)
    inline def underlying: Array[Char] = a

  extension (a: kse.flow.PythonIndexedChars)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Char) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedInts = Array[Int]
object PythonIndexedInts {
  extension (a: PythonIndexedInts)
    inline def underlying: Array[Int] = a

  extension (a: kse.flow.PythonIndexedInts)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Int) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedLongs = Array[Long]
object PythonIndexedLongs {
  extension (a: PythonIndexedLongs)
    inline def underlying: Array[Long] = a

  extension (a: kse.flow.PythonIndexedLongs)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Long) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedFloats = Array[Float]
object PythonIndexedFloats {
  extension (a: PythonIndexedFloats)
    inline def underlying: Array[Float] = a

  extension (a: kse.flow.PythonIndexedFloats)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Float) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedDoubles = Array[Double]
object PythonIndexedDoubles {
  extension (a: PythonIndexedDoubles)
    inline def underlying: Array[Double] = a

  extension (a: kse.flow.PythonIndexedDoubles)
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: Double) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

opaque type PythonIndexedAnyRefs[A <: AnyRef] = Array[A]
object PythonIndexedAnyRefs {
  extension [A <: AnyRef](a: PythonIndexedAnyRefs[A])
    inline def underlying: Array[A] = a

  extension [A <: AnyRef](a: kse.flow.PythonIndexedAnyRefs[A])
    inline def apply(i: Int) = if i < 0 then a.underlying(a.underlying.length + i) else a.underlying(i)
    inline def update(i: Int, x: A) = if i < 0 then a.underlying(a.underlying.length + i) = x else a.underlying(i) = x
    inline def index(i: Int) = if i < 0 then a.underlying.length + i else i
}

/** Byte Array specific functionality from java.util.Arrays and java.lang.System */
extension (ab: Array[Byte])
  inline def py: kse.flow.PythonIndexedBytes = ab
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ab, size)
  inline def shrinkCopy(size: Int) = if size < ab.length then java.util.Arrays.copyOf(ab, size) else ab
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ab, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Byte](ab.length + n); java.lang.System.arraycopy(ab, 0, a, n, ab.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(ab, if n < 0 then n else ab.length + n)
  inline def copyInto(that: Array[Byte]): that.type = { java.lang.System.arraycopy(ab, 0, that, 0, ab.length); that }
  inline def copyInto(that: Array[Byte], where: Int): that.type = { java.lang.System.arraycopy(ab, 0, that, where, ab.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Byte]): that.type = { java.lang.System.arraycopy(ab, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Byte], where: Int): that.type = { java.lang.System.arraycopy(ab, i0, that, where, iN-i0); that }
  inline def search(b: Byte): Int = java.util.Arrays.binarySearch(ab, b)
  inline def searchRange(i0: Int, iN: Int)(b: Byte): Int = java.util.Arrays.binarySearch(ab, i0, iN, b)
  inline def fill(b: Byte): ab.type = { java.util.Arrays.fill(ab, b); ab }
  inline def fillRange(i0: Int, iN: Int)(b: Byte): ab.type = { java.util.Arrays.fill(ab, i0, iN, b); ab }
  inline def sort(): ab.type = { java.util.Arrays.sort(ab); ab }
  inline def sortRange(i0: Int, iN: Int): ab.type = { java.util.Arrays.sort(ab, i0, iN); ab }
  inline def isSorted: Boolean = isSortedRange(0, ab.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ab(i-1) <= ab(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Byte => Byte): ab.type =
    var i = 0
    while i < ab.length do
      ab(i) = f(ab(i))
      i += 1
    ab

/** Short Array specific functionality from java.util.Arrays and java.lang.System */
extension (as: Array[Short])
  inline def py: kse.flow.PythonIndexedShorts = as
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(as, size)
  inline def shrinkCopy(size: Int) = if size < as.length then java.util.Arrays.copyOf(as, size) else as
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(as, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Short](as.length + n); java.lang.System.arraycopy(as, 0, a, n, as.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(as, if n < 0 then n else as.length + n)
  inline def copyInto(that: Array[Short]): that.type = { java.lang.System.arraycopy(as, 0, that, 0, as.length); that }
  inline def copyInto(that: Array[Short], where: Int): that.type = { java.lang.System.arraycopy(as, 0, that, where, as.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Short]): that.type = { java.lang.System.arraycopy(as, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Short], where: Int): that.type = { java.lang.System.arraycopy(as, i0, that, where, iN-i0); that }
  inline def search(s: Short): Int = java.util.Arrays.binarySearch(as, s)
  inline def searchRange(i0: Int, iN: Int)(s: Short): Int = java.util.Arrays.binarySearch(as, i0, iN, s)  
  inline def fill(s: Short): as.type = { java.util.Arrays.fill(as, s); as }
  inline def fillRange(i0: Int, iN: Int)(s: Short): as.type = { java.util.Arrays.fill(as, i0, iN, s); as }
  inline def sort(): as.type = { java.util.Arrays.sort(as); as }
  inline def sortRange(i0: Int, iN: Int): as.type = { java.util.Arrays.sort(as, i0, iN); as }
  inline def isSorted: Boolean = isSortedRange(0, as.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && as(i-1) <= as(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Short => Short): as.type =
    var i = 0
    while i < as.length do
      as(i) = f(as(i))
      i += 1
    as

/** Char Array specific functionality from java.util.Arrays and java.lang.System */
extension (ac: Array[Char])
  inline def py: kse.flow.PythonIndexedChars = ac
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ac, size)
  inline def shrinkCopy(size: Int) = if size < ac.length then java.util.Arrays.copyOf(ac, size) else ac
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ac, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Char](ac.length + n); java.lang.System.arraycopy(ac, 0, a, n, ac.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(ac, if n < 0 then n else ac.length + n)
  inline def copyInto(that: Array[Char]): that.type = { java.lang.System.arraycopy(ac, 0, that, 0, ac.length); that }
  inline def copyInto(that: Array[Char], where: Int): that.type = { java.lang.System.arraycopy(ac, 0, that, where, ac.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Char]): that.type = { java.lang.System.arraycopy(ac, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Char], where: Int): that.type = { java.lang.System.arraycopy(ac, i0, that, where, iN-i0); that }
  inline def search(c: Char): Int = java.util.Arrays.binarySearch(ac, c)
  inline def searchRange(i0: Int, iN: Int)(c: Char): Int = java.util.Arrays.binarySearch(ac, i0, iN, c)
  inline def fill(c: Char): ac.type = { java.util.Arrays.fill(ac, c); ac }
  inline def fillRange(i0: Int, iN: Int)(c: Char): ac.type = { java.util.Arrays.fill(ac, i0, iN, c); ac }
  inline def sort(): ac.type = { java.util.Arrays.sort(ac); ac }
  inline def sortRange(i0: Int, iN: Int): ac.type = { java.util.Arrays.sort(ac, i0, iN); ac }
  inline def isSorted: Boolean = isSortedRange(0, ac.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ac(i-1) <= ac(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Char => Char): ac.type =
    var i = 0
    while i < ac.length do
      ac(i) = f(ac(i))
      i += 1
    ac

/** Int Array specific functionality from java.util.Arrays and java.lang.System */
extension (ai: Array[Int])
  inline def py: kse.flow.PythonIndexedInts = ai
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ai, size)
  inline def shrinkCopy(size: Int) = if size < ai.length then java.util.Arrays.copyOf(ai, size) else ai
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ai, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Int](ai.length + n); java.lang.System.arraycopy(ai, 0, a, n, ai.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(ai, if n < 0 then n else ai.length + n)
  inline def copyInto(that: Array[Int]): that.type = { java.lang.System.arraycopy(ai, 0, that, 0, ai.length); that }
  inline def copyInto(that: Array[Int], where: Int): that.type = { java.lang.System.arraycopy(ai, 0, that, where, ai.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Int]): that.type = { java.lang.System.arraycopy(ai, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Int], where: Int): that.type = { java.lang.System.arraycopy(ai, i0, that, where, iN-i0); that }
  inline def search(i: Int): Int = java.util.Arrays.binarySearch(ai, i)
  inline def searchRange(i0: Int, iN: Int)(i: Int): Int = java.util.Arrays.binarySearch(ai, i0, iN, i)
  inline def fill(i: Int): ai.type = { java.util.Arrays.fill(ai, i); ai }
  inline def fillRange(i0: Int, iN: Int)(i: Int): ai.type = { java.util.Arrays.fill(ai, i0, iN, i); ai }
  inline def sort(): ai.type = { java.util.Arrays.sort(ai); ai }
  inline def sortRange(i0: Int, iN: Int): ai.type = { java.util.Arrays.sort(ai, i0, iN); ai }
  inline def isSorted: Boolean = isSortedRange(0, ai.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ai(i-1) <= ai(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Int => Int): ai.type =
    var i = 0
    while i < ai.length do
      ai(i) = f(ai(i))
      i += 1
    ai

/** Long Array specific functionality from java.util.Arrays and java.lang.System */
extension (al: Array[Long])
  inline def py: kse.flow.PythonIndexedLongs = al
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(al, size)
  inline def shrinkCopy(size: Int) = if size < al.length then java.util.Arrays.copyOf(al, size) else al
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(al, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Long](al.length + n); java.lang.System.arraycopy(al, 0, a, n, al.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(al, if n < 0 then n else al.length + n)
  inline def copyInto(that: Array[Long]): that.type = { java.lang.System.arraycopy(al, 0, that, 0, al.length); that }
  inline def copyInto(that: Array[Long], where: Int): that.type = { java.lang.System.arraycopy(al, 0, that, where, al.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Long]): that.type = { java.lang.System.arraycopy(al, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Long], where: Int): that.type = { java.lang.System.arraycopy(al, i0, that, where, iN-i0); that }
  inline def search(l: Long): Int = java.util.Arrays.binarySearch(al, l)
  inline def searchRange(i0: Int, iN: Int)(l: Long): Int = java.util.Arrays.binarySearch(al, i0, iN, l)
  inline def fill(l: Long): al.type = { java.util.Arrays.fill(al, l); al }
  inline def fillRange(i0: Int, iN: Int)(l: Long): al.type = { java.util.Arrays.fill(al, i0, iN, l); al }
  inline def sort(): al.type = { java.util.Arrays.sort(al); al }
  inline def sortRange(i0: Int, iN: Int): al.type = { java.util.Arrays.sort(al, i0, iN); al }
  inline def isSorted: Boolean = isSortedRange(0, al.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && al(i-1) <= al(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Long => Long): al.type =
    var i = 0
    while i < al.length do
      al(i) = f(al(i))
      i += 1
    al

/** Float Array specific functionality from java.util.Arrays and java.lang.System */
extension (af: Array[Float])
  inline def py: kse.flow.PythonIndexedFloats = af
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(af, size)
  inline def shrinkCopy(size: Int) = if size < af.length then java.util.Arrays.copyOf(af, size) else af
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(af, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Float](af.length + n); java.lang.System.arraycopy(af, 0, a, n, af.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(af, if n < 0 then n else af.length + n)
  inline def copyInto(that: Array[Float]): that.type = { java.lang.System.arraycopy(af, 0, that, 0, af.length); that }
  inline def copyInto(that: Array[Float], where: Int): that.type = { java.lang.System.arraycopy(af, 0, that, where, af.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Float]): that.type = { java.lang.System.arraycopy(af, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Float], where: Int): that.type = { java.lang.System.arraycopy(af, i0, that, where, iN-i0); that }
  inline def search(f: Float): Int = java.util.Arrays.binarySearch(af, f)
  inline def searchRange(i0: Int, iN: Int)(f: Float): Int = java.util.Arrays.binarySearch(af, i0, iN, f)
  inline def fill(f: Float): af.type = { java.util.Arrays.fill(af, f); af }
  inline def fillRange(i0: Int, iN: Int)(f: Float): af.type = { java.util.Arrays.fill(af, i0, iN, f); af }
  inline def sort(): af.type = { java.util.Arrays.sort(af); af }
  inline def sortRange(i0: Int, iN: Int): af.type = { java.util.Arrays.sort(af, i0, iN); af }
  inline def isSorted: Boolean = isSortedRange(0, af.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && af(i-1) <= af(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Float => Float): af.type =
    var i = 0
    while i < af.length do
      af(i) = f(af(i))
      i += 1
    af

/** Double Array specific functionality from java.util.Arrays and java.lang.System */
extension (ad: Array[Double])
  inline def py: kse.flow.PythonIndexedDoubles = ad
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ad, size)
  inline def shrinkCopy(size: Int) = if size < ad.length then java.util.Arrays.copyOf(ad, size) else ad
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ad, i0, iN)
  inline def addLeftSlots(n: Int) = { val a = new Array[Double](ad.length + n); java.lang.System.arraycopy(ad, 0, a, n, ad.length); a }
  inline def addRightSlots(n: Int) = java.util.Arrays.copyOf(ad, if n < 0 then n else ad.length + n)
  inline def copyInto(that: Array[Double]): that.type = { java.lang.System.arraycopy(ad, 0, that, 0, ad.length); that }
  inline def copyInto(that: Array[Double], where: Int): that.type = { java.lang.System.arraycopy(ad, 0, that, where, ad.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Double]): that.type = { java.lang.System.arraycopy(ad, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[Double], where: Int): that.type = { java.lang.System.arraycopy(ad, i0, that, where, iN-i0); that }
  inline def search(d: Double): Int = java.util.Arrays.binarySearch(ad, d)
  inline def searchRange(i0: Int, iN: Int)(d: Double): Int = java.util.Arrays.binarySearch(ad, i0, iN, d)
  inline def fill(d: Double): ad.type = { java.util.Arrays.fill(ad, d); ad }
  inline def fillRange(i0: Int, iN: Int)(d: Double): ad.type = { java.util.Arrays.fill(ad, i0, iN, d); ad }
  inline def sort(): ad.type = { java.util.Arrays.sort(ad); ad }
  inline def sortRange(i0: Int, iN: Int): ad.type = { java.util.Arrays.sort(ad, i0, iN); ad }
  inline def isSorted: Boolean = isSortedRange(0, ad.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ad(i-1) <= ad(i) do i += 1
      i >= iN
  inline def zapAll(inline f: Double => Double): ad.type =
    var i = 0
    while i < ad.length do
      ad(i) = f(ad(i))
      i += 1
    ad

/** Object Array specific functionality from java.util.Arrays and java.lang.System */
extension [A >: Null <: AnyRef](aa: Array[A])
  inline def py: kse.flow.PythonIndexedAnyRefs[A] = aa
  inline def copyToSize(size: Int): Array[A] = java.util.Arrays.copyOf(aa, size)
  inline def shrinkCopy(size: Int): Array[A] = if size < aa.length then java.util.Arrays.copyOf(aa, size) else aa
  inline def copyOfRange(i0: Int, iN: Int): Array[A] = java.util.Arrays.copyOfRange(aa, i0, iN)
  inline def addLeftSlots(n: Int)(using scala.reflect.ClassTag[A]): Array[A] = { val a = new Array[A](aa.length + n); java.lang.System.arraycopy(aa, 0, a, n, aa.length); a }
  inline def addRightSlots(n: Int)(using scala.reflect.ClassTag[A]): Array[A] = java.util.Arrays.copyOf(aa, if n < 0 then n else aa.length + n)
  inline def copyInto(that: Array[A]): that.type = { java.lang.System.arraycopy(aa, 0, that, 0, aa.length); that }
  inline def copyInto(that: Array[A], where: Int): that.type = { java.lang.System.arraycopy(aa, 0, that, where, aa.length); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[A]): that.type = { java.lang.System.arraycopy(aa, i0, that, 0, iN-i0); that }
  inline def copyRangeInto(i0: Int, iN: Int)(that: Array[A], where: Int): that.type = { java.lang.System.arraycopy(aa, i0, that, where, iN-i0); that }
  inline def search(a: A)(using o: scala.math.Ordering[A]): Int = java.util.Arrays.binarySearch(aa, a, o)
  inline def searchRange(i0: Int, iN: Int)(a: A)(using o: scala.math.Ordering[A]): Int = java.util.Arrays.binarySearch(aa, i0, iN, a, o)
  inline def fill(a: A): aa.type = { java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], a: AnyRef); aa }
  inline def fillRange(i0: Int, iN: Int)(a: A): aa.type = { java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], i0, iN, a: AnyRef); aa }
  inline def sort()(using scala.math.Ordering[A]): aa.type = { scala.util.Sorting.stableSort(aa); aa }
  inline def sortRange(i0: Int, iN: Int)(using scala.math.Ordering[A]): aa.type = { scala.util.Sorting.stableSort(aa, i0, iN); aa }
  inline def isSorted(using scala.math.Ordering[A]): Boolean = isSortedRange(0, aa.length)
  def isSortedRange(i0: Int, iN: Int)(using o: scala.math.Ordering[A]): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && o.compare(aa(i-1), aa(i)) <= 0 do i += 1
      i >= iN
  inline def zapAll(inline f: A => A): aa.type =
    var i = 0
    while i < aa.length do
      aa(i) = f(aa(i))
      i += 1
    aa


/** Holds mutable data (would be better if standard library exposed this!) */
sealed abstract class Mu[A] {
  inline def apply(): A = value
  def value: A
  def value_=(a: A): Unit
  def set(a: A): this.type = { value = a; this }
  inline final def zap(inline f: A => A): this.type = { value = f(value); this }
  inline final def use(inline f: A => Unit): this.type = { f(value); this }
  def copy: Mu[A]
  override def toString = s"~$value"
  override def hashCode = value.##
  override def equals(a: Any) = a match
    case m: Mu[_] => m.value.asInstanceOf[Any] == value
    case _ => false
}
object Mu {
  object      MuUnit                   extends Mu[Unit]    { def copy: MuUnit.type = this               ; def value: Unit = (); def value_=(u: Unit): Unit = () }
  final class MuBoolean(init: Boolean) extends Mu[Boolean] { def copy: MuBoolean = new MuBoolean(value) ; var value = init }
  final class MuByte   (init: Byte)    extends Mu[Byte]    { def copy: MuByte    = new MuByte(value)    ; var value = init }
  final class MuShort  (init: Short)   extends Mu[Short]   { def copy: MuShort   = new MuShort(value)   ; var value = init }
  final class MuChar   (init: Char)    extends Mu[Char]    { def copy: MuChar    = new MuChar(value)    ; var value = init }
  final class MuInt    (init: Int)     extends Mu[Int]     { def copy: MuInt     = new MuInt(value)     ; var value = init }
  final class MuLong   (init: Long)    extends Mu[Long]    { def copy: MuLong    = new MuLong(value)    ; var value = init }
  final class MuFloat  (init: Float)   extends Mu[Float]   { def copy: MuFloat   = new MuFloat(value)   ; var value = init }
  final class MuDouble (init: Double)  extends Mu[Double]  { def copy: MuDouble  = new MuDouble(value)  ; var value = init }
  final class MuAny[A] (init: A)       extends Mu[A]       { def copy: MuAny[A]  = new MuAny[A](value)  ; var value = init }
  def apply(u: Unit):    MuUnit.type = MuUnit
  def apply(z: Boolean): MuBoolean   = new MuBoolean(z)
  def apply(b: Byte):    MuByte      = new MuByte(b)
  def apply(s: Short):   MuShort     = new MuShort(s)
  def apply(c: Char):    MuChar      = new MuChar(c)
  def apply(i: Int):     MuInt       = new MuInt(i)
  def apply(l: Long):    MuLong      = new MuLong(l)
  def apply(f: Float):   MuFloat     = new MuFloat(f)
  def apply(d: Double):  MuDouble    = new MuDouble(d)
  def apply[A](a: A):    Mu[A]       = new MuAny(a)
}

extension [A, M <: Mu[A]](mu: M)
  transparent inline def specific: Any = inline mu match
    case mz: Mu.MuBoolean => mz
    case mb: Mu.MuByte    => mb
    case ms: Mu.MuShort   => ms
    case mc: Mu.MuChar    => mc
    case mi: Mu.MuInt     => mi
    case ml: Mu.MuLong    => ml
    case mf: Mu.MuFloat   => mf
    case md: Mu.MuDouble  => md
    case mv: Mu[Unit]     => Mu.MuUnit
    case miq: Mu[Boolean]  => miq match { case mi: Mu.MuBoolean => mi; case _ => Mu(miq.value) }
    case miq: Mu[Byte   ]  => miq match { case mi: Mu.MuByte    => mi; case _ => Mu(miq.value) }
    case miq: Mu[Short  ]  => miq match { case mi: Mu.MuShort   => mi; case _ => Mu(miq.value) }
    case miq: Mu[Char   ]  => miq match { case mi: Mu.MuChar    => mi; case _ => Mu(miq.value) }
    case miq: Mu[Int    ]  => miq match { case mi: Mu.MuInt     => mi; case _ => Mu(miq.value) }
    case miq: Mu[Long   ]  => miq match { case mi: Mu.MuLong    => mi; case _ => Mu(miq.value) }
    case miq: Mu[Float  ]  => miq match { case mi: Mu.MuFloat   => mi; case _ => Mu(miq.value) }
    case miq: Mu[Double ]  => miq match { case mi: Mu.MuDouble  => mi; case _ => Mu(miq.value) }
    case _ => mu



/** Hides data from case classes, etc.
  *
  * Do NOT use in hash maps!  Every anonymous value looks like every other!
  */
final class Anon[A](val value: A) {
  def map[B](f: A => B) = new Anon(f(value))
  inline def use(f: A => Unit): this.type = { f(value); this }
  override def toString = "..."
  override def hashCode = 1239182
  override def equals(a: Any) = a match {
    case _: Anon[_] => true
    case _ => false
  }
}
object Anon {
  def apply[A](a: A) = new Anon(a)
}


/** Box that uses reference equality and identity hash code */
final class Identity[A](val value: A) {
  def map[B](f: A => B) = new Identity(f(value))
  inline def use(f: A => Unit): this.type = { f(value); this }
  override def toString = value.toString
  override def hashCode = java.lang.System.identityHashCode(value)
  override def equals(a: Any) = a.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]
}


/** Way to create new unboxed types.  Use
  * ```
  * object MyThing extends NewType[OldThing] {
  *   extension (t: Type) def myMethodOnMyThing: Bar = whateverComputation(t.value)
  * }
  * ```
  *
  * To generate a new value, use `val m = MyThing(x)`.  To get the value, use `m.value`.
  * In addition to that, you can use any extension methods you have defined.
  */
trait NewType[A] {
  opaque type Type = A

  /** Create the newtype */
  inline def apply(a: A): Type = a

  /** Get the value from the newtype--this is the ONLY way to interact with it */
  extension (t: Type) inline def value: A = t

  /** Defer to the wrapped type's CanEqual */
  given (using CanEqual[A, A]): CanEqual[Type, Type] = CanEqual.derived
}



//////////////////////////////////////////////////////////////////////////////////
/// Generally helpful evaluation/execution utilities for singletons and tuples ///
//////////////////////////////////////////////////////////////////////////////////

extension[A <: AnyRef](a: A)
  inline def identityHash: Int = java.lang.System.identityHashCode(a)


extension [A](a: A) {
  /** Apply a function to this value and return the result.  Same as `pipe`. */
  inline def fn[B](inline f: A => B): B = f(a)

  /** Apply a function to this value and return the result.  Same as `fn`. */
  inline def pipe[B](inline f: A => B): B = f(a)

  /** Apply a side-effecting function to this value; return the original value */
  inline def tap(inline f: A => Unit): A = { f(a); a }


  /** Make a tuple with this value and another.  Equivalent to `a -> z`. */
  inline def tup[Z](inline z: Z): (A, Z) = (a, z)

  /** Make a tuple with this value and another, by putting the other value in slot 1.  Equivalent to `z -> a`. */
  inline def tup_1[Z](inline z: Z): (Z, A) = (z, a)

  /** Make a tuple by applying a function to this value, and keeping both this value and the result. */
  inline def tupWith[Z](inline f: A => Z): (A, Z) = (a, f(a))
}


extension [A, B](q: (A, B)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B) = (z, q._2)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B) = (zfn(q._1), q._2)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z) = (q._1, z)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z) = (q._1, zfn(q._2))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y](inline az: A => Z, inline by: B => Y): (Z, Y) = (az(q._1), by(q._2))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B) => Z): (Z, Z) = (zfn(q._1), zfn(q._2))

  /** Pass the values of this tuple into a two-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B) => Z): Z = zfn(q._1, q._2)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B](zop: (Z, Z) => Z) = zop(q._1, q._2)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, Z) = (q._1, q._2, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B) = (z, q._1, q._2)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B) = (q._1, z, q._2)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B) => Z): (A, B, Z) = (q._1, q._2, zfn(q._1, q._2))


  /** Cut out the first value of this tuple, leaving only the second. */
  inline def snip_1: B = q._2

  /** Cut out the last value of this tuple, leaving only the first. */
  inline def snip: A = q._1

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, Y, Z) = (q._1, q._2, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4)

  /** Concatenate this tuple with a 5-tuple. */
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5)

  /** Concatenate this tuple with a 6-tuple. */
  inline def join[U, V, W, X, Y, Z](p: (U, V, W, X, Y, Z)): (A, B, U, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5, p._6)

  /** Concatenate this tuple with a 7-tuple. */
  inline def join[T, U, V, W, X, Y, Z](p: (T, U, V, W, X, Y, Z)): (A, B, T, U, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5, p._6, p._7)
}


extension [A, B, C](q: (A, B, C)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C) = (z, q._2, q._3)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C) = (zfn(q._1), q._2, q._3)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C) = (q._1, z, q._3)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C) = (q._1, zfn(q._2), q._3)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z) = (q._1, q._2, z)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z) = (q._1, q._2, zfn(q._3))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X](inline az: A => Z, inline by: B => Y, inline cx: C => X): (Z, Y, X) = (az(q._1), by(q._2), cx(q._3))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C) => Z): (Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3))

  /** Pass the values of this tuple into a three-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C) => Z): Z = zfn(q._1, q._2, q._3)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C](zop: (Z, Z) => Z) = zop(zop(q._1, q._2), q._3)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, Z) = (q._1, q._2, q._3, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C) = (z, q._1, q._2, q._3)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C) = (q._1, z, q._2, q._3)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C) = (q._1, q._2, z, q._3)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C) => Z): (A, B, C, Z) = (q._1, q._2, q._3, zfn(q._1, q._2, q._3))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C) = (q._2, q._3)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C) = (q._1, q._3)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B) = (q._1, q._2)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, Y, Z) = (q._1, q._2, q._3, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4)

  /** Concatenate this tuple with a 5-tuple */
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, C, V, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4, p._5)

  /** Concatenate this tuple with a 6-tuple */
  inline def join[U, V, W, X, Y, Z](p: (U, V, W, X, Y, Z)): (A, B, C, U, V, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4, p._5, p._6)

  /** Split this tuple after element 1, creating a singleton and a 2-tuple. */
  inline def cutAt1: (A, (B, C)) = (q._1, (q._2, q._3))

  /** Split this tuple after element 2, creating a 2-tuple and a singleton. */
  inline def cutAt2: ((A, B), C) = ((q._1, q._2), q._3)
}


extension [A, B, C, D](q: (A, B, C, D)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D) = (z, q._2, q._3, q._4)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D) = (zfn(q._1), q._2, q._3, q._4)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D) = (q._1, z, q._3, q._4)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D) = (q._1, zfn(q._2), q._3, q._4)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D) = (q._1, q._2, z, q._4)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D) = (q._1, q._2, zfn(q._3), q._4)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z) = (q._1, q._2, q._3, z)

  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z) = (q._1, q._2, q._3, zfn(q._4))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W): (Z, Y, X, W) = (az(q._1), by(q._2), cx(q._3), dw(q._4))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D) => Z): (Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4))

  /** Pass the values of this tuple into a four-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D) => Z): Z = zfn(q._1, q._2, q._3, q._4)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D](zop: (Z, Z) => Z) = zop(zop(zop(q._1, q._2), q._3), q._4)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D) = (z, q._1, q._2, q._3, q._4)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D) = (q._1, z, q._2, q._3, q._4)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D) = (q._1, q._2, z, q._3, q._4)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D) = (q._1, q._2, q._3, z, q._4)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D) => Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, zfn(q._1, q._2, q._3, q._4))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D) = (q._2, q._3, q._4)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D) = (q._1, q._3, q._4)

  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D) = (q._1, q._2, q._4)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C) = (q._1, q._2, q._3)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, D, W, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3, p._4)

  /** Concatenate this tuple with a 5-tuple */
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, C, D, V, W, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3, p._4, p._5)

  /** Split this tuple after element 1, creating a singleton and a 3-tuple. */
  inline def cutAt1: (A, (B, C, D)) = (q._1, (q._2, q._3, q._4))

  /** Split this tuple after element 2, creating two 2-tuples. */
  inline def cutAt2: ((A, B), (C, D)) = ((q._1, q._2), (q._3, q._4))

  /** Split this tuple after element 3, creating a 3-tuple and a singleton. */
  inline def cutAt3: ((A, B, C), D) = ((q._1, q._2, q._3), q._4)
}

extension [A, B, C, D, E](q: (A, B, C, D, E)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E) = (z, q._2, q._3, q._4, q._5)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E) = (zfn(q._1), q._2, q._3, q._4, q._5)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E) = (q._1, z, q._3, q._4, q._5)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E) = (q._1, zfn(q._2), q._3, q._4, q._5)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E) = (q._1, q._2, z, q._4, q._5)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E) = (q._1, q._2, zfn(q._3), q._4, q._5)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E) = (q._1, q._2, q._3, z, q._5)

  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E) = (q._1, q._2, q._3, zfn(q._4), q._5)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, z)

  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, zfn(q._5))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V): (Z, Y, X, W, V) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E) => Z): (Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5))
  
  /** Pass the values of this tuple into a five-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E](zop: (Z, Z) => Z) = zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E) = (z, q._1, q._2, q._3, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E) = (q._1, z, q._2, q._3, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E) = (q._1, q._2, z, q._3, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E) = (q._1, q._2, q._3, z, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E) = (q._1, q._2, q._3, q._4, z, q._5)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E) => Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, zfn(q._1, q._2, q._3, q._4, q._5))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E) = (q._2, q._3, q._4, q._5)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E) = (q._1, q._3, q._4, q._5)

  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E) = (q._1, q._2, q._4, q._5)

  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E) = (q._1, q._2, q._3, q._5)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D) = (q._1, q._2, q._3, q._4)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, E, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, D, E, W, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2, p._3, p._4)

  /** Split this tuple after element 1, creating a singleton and a 4-tuple. */
  inline def cutAt1: (A, (B, C, D, E)) = (q._1, (q._2, q._3, q._4, q._5))

  /** Split this tuple after element 2, creating a 2-tuple and a 3-tuple. */
  inline def cutAt2: ((A, B), (C, D, E)) = ((q._1, q._2), (q._3, q._4, q._5))

  /** Split this tuple after element 3, creating a 3-tuple and a 2-tuple. */
  inline def cutAt3: ((A, B, C), (D, E)) = ((q._1, q._2, q._3), (q._4, q._5))

  /** Split this tuple after element 4, creating a 4-tuple and a singleton. */
  inline def cutAt4: ((A, B, C, D), E) = ((q._1, q._2, q._3, q._4), q._5)
}


extension [A, B, C, D, E, F](q: (A, B, C, D, E, F)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F) = (z, q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F) = (q._1, z, q._3, q._4, q._5, q._6)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F) = (q._1, q._2, z, q._4, q._5, q._6)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F) = (q._1, q._2, q._3, z, q._5, q._6)

  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F) = (q._1, q._2, q._3, q._4, z, q._6)

  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, z)

  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U): (Z, Y, X, W, V, U) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F) => Z): (Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6))
  
  /** Pass the values of this tuple into a six-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F) = (z, q._1, q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F) = (q._1, z, q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F) = (q._1, q._2, z, q._3, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F) = (q._1, q._2, q._3, z, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F) = (q._1, q._2, q._3, q._4, z, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 6 and the rest arranged around it in order */
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F) = (q._1, q._2, q._3, q._4, q._5, z, q._6)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E, F) => Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._1, q._2, q._3, q._4, q._5, q._6))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F) = (q._2, q._3, q._4, q._5, q._6)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F) = (q._1, q._3, q._4, q._5, q._6)

  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F) = (q._1, q._2, q._4, q._5, q._6)

  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F) = (q._1, q._2, q._3, q._5, q._6)

  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F) = (q._1, q._2, q._3, q._4, q._6)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E) = (q._1, q._2, q._3, q._4, q._5)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, F, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, p._1, p._2)
  
  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, E, F, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, p._1, p._2, p._3)

  /** Split this tuple after element 1, creating a singleton and a 5-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F)) = (q._1, (q._2, q._3, q._4, q._5, q._6))

  /** Split this tuple after element 2, creating a 2-tuple and a 4-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F)) = ((q._1, q._2), (q._3, q._4, q._5, q._6))

  /** Split this tuple after element 3, creating two 3-tuples. */
  inline def cutAt3: ((A, B, C), (D, E, F)) = ((q._1, q._2, q._3), (q._4, q._5, q._6))

  /** Split this tuple after element 4, creating a 4-tuple and a 2-tuple. */
  inline def cutAt4: ((A, B, C, D), (E, F)) = ((q._1, q._2, q._3, q._4), (q._5, q._6))

  /** Split this tuple after element 5, creating a 5-tuple and a singleton. */
  inline def cutAt5: ((A, B, C, D, E), F) = ((q._1, q._2, q._3, q._4, q._5), q._6)
}


extension [A, B, C, D, E, F, G](q: (A, B, C, D, E, F, G)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F, G) = (z, q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F, G) = (q._1, z, q._3, q._4, q._5, q._6, q._7)
  
  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F, G) = (q._1, q._2, z, q._4, q._5, q._6, q._7)
  
  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F, G) = (q._1, q._2, q._3, z, q._5, q._6, q._7)
  
  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F, G) = (q._1, q._2, q._3, q._4, z, q._6, q._7)
  
  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z, G) = (q._1, q._2, q._3, q._4, q._5, z, q._7)
  
  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7)

  /** Create a new tuple with the seventh value replaced. */
  inline def _7to[Z](inline z: Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, z)
  
  /** Create a new tuple with the seventh value computed from the old one using a function. */
  inline def _7op[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U, T](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T): (Z, Y, X, W, V, U, T) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F | G) => Z): (Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7))
  
  /** Pass the values of this tuple into a seven-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F, G) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F | G](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F, G) = (z, q._1, q._2, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F, G) = (q._1, z, q._2, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F, G) = (q._1, q._2, z, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F, G) = (q._1, q._2, q._3, z, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F, G) = (q._1, q._2, q._3, q._4, z, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 6 and the rest arranged around it in order */
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F, G) = (q._1, q._2, q._3, q._4, q._5, z, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 7 and the rest arranged around it in order */
  inline def tup_7[Z](inline z: Z): (A, B, C, D, E, F, Z, G) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._7)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E, F, G) => Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F, G) = (q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F, G) = (q._1, q._3, q._4, q._5, q._6, q._7)
  
  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F, G) = (q._1, q._2, q._4, q._5, q._6, q._7)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F, G) = (q._1, q._2, q._3, q._5, q._6, q._7)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F, G) = (q._1, q._2, q._3, q._4, q._6, q._7)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what's left. */
  inline def snip_6: (A, B, C, D, E, G) = (q._1, q._2, q._3, q._4, q._5, q._7)
  
  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E, F) = (q._1, q._2, q._3, q._4, q._5, q._6)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, F, G, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, p._1, p._2)

  /** Split this tuple after element 1, creating a singleton and a 6-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F, G)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7))

  /** Split this tuple after element 2, creating a 2-tuple and a 5-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F, G)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7))

  /** Split this tuple after element 3, creating a 3-tuple and a 4-tuple. */
  inline def cutAt3: ((A, B, C), (D, E, F, G)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7))

  /** Split this tuple after element 4, creating a 4-tuple and a 3-tuple. */
  inline def cutAt4: ((A, B, C, D), (E, F, G)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7))

  /** Split this tuple after element 5, creating a 5-tuple and a 2-tuple. */
  inline def cutAt5: ((A, B, C, D, E), (F, G)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7))

  /** Split this tuple after element 6, creating a 6-tuple and a singleton. */
  inline def cutAt6: ((A, B, C, D, E, F), G) = ((q._1, q._2, q._3, q._4, q._5, q._6), q._7)
}


extension [A, B, C, D, E, F, G, H](q: (A, B, C, D, E, F, G, H)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F, G, H) = (z, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G, H) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7, q._8)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F, G, H) = (q._1, z, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G, H) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7, q._8)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F, G, H) = (q._1, q._2, z, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G, H) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7, q._8)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F, G, H) = (q._1, q._2, q._3, z, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G, H) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7, q._8)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F, G, H) = (q._1, q._2, q._3, q._4, z, q._6, q._7, q._8)
  
  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G, H) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7, q._8)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, z, q._7, q._8)
  
  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7, q._8)

  /** Create a new tuple with the seventh value replaced. */
  inline def _7to[Z](inline z: Z): (A, B, C, D, E, F, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._8)
  
  /** Create a new tuple with the seventh value computed from the old one using a function. */
  inline def _7op[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7), q._8)

  /** Create a new tuple with the eighth value replaced. */
  inline def _8to[Z](inline z: Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z)
  
  /** Create a new tuple with the eighth value computed from the old one using a function. */
  inline def _8op[Z](inline zfn: H => Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._8))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U, T, S](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T, inline hs: H => S): (Z, Y, X, W, V, U, T, S) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7), hs(q._8))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F | G | H) => Z): (Z, Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7), zfn(q._8))
  
  /** Pass the values of this tuple into an eight-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F, G, H) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F | G | H](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7), q._8)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, z)
  
  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F, G, H) = (z, q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F, G, H) = (q._1, z, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F, G, H) = (q._1, q._2, z, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F, G, H) = (q._1, q._2, q._3, z, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F, G, H) = (q._1, q._2, q._3, q._4, z, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 6 and the rest arranged around it in order */
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F, G, H) = (q._1, q._2, q._3, q._4, q._5, z, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 7 and the rest arranged around it in order */
  inline def tup_7[Z](inline z: Z): (A, B, C, D, E, F, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 8 and the rest arranged around it in order */
  inline def tup_8[Z](inline z: Z): (A, B, C, D, E, F, G, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z, q._8)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E, F, G, H) => Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8))

  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F, G, H) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F, G, H) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F, G, H) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F, G, H) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F, G, H) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what's left. */
  inline def snip_6: (A, B, C, D, E, G, H) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8)
  
  /** Cut out the seventh value of this tuple, creating a new tuple from what's left. */
  inline def snip_7: (A, B, C, D, E, F, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8)
  
  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E, F, G) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7)

  /** Split this tuple after element 1, creating a singleton and a 7-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F, G, H)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 2, creating a 2-tuple and a 6-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F, G, H)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 3, creating a 3-tuple and a 5-tuple. */
  inline def cutAt3: ((A, B, C), (D, E, F, G, H)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 4, creating two 4-tuples. */
  inline def cutAt4: ((A, B, C, D), (E, F, G, H)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 5, creating a 5-tuple and a 3-tuple. */
  inline def cutAt5: ((A, B, C, D, E), (F, G, H)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7, q._8))
  
  /** Split this tuple after element 6, creating a 6-tuple and a 2-tuple. */
  inline def cutAt6: ((A, B, C, D, E, F), (G, H)) = ((q._1, q._2, q._3, q._4, q._5, q._6), (q._7, q._8))

  /** Split this tuple after element 7, creating a 7-tuple and a singleton. */
  inline def cutAt7: ((A, B, C, D, E, F, G), H) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7), q._8)
}


extension [A, B, C, D, E, F, G, H, I](q: (A, B, C, D, E, F, G, H, I)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F, G, H, I) = (z, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G, H, I) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F, G, H, I) = (q._1, z, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G, H, I) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F, G, H, I) = (q._1, q._2, z, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G, H, I) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F, G, H, I) = (q._1, q._2, q._3, z, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G, H, I) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F, G, H, I) = (q._1, q._2, q._3, q._4, z, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G, H, I) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7, q._8, q._9)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z, G, H, I) = (q._1, q._2, q._3, q._4, q._5, z, q._7, q._8, q._9)
  
  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G, H, I) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7, q._8, q._9)

  /** Create a new tuple with the seventh value replaced. */
  inline def _7to[Z](inline z: Z): (A, B, C, D, E, F, Z, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._8, q._9)
  
  /** Create a new tuple with the seventh value computed from the old one using a function. */
  inline def _7op[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7), q._8, q._9)

  /** Create a new tuple with the eigth value replaced. */
  inline def _8to[Z](inline z: Z): (A, B, C, D, E, F, G, Z, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z, q._9)
  
  /** Create a new tuple with the eigth value computed from the old one using a function. */
  inline def _8op[Z](inline zfn: H => Z): (A, B, C, D, E, F, G, Z, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._8), q._9)

  /** Create a new tuple with the ninth value replaced. */
  inline def _9to[Z](inline z: Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, z)
  
  /** Create a new tuple with the ninth value computed from the old one using a function. */
  inline def _9op[Z](inline zfn: I => Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, zfn(q._9))

  
  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U, T, S, R](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T, inline hs: H => S, inline ir: I => R): (Z, Y, X, W, V, U, T, S, R) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7), hs(q._8), ir(q._9))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F | G | H | I) => Z): (Z, Z, Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7), zfn(q._8), zfn(q._9))
  
  /** Pass the values of this tuple into a nine-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F, G, H, I) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F | G | H | I](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7), q._8), q._9)


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F, G, H, I) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F, G, H, I) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F, G, H, I) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F, G, H, I) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F, G, H, I) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8, q._9)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what's left. */
  inline def snip_6: (A, B, C, D, E, G, H, I) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8, q._9)
  
  /** Cut out the seventh value of this tuple, creating a new tuple from what's left. */
  inline def snip_7: (A, B, C, D, E, F, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8, q._9)
  
  /** Cut out the eigth value of this tuple, creating a new tuple from what's left. */
  inline def snip_8: (A, B, C, D, E, F, G, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._9)
  
  /** Cut out the ninth value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E, F, G, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)

  /** Split this tuple after element 1, creating a singleton and a 7-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F, G, H, I)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 2, creating a 2-tuple and a 7-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F, G, H, I)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 3, creating a 3-tuple and a 6-tuple. */
  inline def cutAt3: ((A, B, C), (D, E, F, G, H, I)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 4, creating a 4-tuple and a 5-tuple. */
  inline def cutAt4: ((A, B, C, D), (E, F, G, H, I)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 5, creating a 5-tuple and a 4-tuple. */
  inline def cutAt5: ((A, B, C, D, E), (F, G, H, I)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 6, creating a 6-tuple and a 3-tuple. */
  inline def cutAt6: ((A, B, C, D, E, F), (G, H, I)) = ((q._1, q._2, q._3, q._4, q._5, q._6), (q._7, q._8, q._9))
  
  /** Split this tuple after element 7, creating a 7-tuple and a 2-tuple. */
  inline def cutAt7: ((A, B, C, D, E, F, G), (H, I)) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7), (q._8, q._9))
  
  /** Split this tuple after element 8, creating an 8-tuple and a singleton. */
  inline def cutAt8: ((A, B, C, D, E, F, G, H), I) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8), q._9)
}
