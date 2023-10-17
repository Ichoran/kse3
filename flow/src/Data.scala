// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-23 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.flow

/////////////////////////////////////////////
/// Methods for modifying arrays in place ///
/////////////////////////////////////////////

extension (ab: Array[Byte])
  inline def zapAll(inline f: Byte => Byte): ab.type =
    var i = 0
    while i < ab.length do
      ab(i) = f(ab(i))
      i += 1
    ab
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension (as: Array[Short])
  inline def zapAll(inline f: Short => Short): as.type =
    var i = 0
    while i < as.length do
      as(i) = f(as(i))
      i += 1
    as
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension (ac: Array[Char])
  inline def zapAll(inline f: Char => Char): ac.type =
    var i = 0
    while i < ac.length do
      ac(i) = f(ac(i))
      i += 1
    ac
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension (ai: Array[Int])
  inline def zapAll(inline f: Int => Int): ai.type =
    var i = 0
    while i < ai.length do
      ai(i) = f(ai(i))
      i += 1
    ai
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension (al: Array[Long])
  inline def zapAll(inline f: Long => Long): al.type =
    var i = 0
    while i < al.length do
      al(i) = f(al(i))
      i += 1
    al
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension (af: Array[Float])
  inline def zapAll(inline f: Float => Float): af.type =
    var i = 0
    while i < af.length do
      af(i) = f(af(i))
      i += 1
    af
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension (ad: Array[Double])
  inline def zapAll(inline f: Double => Double): ad.type =
    var i = 0
    while i < ad.length do
      ad(i) = f(ad(i))
      i += 1
    ad
  // use in OverloadedExtensions
  // zap in OverloadedExtensions

extension [A >: Null <: AnyRef](aa: Array[A])
  inline def zapAll(inline f: A => A): aa.type =
    var i = 0
    while i < aa.length do
      aa(i) = f(aa(i))
      i += 1
    aa
  // use in OverloadedExtensions
  // zap in OverloadedExtensions



//////////////////////////////////////////////////////////////
/// Methods for dealing with errors and Ors in collections ///
//////////////////////////////////////////////////////////////

extension [X, Y, CC[_]](coll: CC[X Or Y])(using iter: scala.collection.generic.IsIterableOnce[CC[X Or Y]]) {
  def collectIs(using factory: scala.collection.Factory[X, CC[X]], id: iter.A =:= (X Or Y)): CC[X] =
    factory.fromSpecific(iter(coll).iterator.asInstanceOf[Iterator[X Or Y]].collect{ case xy if xy.isIs => xy.get })

  def collectAlt(using factory: scala.collection.Factory[Y, CC[Y]], id: iter.A =:= (X Or Y)): CC[Y] =
    factory.fromSpecific(iter(coll).iterator.asInstanceOf[Iterator[X Or Y]].collect{ case xy if xy.isAlt => xy.alt })
}


extension [A, CC[_]](coll: CC[A Or Err])(using iter: scala.collection.generic.IsIterable[CC[A Or Err]]) {
  /** Collects all favored results as favored, or gives the first disfavored result as disfavored */
  def valid(using factory: scala.collection.Factory[A, CC[A]], id: iter.A =:= (A Or Err)): CC[A] Or Err = Or.Ret:
    val b = factory.newBuilder
    val i = iter(coll).iterator.asInstanceOf[Iterator[A Or Err]] // id witnesses that this must be the case
    while i.hasNext do
      b += i.next.?
    b.result()

  /** Extracts all disfavored results */
  def errors(using factory: scala.collection.Factory[Err, CC[Err]], id: iter.A =:= (A Or Err)): CC[Err] =
    val b = factory.newBuilder
    val i = iter(coll).iterator.asInstanceOf[Iterator[A Or Err]]
    while i.hasNext do
      i.next.foreachAlt(b += _)
    b.result()

  /** Collects all favored results as favored, or if there are any errors, collects all those as disfavored */
  def validOrErrors(
    using fa: scala.collection.Factory[A, CC[A]], ferr: scala.collection.Factory[Err, CC[Err]], id: iter.A =:= (A Or Err)
  ): CC[A] Or CC[Err] =
    var ba: scala.collection.mutable.Builder[A, CC[A]] = null
    var berr: scala.collection.mutable.Builder[Err, CC[Err]] = null
    val i = iter(coll).iterator.asInstanceOf[Iterator[A Or Err]]
    if i.hasNext then
      i.next.foreachThem{ a =>
        ba = fa.newBuilder; ba += a
      }{ err =>
        berr = ferr.newBuilder; berr += err
      }
      while (berr eq null) && i.hasNext do
        i.next.foreachThem{ ba += _ }{ err =>
          berr = ferr.newBuilder; berr += err
        }
      while i.hasNext do
        i.next.foreachAlt(berr += _)
      if berr ne null then berr.result().asAlt
      else ba.result().asIs
    else fa.newBuilder.result().asIs
}


extension [A, CC[_]](coll: CC[A Or Err])(using seq: scala.collection.generic.IsSeq[CC[A Or Err]])
  /** Collects all favored results as favored, or partitions the results into favored results and indexed disfavored results */
  def validOrIndexedResults(
    using fa: scala.collection.Factory[A, CC[A]], fie: scala.collection.Factory[(Int, Err), CC[(Int, Err)]], id: seq.A =:= (A Or Err)
  ): CC[A] Or (CC[A], CC[(Int, Err)]) =
    var ba: scala.collection.mutable.Builder[A, CC[A]] = fa.newBuilder
    var bie: scala.collection.mutable.Builder[(Int, Err), CC[(Int, Err)]] = null
    var n = 0
    val i = seq(coll).iterator.asInstanceOf[Iterator[A Or Err]]
    while i.hasNext do
      i.next.foreachThem(ba += _){ err =>
        if bie eq null then bie = fie.newBuilder; bie += ((n, err))
      }
      n += 1
    if bie eq null then ba.result().asIs
    else (ba.result(), bie.result()).asAlt


extension [A, CC[_]](coll: CC[A])(using iter: scala.collection.generic.IsIterable[CC[A]])
  /** Maps a value where an error might occur, returning the (first) error as disfavored,
    * and the mapped result as favored if no error is encountered
    */
  def validMap[B](f: A => B Or Err)(using factory: scala.collection.Factory[B, CC[B]], id: iter.A =:= A): CC[B] Or Err = Or.Ret:
    val b = factory.newBuilder
    val i = iter(coll).iterator.asInstanceOf[Iterator[A]]
    while i.hasNext do
      b += f(i.next).?
    b.result()
