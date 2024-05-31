// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-23 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.flow

import scala.reflect.ClassTag

import kse.basics._
import kse.basics.intervals._

//////////////////////////////////////////////////////////////
/// Methods for dealing with errors and Ors in collections ///
//////////////////////////////////////////////////////////////

extension [X, Y, CC[_]](coll: CC[X Or Y])(using iter: scala.collection.generic.IsIterableOnce[CC[X Or Y]]) {
  def collectIs(using factory: scala.collection.Factory[X, CC[X]], id: iter.A =:= (X Or Y)): CC[X] =
    factory.fromSpecific(iter(coll).iterator.asInstanceOf[Iterator[X Or Y]].collect{ case xy if xy.isIs => xy.get })

  def collectAlt(using factory: scala.collection.Factory[Y, CC[Y]], id: iter.A =:= (X Or Y)): CC[Y] =
    factory.fromSpecific(iter(coll).iterator.asInstanceOf[Iterator[X Or Y]].collect{ case xy if xy.isAlt => xy.alt })

  def collectThem(using fax: scala.collection.Factory[X, CC[X]], fay: scala.collection.Factory[Y, CC[Y]], id: iter.A =:= (X Or Y)): (CC[X], CC[Y]) =
    val bx = fax.newBuilder
    val by = fay.newBuilder
    iter(coll).iterator.foreach{ i =>
      id(i).foreachThem(x => bx += x)(y => by += y)
    }
    (bx.result, by.result)
}

extension [X, Y](a: Array[X Or Y]) {
  def collectIs(using ClassTag[X]): Array[X] =
    var n = 0
    a.peek()(xy => if xy.isIs then n += 1)
    val xs = new Array[X](n)
    n = 0
    a.peek(){ xy => xy.foreach{ x => xs(n) = x; n += 1 } }
    xs

  def collectAlt(using ClassTag[Y]): Array[Y] =
    var n = 0
    a.peek()(xy => if xy.isAlt then n += 1)
    val ys = new Array[Y](n)
    n = 0
    a.peek(){ xy => xy.foreachAlt{ y => ys(n) = y; n += 1 } }
    ys

  def collectThem(using ClassTag[X], ClassTag[Y]): (Array[X], Array[Y]) =
    var n = 0
    a.peek()(xy => if xy.isIs then n += 1)
    val xs = new Array[X](n)
    val ys = new Array[Y](a.length - n)
    n = 0
    var m = 0
    a.peek()(xy => xy.foreachThem{ x => xs(n) = x; n += 1 }{ y => ys(m) = y; m += 1 })
    (xs, ys)
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
        if bie eq null then bie = fie.newBuilder
        bie += ((n, err))
      }
      n += 1
    if bie eq null then ba.result().asIs
    else (ba.result(), bie.result()).asAlt


extension [A](a: Array[A Or Err]) {
  def valid(using ClassTag[A]): Array[A] Or Err = Or.Ret:
    a.copyWith(_.?)

  def errors: Array[Err] =
    a.collectAlt

  def validOrErrors(using ClassTag[A]): Array[A] Or Array[Err] =
    var xs: Array[A] = null
    var es: Array[Err] = null
    var xi = 0
    var ei = 0
    a.peek(){ xe =>
      xe.fold{ x =>
        if es eq null then
          if xs eq null then
            xs = new Array[A](16 min a.length)
          else
            if xi == xs.length then xs.enlargeTo(if (xs.length >> 1) + (xs.length >> 2) >= xi then xs.length else 2*xi)
          xs(xi) = x
        xi += 1
      }{ e =>
        if es eq null then
          es = new Array[Err](8 min (a.length - xi))
          xs = null
        else if ei == es.length then
          es.enlargeTo(if ((a.length - xi) >> 1) >= ei then a.length - xi else 2*ei)
        es(ei) = e
        ei += 1
      }
    }
    if es ne null then Alt(es.shrinkTo(ei))
    else if xs ne null then Is(xs)
    else Is(new Array[A](0))

  def validOrIndexedResults(using ClassTag[A]): Array[A] Or (Array[A], Array[(Int, Err)]) =
    var xs: Array[A] = null
    var es: Array[(Int, Err)] = null
    var xi = 0
    var ei = 0
    a.visit(){ (xe, i) =>
      xe.fold{ x =>
        if xs eq null then
          xs = new Array[A](16 min (a.length - ei))
        else if xi == xs.length then
          val biggest = xs.length - ei
          xs.enlargeTo(if (biggest >> 1) + (biggest >> 2) >= xi then biggest else 2*xi)
        xs(xi) = x
        xi += 1
      }{ e =>
        if es eq null then
          es = new Array[(Int, Err)](8 min (a.length - xi))
        else if ei == es.length then
          es.enlargeTo(if ((a.length - xi) >> 1) >= ei then a.length - xi else 2*ei)
        es(ei) = (i, e)
        ei += 1
      }
    }
    if es eq null then
      if xs eq null then Is(new Array[A](0))
      else Is(xs)
    else
      if xs eq null then Alt((new Array[A](0), es))
      else Alt((xs.shrinkTo(xi), es.shrinkTo(ei)))
}


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


extension [A](a: Array[A])
  inline def validMap[B](f: A => B Or Err)(using ClassTag[B]): Array[B] Or Err = Or.Ret:
    a.breakable.copyWith(x => f(x).?)



//////////////////////////////////////////////////
/// Interface for traversing internal buffers  ///
//////////////////////////////////////////////////

trait RotatingBuffer[A] {
  def rotate(buffer: Mu[Array[A] Or Unit], interval: Mu.T[Iv]): Unit
  def recycling: Boolean = true
  final def asIterator: Iterator[(Array[A], Iv)] = RotatingBuffer.AsIterator(this)
  final def asCopyingIterator(using ClassTag[A]): Iterator[Array[A]] = RotatingBuffer.CopyIterator(this)
}
object RotatingBuffer {
  sealed abstract class Iterating[A, C](ib: RotatingBuffer[A], recycle: Boolean = true) extends Iterator[C] {
    protected val ma = Mu(().orIs[Array[A]])
    protected val miv = Mu(Iv(0, 0))
    protected var state = 0
    def hasNext: Boolean =
      if state > 0 then true
      else if state < 0 then false
      else
        if !recycle then ma.value = Alt.unit
        ib.rotate(ma, miv)
        if ma.value.isAlt then
          state = -1
          false
        else
          state = 1
          true
  }
  final class AsIterator[A](ib: RotatingBuffer[A]) extends Iterating[A, (Array[A], Iv)](ib, true) {
    def next: (Array[A], Iv) =
      if hasNext then
        state = 0
        (ma.value.get, miv.value)
      else Iterator.empty.next
  }
  final class CopyIterator[A](ib: RotatingBuffer[A])(using ClassTag[A]) extends Iterating[A, Array[A]](ib, false) {
    def next: Array[A] =
      if hasNext then
        state = 0
        val a = ma.value.get
        a.clip.select(miv.value)
      else Iterator.empty.next
  }

  final class Lines(source: Array[Char], where: Iv = Iv(0, Int.MaxValue)) extends RotatingBuffer[Char] {
    override def recycling = false
    private var i0 = math.max(0, where.i0)
    private val iN = math.min(source.length, where.iN)
    def rotate(buffer: Mu[Array[Char] Or Unit], interval: Mu.T[Iv]): Unit =
      if i0 >= iN then
        interval.value = Iv(0, 0)
        buffer.value = Alt.unit
      else
        val i = i0
        var c = ' '
        while i0 < iN && { c = source(i0); c != '\n' && c != '\r' } do i0 += 1
        interval.value = Iv(i, i0)
        if i0 < iN then
          i0 += 1
          if c == '\r' && i0 < iN && source(i0) == '\n' then i0 += 1
        buffer.value = Is(source)
  }

  final class TextLines(source: Array[Byte], where: Iv = Iv(0, Int.MaxValue)) extends RotatingBuffer[Byte] {
    override def recycling = false
    private var i0 = math.max(0, where.i0)
    private var iN = math.min(source.length, where.iN)
    def rotate(buffer: Mu[Array[Byte] Or Unit], interval: Mu.T[Iv]): Unit =
      if i0 >= iN then
        interval.value = Iv(0, 0)
        buffer.value = Alt.unit
      else
        val i = i0
        var b: Byte = 0
        while i0 < iN && { b = source(i0); b != (10: Byte) && b != (13: Byte) } do i0 += 1
        interval.value = Iv(i, i0)
        if i0 < iN then
          i0 += 1
          if b == (13: Byte) && i0 < iN && source(i0) == (10: Byte) then i0 += 1
        buffer.value = Is(source)
  }
}

extension (ac: Array[Char])
  inline def lines(): RotatingBuffer.Lines = RotatingBuffer.Lines(ac)
  inline def lines(iv: Iv | PIv): RotatingBuffer.Lines = RotatingBuffer.Lines(ac, Iv.of(iv, ac))
  inline def lines(inline rg: collection.immutable.Range): RotatingBuffer.Lines = RotatingBuffer.Lines(ac, Iv.of(rg))

extension (ab: Array[Byte])
  inline def textLines(): RotatingBuffer.TextLines = RotatingBuffer.TextLines(ab)
  inline def textLines(iv: Iv | PIv): RotatingBuffer.TextLines = RotatingBuffer.TextLines(ab, Iv.of(iv, ab))
  inline def textLines(inline rg: collection.immutable.Range): RotatingBuffer.TextLines = RotatingBuffer.TextLines(ab, Iv.of(rg))
