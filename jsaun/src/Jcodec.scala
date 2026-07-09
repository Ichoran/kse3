// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import scala.compiletime.{constValueTuple, erasedValue, summonFrom}
import scala.deriving.Mirror

import kse.basics.{given, _}
import kse.flow.{given, _}


/** Turns an `A` into JSON.  Case classes get instances with `derives Jsonize`; nested case
  * classes are derived automatically when no instance is in scope.  (Deliberately invariant:
  * a contravariant Jsonize makes a sealed trait's own instance answer the summons for its
  * children during sum derivation, which is instant infinite recursion.)
  */
trait Jsonize[A] {
  def jsonize(a: A): Json

  /** Serialize straight to an output; override to skip building the tree. */
  def jsonizeTo(a: A, out: Jout): Unit = jsonize(a).printTo(out)
}
object Jsonize {
  def apply[A](using jz: Jsonize[A]): Jsonize[A] = jz

  given Jsonize[Json] = j => j
  given Jsonize[Boolean] = b => Jbool(b)
  given Jsonize[Int] = i => Jnum(i.toLong)
  given Jsonize[Long] = l => Jnum(l)
  given Jsonize[Double] = d => Jnum(d)
  given Jsonize[Float] = f => Jnum(f.toDouble)
  given Jsonize[String] = s => Jstr(s)
  given Jsonize[BigDecimal] = x => Jnum(x)

  given [A](using jz: Jsonize[A]): Jsonize[Option[A]] = {
    case Some(a) => jz.jsonize(a)
    case None => Jnull
  }

  given [A, CC[X] <: Iterable[X]](using jz: Jsonize[A]): Jsonize[CC[A]] = xs =>
    val vs = new Array[Json](xs.size)
    var k = 0
    val it = xs.iterator
    while it.hasNext do
      vs(k) = jz.jsonize(it.next())
      k += 1
    new Jarr.A(vs, k)

  given [A](using jz: Jsonize[A]): Jsonize[Array[A]] = xs =>
    val vs = new Array[Json](xs.length)
    var k = 0
    while k < xs.length do
      vs(k) = jz.jsonize(xs(k))
      k += 1
    new Jarr.A(vs, k)

  given Jsonize[Array[Double]] = xs => Jarr(xs)

  given [A, MM[K, V] <: scala.collection.Map[K, V]](using jz: Jsonize[A]): Jsonize[MM[String, A]] = m =>
    val ks = new Array[String](m.size)
    val vs = new Array[Json](m.size)
    var k = 0
    val it = m.iterator
    while it.hasNext do
      val (key, v) = it.next()
      ks(k) = key
      vs(k) = jz.jsonize(v)
      k += 1
    new Jobj(ks, vs, k)

  /** Derivation entry point for `derives Jsonize`: products become objects keyed by field
    * name; sums add a `"type"` discriminator to the child's object (or wrap a non-object
    * child as `{"type": ..., "value": ...}`).
    */
  inline def derived[A](using m: Mirror.Of[A]): Jsonize[A] = inline m match
    case pm: Mirror.ProductOf[A] =>
      productInstance[A](
        constValueTuple[pm.MirroredElemLabels].toList.map(_.toString).toArray,
        thunks[pm.MirroredElemTypes].toArray
      )
    case sm: Mirror.SumOf[A] =>
      sumInstance[A](
        constValueTuple[sm.MirroredElemLabels].toList.map(_.toString).toArray,
        thunks[sm.MirroredElemTypes].toArray,
        sm
      )

  private inline def of[T]: Jsonize[T] = summonFrom {
    case jz: Jsonize[T] => jz
    case m: Mirror.Of[T] => derived[T](using m)
  }

  private inline def thunks[T <: Tuple]: List[() => Jsonize[?]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => (() => of[h]) :: thunks[t]

  // Thunked and lazily resolved so recursive types (a case class containing itself in a
  // List, say) finish wiring before any instance is actually used
  private[jsaun] def productInstance[A](labels: Array[String], elems: Array[() => Jsonize[?]]): Jsonize[A] =
    new Jsonize[A] {
      private lazy val jzs = elems.map(_())
      def jsonize(a: A): Json =
        val p = a.asInstanceOf[Product]
        val n = labels.length
        val vs = new Array[Json](n)
        var k = 0
        while k < n do
          vs(k) = jzs(k).asInstanceOf[Jsonize[Any]].jsonize(p.productElement(k))
          k += 1
        new Jobj(labels, vs, n)   // labels shared: never mutated by an immutable Jobj
    }

  private[jsaun] def sumInstance[A](labels: Array[String], elems: Array[() => Jsonize[?]], sm: Mirror.SumOf[A]): Jsonize[A] =
    new Jsonize[A] {
      private lazy val jzs = elems.map(_())
      def jsonize(a: A): Json =
        val ord = sm.ordinal(a)
        jzs(ord).asInstanceOf[Jsonize[Any]].jsonize(a) match
          case o: Jobj =>
            val ks = new Array[String](o.n + 1)
            val vs = new Array[Json](o.n + 1)
            ks(0) = "type"
            vs(0) = Jstr(labels(ord))
            System.arraycopy(o.ks, 0, ks, 1, o.n)
            System.arraycopy(o.vs, 0, vs, 1, o.n)
            new Jobj(ks, vs, o.n + 1)
          case other => Jobj("type" -> Jstr(labels(ord)), "value" -> other)
    }
}


/** Reads an `A` out of JSON, or explains what went wrong.  Case classes get instances with
  * `derives FromJson`; nested case classes are derived automatically when no instance is in
  * scope.
  */
trait FromJson[A] {
  def from(j: Json): Ask[A]

  /** How a missing object field decodes: an error, unless overridden (as `Option` does). */
  def missing(key: String): Ask[A] = Alt(Err(s"missing key \"$key\""))
}
object FromJson {
  def apply[A](using fj: FromJson[A]): FromJson[A] = fj

  private val noErr: Err = Err("(no error)")

  given FromJson[Json] = j => Is(j)
  given FromJson[Boolean] = _.bool
  given FromJson[Long] = _.long
  given FromJson[Double] = _.dbl
  given FromJson[Float] = _.dbl.map(_.toFloat)
  given FromJson[String] = _.str
  given FromJson[Int] = _.long.flatMap{ l =>
    if l < Int.MinValue || l > Int.MaxValue then Alt(Err(s"integer out of Int range: $l"))
    else Is(l.toInt)
  }
  given FromJson[BigDecimal] = {
    case b: Jnum.Big => Is(b.big)
    case n: Jnum.L => Is(BigDecimal(n.value))
    case n: Jnum.D => Is(BigDecimal(n.value))
    case x => Alt(Json.expectErr("a number", x))
  }

  private final class OptionFrom[A](fj: FromJson[A]) extends FromJson[Option[A]] {
    def from(j: Json): Ask[Option[A]] = if j.isNull then Is(None) else fj.from(j).map(Some(_))
    override def missing(key: String): Ask[Option[A]] = Is(None)
  }
  given [A](using fj: FromJson[A]): FromJson[Option[A]] = new OptionFrom(fj)

  given [A, C[_]](using fj: FromJson[A], fac: scala.collection.Factory[A, C[A]]): FromJson[C[A]] = {
    case a: Jarr =>
      val get: Int => Json = a match
        case aa: Jarr.A => k => aa.vs(k)
        case _ => k => a.elem(k)
      val b = fac.newBuilder
      var bad = FromJson.noErr
      var badly = false
      var k = 0
      val n = a.size
      while !badly && k < n do
        fj.from(get(k)) match
          case Alt(e) =>
            bad = e.explainBy(s"in element $k:")
            badly = true
          case x => b.addOne(Is unwrap x.asInstanceOf[Is[A]]) __ Unit
        k += 1
      if badly then Alt(bad) else Is(b.result())
    case x => Alt(Json.expectErr("an array", x))
  }

  given FromJson[Array[Double]] = j => j.arr.flatMap(_.dbls)

  given [A](using fj: FromJson[A]): FromJson[Map[String, A]] = {
    case o: Jobj =>
      val b = Map.newBuilder[String, A]
      var bad = FromJson.noErr
      var badly = false
      var k = 0
      while !badly && k < o.n do
        fj.from(o.vs(k)) match
          case Alt(e) =>
            bad = e.explainBy(s"in value for key \"${o.ks(k)}\":")
            badly = true
          case x => b.addOne(o.ks(k) -> (Is unwrap x.asInstanceOf[Is[A]])) __ Unit
        k += 1
      if badly then Alt(bad) else Is(b.result())
    case x => Alt(Json.expectErr("an object", x))
  }

  /** Derivation entry point for `derives FromJson`: mirror of `Jsonize.derived`.  All field
    * failures are reported together (`ErrType.Many`), each with its key as context.
    */
  inline def derived[A](using m: Mirror.Of[A]): FromJson[A] = inline m match
    case pm: Mirror.ProductOf[A] =>
      productInstance[A](
        constValueTuple[pm.MirroredElemLabels].toList.map(_.toString).toArray,
        thunks[pm.MirroredElemTypes].toArray,
        pm
      )
    case sm: Mirror.SumOf[A] =>
      sumInstance[A](
        constValueTuple[sm.MirroredElemLabels].toList.map(_.toString).toArray,
        thunks[sm.MirroredElemTypes].toArray
      )

  private inline def of[T]: FromJson[T] = summonFrom {
    case fj: FromJson[T] => fj
    case m: Mirror.Of[T] => derived[T](using m)
  }

  private inline def thunks[T <: Tuple]: List[() => FromJson[?]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => (() => of[h]) :: thunks[t]

  private[jsaun] def productInstance[A](labels: Array[String], elems: Array[() => FromJson[?]], pm: Mirror.ProductOf[A]): FromJson[A] =
    new FromJson[A] {
      private lazy val fjs = elems.map(_())
      def from(j: Json): Ask[A] = j match
        case o: Jobj =>
          val n = labels.length
          val args = new Array[Any](n)
          var errs: List[Err] = Nil
          var k = 0
          while k < n do
            val fj = fjs(k).asInstanceOf[FromJson[Any]]
            val v = o.get(labels(k))
            val r = if v eq null then fj.missing(labels(k)) else fj.from(v)
            r match
              case Alt(e) => errs = e.explainBy(s"in field \"${labels(k)}\":") :: errs
              case x => args(k) = Is unwrap x.asInstanceOf[Is[Any]]
            k += 1
          errs match
            case Nil => Is(pm.fromProduct(Tuple.fromArray(args.asInstanceOf[Array[Object]])))
            case e :: Nil => Alt(e)
            case es => Alt(Err(es.reverse*)(s"${es.length} fields failed to decode"))
        case x => Alt(Json.expectErr("an object", x))
    }

  private[jsaun] def sumInstance[A](labels: Array[String], elems: Array[() => FromJson[?]]): FromJson[A] =
    new FromJson[A] {
      private lazy val fjs = elems.map(_())
      def from(j: Json): Ask[A] = j("type").str match
        case Alt(e) => Alt(e.explainBy("decoding the \"type\" discriminator:"))
        case t =>
          val name = Is unwrap t.asInstanceOf[Is[String]]
          val ord = labels.indexOf(name)
          if ord < 0 then Alt(Err(s"unknown type \"$name\" (expected one of ${labels.mkString(", ")})"))
          else
            val fj = fjs(ord).asInstanceOf[FromJson[A]]
            val r = fj.from(j) match
              case Alt(e) =>
                j("value").ask match   // non-object children were wrapped; unwrap and retry
                  case Alt(_) => Alt(e)
                  case v => fj.from(Is unwrap v.asInstanceOf[Is[Json]])
              case ok => ok
            r match
              case Alt(e) => Alt(e.explainBy(s"decoding \"$name\":"))
              case ok => ok
    }
}


extension (j: Json)
  /** Decode this JSON into an `A` via its `FromJson`. */
  def to[A](using fj: FromJson[A]): Ask[A] = fj.from(j)

extension (ja: JAny)
  /** Decode into an `A`; an error already present just flows through. */
  def to[A](using fj: FromJson[A]): Ask[A] = (ja: Any) match
    case _: Alt[?] => ja.asInstanceOf[Ask[A]]
    case j => fj.from(j.asInstanceOf[Json])
