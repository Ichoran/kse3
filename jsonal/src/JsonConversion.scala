// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.time._

import scala.util.control.NonFatal
import scala.reflect.ClassTag

import kse.flow._

object JsonConversion {
  private[this] val encoderOfUrlBase64 = java.util.Base64.getUrlEncoder

  // Utility function for decoder.  Note that 26 = 'a' - 'G' and 52 = '0' + 4.
  private[this] def c2i(c: Char) =
    if      (c >= 'A' && c <= 'Z') c - 'A'
    else if (c >= 'a' && c <= 'z') c - 'G'
    else if (c >= '0' && c <= '9') c + 4
    else if (c == '-')             62
    else if (c == '_')             63
    else                           -1

  // The Java encoder is fast and exception-free, so we use it
  def encodeUrl64(a: Array[Byte]): String = encoderOfUrlBase64.encodeToString(a)

  // Java default decoder is not as fast as this one, and also throws exceptions which we want to avoid
  def decodeUrl64(s: String, i0: Int = 0, iN: Int = Int.MaxValue): Jast.To[Array[Byte]] =
    var m = math.min(s.length, iN)
    var i = math.max(i0, 0)
    while (m > i && s.charAt(m-1) == '=') m -= 1
    val a = new Array[Byte](((3L*(m-i)) >> 2).toInt)
    var j = 0
    while (i < m - 3) { 
      var x = (c2i(s.charAt(i)) << 18)
      x = x | (c2i(s.charAt(i+1)) << 12)
      x = x | (c2i(s.charAt(i+2)) << 6)
      x = x | (c2i(s.charAt(i+3)))
      a(j+2) = (x&0xFF).toByte
      a(j+1) = ((x >> 8)&0xFF).toByte
      a(j) = (x >> 16).toByte
      if (x == -1) return Jast.To.error("Invalid character in Base64 encoded string", i)
      i += 4
      j += 3
    }
    var x = 0
    var n = 0
    if (i < m) {
      if (i == m - 1) return Jast.To.error("A single Base64 character (mod 4) is not a valid encoding!")
      else if (i == m - 2) {
        val x = (c2i(s.charAt(i)) << 6) | c2i(s.charAt(i+1))
        if ((x & 0xF) != 0) return Jast.To.error("Wrong encoding at end of block: these two chars do not fit in a byte")
        a(j) = (x >> 4).toByte
      }
      else {
        val x = (c2i(s.charAt(i)) << 12) | (c2i(s.charAt(i+1)) << 6) | c2i(s.charAt(i+2))
        if ((x & 0x3) != 0) return Jast.To.error("Wrong encoding at end of block: these three chars do not fit in two bytes")
        a(j+1) = ((x >> 2) & 0xFF).toByte
        a(j) = ((x >> 10) & 0xFF).toByte
      }
    }
    Yes(a)

  
  val patternForDuration = """PT(?:\d+H)?(?:\d+M)?(?:\d+(?:.\d+)?S)?""".r.pattern
  
  val patternForInstant = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z$""".r.pattern
  
  val patternForLocalDateTime = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?""".r.pattern
  
  val patternForOffsetDateTime = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?(?:Z|[+-]\d{2}:\d{2})""".r.pattern
  
  val patternForZonedDateTime = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?(?:Z|[+-]\d{2}:\d{2})(?:\[[^\]]+])?""".r.pattern

  def quote(s: String, longest: Int = 79): String =
    if (s.length - 2 > longest) s"'${s.take(longest-5)}...'"
    else s"'$s'"

  def prequote(pre: String, s: String, longest: Int = 79): String =
    if (pre.length > longest-7) pre + quote(s, 7)
    else pre + quote(s, longest-pre.length)

  def typeError(wanted: String, found: Json): No[JastError] =
    Jast.To.error(s"Expected $wanted but found a ${found.jsonClassName}")

  def boundsError(wanted: String, found: Json.Num): No[JastError] =
    Jast.To.error(s"Requested $wanted but value out of range: $found")

  def arrayBoundsError(wanted: String, found: Double, index: Int): No[JastError] =
    Jast.To.error(s"Expected $wanted in array but found $found at index $index")

  def arrayBoundsError(wanted: String, found: Json.Num, index: Int): No[JastError] =
    Jast.To.error(s"Expected $wanted in array but found $found at index $index")

  def arrayTypeError(wanted: String, found: Json, index: Int): No[JastError] =
    Jast.To.error(s"Expected $wanted in array but found ${found.jsonClassName} in index $index")

  def arrayTypeError(wanted: String, found: Jast, index: Int): No[JastError] = found match
    case j: Json => arrayTypeError(wanted, j, index)
    case je: JastError => No(JastError("Expected $wanted but found error at index $index", because = je))

  def fromArr[A](j: Jast)(wanted: String)(f: Json.Arr => Jast.To[A]) = j match
    case a: Json.Arr => f(a)
    case je: JastError => Jast.To.error(s"Could not create $wanted from error", why = je)
    case x: Json => Jast.To.error(s"Could not create $wanted: Json.Arr required but found ${x.jsonClassName}")

  def fromObj[A](j: Jast)(wanted: String)(f: Json.Obj => Jast.To[A]) = j match
    case o: Json.Obj => f(o)
    case je: JastError => Jast.To.error(s"Could not create $wanted from error condition", why = je)
    case x: Json => Jast.To.error(s"Could not create $wanted: Json.Obj required but found ${x.jsonClassName}")
}


//////////////////////////
/// Conversion to JSON ///
//////////////////////////


extension [A](a: A)(using Jsonize[A]) def json: Json = summon[Jsonize[A]].jsonize(a)

given identity_Jsonize[J <: Json]: Jsonize[J] with
  def jsonize(j: J) = j

given [J <: AsJson]: Jsonize[J] with
  def jsonize(j: J) = j.json

given Jsonize[Null]    = new { def jsonize(n: Null)    = Json.Null }
given Jsonize[Boolean] = new { def jsonize(b: Boolean) = Json.Bool(b) }
given Jsonize[Byte]    = new { def jsonize(b: Byte)    = Json.Num(b) }
given Jsonize[Short]   = new { def jsonize(s: Short)   = Json.Num(s) }
given Jsonize[Char]    = new { def jsonize(c: Char)    = Json.Str(c.toString) }
given Jsonize[Int]     = new { def jsonize(i: Int)     = Json.Num(i) }
given Jsonize[Long]    = new { def jsonize(l: Long)    = Json.Num(l) }
given Jsonize[Float]   = new { def jsonize(f: Float)   = Json.Num(f) }
given Jsonize[Double]  = new { def jsonize(d: Double)  = Json.Num(d) }
given Jsonize[String]  = new { def jsonize(s: String)  = Json.Str(s) }

given Jsonize[java.math.BigDecimal] = new { def jsonize(bd: java.math.BigDecimal) = Json.Num(bd) }

given Jsonize[Duration]       = new { def jsonize(d: Duration)       = Json.Str(d.toString) }
given Jsonize[Instant]        = new { def jsonize(i: Instant)        = Json.Str(i.toString) }
given Jsonize[LocalDateTime]  = new { def jsonize(l: LocalDateTime)  = Json.Str(l.toString) }
given Jsonize[OffsetDateTime] = new { def jsonize(o: OffsetDateTime) = Json.Str(o.toString) }
given Jsonize[ZonedDateTime]  = new { def jsonize(z: ZonedDateTime)  = Json.Str(z.toString) }

given jsonize_Array_Double: Jsonize[Array[Double]] = new { def jsonize(ad: Array[Double]) = Json.Arr.Dbl(ad) }
given jsonize_Array_Float:  Jsonize[Array[Float]]  = new { def jsonize(af: Array[Float])  = Jsonize.numericArray(af) }
given jsonize_Array_Long:   Jsonize[Array[Long]]   = new { def jsonize(al: Array[Long])   = Jsonize.numericArray(al) }
given jsonize_Array_Int:    Jsonize[Array[Int]]    = new { def jsonize(ai: Array[Int])    = Jsonize.numericArray(ai) }
given jsonize_Array_Char:   Jsonize[Array[Char]]   = new { def jsonize(ac: Array[Char])   = Jsonize.stringArray(ac) }
given jsonize_Array_Short:  Jsonize[Array[Short]]  = new { def jsonize(as: Array[Short])  = Jsonize.numericArray(as) }
given jsonize_Array_Byte:   Jsonize[Array[Byte]]   = new { def jsonize(ab: Array[Byte])   = Json.Str(JsonConversion.encodeUrl64(ab)) }
given [A](using Jsonize[A]): Jsonize[Array[A]] with
  def jsonize(aa: Array[A]) = Jsonize.arbitraryArray(aa)

given Jsonize[None.type] = new { def jsonize(n: None.type) = Json.Null }
given [A](using Jsonize[A]): Jsonize[Some[A]] with
  def jsonize(s: Some[A]) = summon[Jsonize[A]].jsonize(s.get)
given [A](using Jsonize[A]): Jsonize[Option[A]] with
  def jsonize(aa: Option[A]) = aa match
    case Some(a) => summon[Jsonize[A]].jsonize(a)
    case _       => Json.Null

given [L, R, E <: Either[L, R]](using Jsonize[L], Jsonize[R]): Jsonize[E] with
  def jsonize(e: E) = e match
    case Left(l)  => summon[Jsonize[L]].jsonize(l)
    case Right(r) => summon[Jsonize[R]].jsonize(r)

given [N](using Jsonize[N]): Jsonize[No[N]] with
  def jsonize(n: No[N]) = summon[Jsonize[N]].jsonize(n.no)
given [Y](using Jsonize[Y]): Jsonize[Yes[Y]] with
  def jsonize(y: Yes[Y]) = summon[Jsonize[Y]].jsonize(y.yes)
given [N, Y](using Jsonize[N], Jsonize[Y]): Jsonize[Ok[N, Y]] with
  def jsonize(ok: Ok[N, Y]) = ok match
    case Yes(y) => summon[Jsonize[Y]].jsonize(y)
    case No(n)  => summon[Jsonize[N]].jsonize(n)

given [A, S <: scala.collection.Seq[A]](using Jsonize[A]): Jsonize[S] with
  def jsonize(xs: S) =
    val a = new Array[Json](xs.length)
    val jz = summon[Jsonize[A]]
    ixFor(xs.iterator)((x, i) => a(i) = jz.jsonize(x))
    Json.Arr.All(a)

given [V, M <: scala.collection.Map[String, V]](using Jsonize[V]): Jsonize[M] with
  def jsonize(m: M) =
    val b = Json.Obj.builder
    val jz = summon[Jsonize[V]]
    m.foreach{ case (k, v) => b ~ (k, jz.jsonize(v)) }
    b ~ Json.Obj

given [A, B](using Jsonize[A], Jsonize[B]): Jsonize[(A, B)] with
  def jsonize(x: (A, B)) =
    val y = new Array[Json](2)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    Json.Arr.All(y)

given [A, B, C](using Jsonize[A], Jsonize[B], Jsonize[C]): Jsonize[(A, B, C)] with
  def jsonize(x: (A, B, C)) =
    val y = new Array[Json](3)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    Json.Arr.All(y)

given [A, B, C, D](using Jsonize[A], Jsonize[B], Jsonize[C], Jsonize[D]): Jsonize[(A, B, C, D)] with
  def jsonize(x: (A, B, C, D)) =
    val y = new Array[Json](4)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    y(3) = summon[Jsonize[D]].jsonize(x._4)
    Json.Arr.All(y)

given [A, B, C, D, E](using Jsonize[A], Jsonize[B], Jsonize[C], Jsonize[D], Jsonize[E]): Jsonize[(A, B, C, D, E)] with
  def jsonize(x: (A, B, C, D, E)) =
    val y = new Array[Json](5)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    y(3) = summon[Jsonize[D]].jsonize(x._4)
    y(4) = summon[Jsonize[E]].jsonize(x._5)
    Json.Arr.All(y)

given [A, B, C, D, E, F](using Jsonize[A], Jsonize[B], Jsonize[C], Jsonize[D], Jsonize[E], Jsonize[F]): Jsonize[(A, B, C, D, E, F)] with
  def jsonize(x: (A, B, C, D, E, F)) =
    val y = new Array[Json](6)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    y(3) = summon[Jsonize[D]].jsonize(x._4)
    y(4) = summon[Jsonize[E]].jsonize(x._5)
    y(5) = summon[Jsonize[F]].jsonize(x._6)
    Json.Arr.All(y)

given [A, B, C, D, E, F, G](
  using Jsonize[A], Jsonize[B], Jsonize[C], Jsonize[D], Jsonize[E], Jsonize[F], Jsonize[G]
): Jsonize[(A, B, C, D, E, F, G)] with
  def jsonize(x: (A, B, C, D, E, F, G)) =
    val y = new Array[Json](7)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    y(3) = summon[Jsonize[D]].jsonize(x._4)
    y(4) = summon[Jsonize[E]].jsonize(x._5)
    y(5) = summon[Jsonize[F]].jsonize(x._6)
    y(6) = summon[Jsonize[G]].jsonize(x._7)
    Json.Arr.All(y)

given [A, B, C, D, E, F, G, H](
  using Jsonize[A], Jsonize[B], Jsonize[C], Jsonize[D], Jsonize[E], Jsonize[F], Jsonize[G], Jsonize[H]
): Jsonize[(A, B, C, D, E, F, G, H)] with
  def jsonize(x: (A, B, C, D, E, F, G, H)) =
    val y = new Array[Json](8)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    y(3) = summon[Jsonize[D]].jsonize(x._4)
    y(4) = summon[Jsonize[E]].jsonize(x._5)
    y(5) = summon[Jsonize[F]].jsonize(x._6)
    y(6) = summon[Jsonize[G]].jsonize(x._7)
    y(7) = summon[Jsonize[H]].jsonize(x._8)
    Json.Arr.All(y)

given [A, B, C, D, E, F, G, H, I](
  using Jsonize[A], Jsonize[B], Jsonize[C], Jsonize[D], Jsonize[E], Jsonize[F], Jsonize[G], Jsonize[H], Jsonize[I]
): Jsonize[(A, B, C, D, E, F, G, H, I)] with
  def jsonize(x: (A, B, C, D, E, F, G, H, I)) =
    val y = new Array[Json](9)
    y(0) = summon[Jsonize[A]].jsonize(x._1)
    y(1) = summon[Jsonize[B]].jsonize(x._2)
    y(2) = summon[Jsonize[C]].jsonize(x._3)
    y(3) = summon[Jsonize[D]].jsonize(x._4)
    y(4) = summon[Jsonize[E]].jsonize(x._5)
    y(5) = summon[Jsonize[F]].jsonize(x._6)
    y(6) = summon[Jsonize[G]].jsonize(x._7)
    y(7) = summon[Jsonize[H]].jsonize(x._8)
    y(8) = summon[Jsonize[I]].jsonize(x._9)
    Json.Arr.All(y)


////////////////////////////
/// Conversion from JSON ///
////////////////////////////

given FromJson[Boolean] = new { 
  def apply(input: Json) = input match
    case b: Json.Bool => Yes(b.value)
    case _ => JsonConversion.typeError("Boolean", input)
}

given FromJson[Byte] = new {
  def apply(input: Json) = input match
    case n: Json.Num =>
      val d = n.double
      val b = d.toByte
      if (b == d) Yes(b) else JsonConversion.boundsError("Byte", n)
    case _ => JsonConversion.typeError("Byte", input)
}

given FromJson[Short] = new {
  def apply(input: Json) = input match
    case n: Json.Num =>
      val d = n.double
      val s = d.toShort
      if (s == d) Yes(s) else JsonConversion.boundsError("Short", n)
    case _ => JsonConversion.typeError("Short", input)
}

given FromJson[Char] = new {
  def apply(input: Json) = input match
    case s: Json.Str =>
      val t = s.text
      if (t.length != 1) Jast.To.error(s"Requested Char but found string of length ${t}: ${JsonConversion.quote(t, 8)}")
      else Yes(t charAt 0)
    case _ => JsonConversion.typeError("Char", input)
}

given FromJson[Int] = new {
  def apply(input: Json) = input match
    case n: Json.Num =>
      val d = n.double
      val i = d.toInt
      if (i == d) Yes(i) else JsonConversion.boundsError("Int", n)
    case _ => JsonConversion.typeError("Int", input)
}

given FromJson[Long] = new {
  def apply(input: Json) = input match
    case n: Json.Num =>
      if (n.isLong) Yes(n.long)
      else JsonConversion.boundsError("Long", n)
    case _ => JsonConversion.typeError("Long", input)
}

given FromJson[Float] = new {
  def apply(input: Json) = input match
    case n: Json.Num =>
      val d = n.double
      val f = d.toFloat
      if (f.isInfinite) JsonConversion.boundsError("Float", n)
      else Yes(f)
    case _: Json.Null.type => Yes(Float.NaN)
    case _ => JsonConversion.typeError("Float", input)
}

given FromJson[Double] = new {
  def apply(input: Json) = input match
    case n: Json.Num =>
      val d = n.double
      if (d.isInfinite) JsonConversion.boundsError("Double", n)
      else Yes(d)
    case _: Json.Null.type => Yes(Double.NaN)
    case _ => JsonConversion.typeError("Double", input)
}

given FromJson[String] = new { 
  def apply(input: Json) = input match
    case s: Json.Str => Yes(s.text)
    case _ => JsonConversion.typeError("String", input)
}


given FromJson[BigDecimal] = new {
  def apply(input: Json) = input match
    case n: Json.Num => Yes(n.big)
    case _ => JsonConversion.typeError("BigDecimal", input)
}


given FromJson[Duration] = new { 
  def apply(input: Json) = input match
    case s: Json.Str =>
      val text = s.text
      if (JsonConversion.patternForDuration.matcher(text).matches)
        try { return Yes(java.time.Duration.parse(text)) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(JsonConversion.prequote("Not formatted as a Duration: ", text))
    case _ => JsonConversion.typeError("Duration", input)
}

given FromJson[Instant] = new { 
  def apply(input: Json) = input match
    case s: Json.Str =>
      val text = s.text
      if (JsonConversion.patternForInstant.matcher(text).matches)
        try { return Yes(java.time.Instant.parse(text)) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(JsonConversion.prequote("Not formatted as an Instant: ", text))
    case _ => JsonConversion.typeError("Instant", input)
}

given FromJson[LocalDateTime] = new { 
  def apply(input: Json) = input match
    case s: Json.Str =>
      val text = s.text
      if (JsonConversion.patternForLocalDateTime.matcher(text).matches)
        try { return Yes(java.time.LocalDateTime.parse(text)) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(JsonConversion.prequote("Not formatted as a LocalDateTime: ", text))
    case _ => JsonConversion.typeError("LocalDateTime", input)
}

given FromJson[OffsetDateTime] = new { 
  def apply(input: Json) = input match
    case s: Json.Str =>
      val text = s.text
      if (JsonConversion.patternForOffsetDateTime.matcher(text).matches)
        try { return Yes(java.time.OffsetDateTime.parse(text)) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(JsonConversion.prequote("Not formatted as an OffsetDateTime: ", text))
    case _ => JsonConversion.typeError("OffsetDateTime", input)
}

given FromJson[ZonedDateTime] = new { 
  def apply(input: Json) = input match
    case s: Json.Str =>
      val text = s.text
      if (JsonConversion.patternForZonedDateTime.matcher(text).matches)
        try { return Yes(java.time.ZonedDateTime.parse(text)) }
        catch { case t if NonFatal(t) => }
      Jast.To.error(JsonConversion.prequote("Not formatted as a ZonedDateTime: ", text))
    case _ => JsonConversion.typeError("ZonedDateTime", input)
}

given fromJson_Array_Double: FromJson[Array[Double]] = new {
  def apply(input: Json) = input match
    case ad: Json.Arr.Dbl =>
      val ds = ad.doubles
      Yes(java.util.Arrays.copyOf(ds, ds.length))
    case aa: Json.Arr.All =>
      val a = new Array[Double](aa.size)
      nFor(aa.size){ i => aa(i) match
        case n: Json.Num => a(i) = n.double
        case _: Json.Null.type => a(i) = Double.NaN
        case x => return JsonConversion.arrayTypeError("Double", x, i)
      }
      Yes(a)
    case _ => JsonConversion.typeError("Array[Double]", input)
}

given fromJson_Array_Float: FromJson[Array[Float]] = new {
  def apply(input: Json) = input match
    case ad: Json.Arr.Dbl =>
      val ds = ad.doubles
      val a = new Array[Float](ds.length)
      aFor(ds){ (d, i) =>
        val f = d.toFloat
        if (f.isInfinite) return JsonConversion.arrayBoundsError("Float", d, i)
        a(i) = f
      }
      Yes(a)
    case aa: Json.Arr.All =>
      val a = new Array[Float](aa.size)
      nFor(aa.size){ i => aa(i) match
        case n: Json.Num =>
          val d = n.double
          val f = d.toFloat
          if (f.isInfinite) return JsonConversion.arrayBoundsError("Float", n, i)
          a(i) = f
        case _: Json.Null.type => a(i) = Float.NaN
        case x => return JsonConversion.arrayTypeError("Float", x, i)
      }
      Yes(a)
    case _ => JsonConversion.typeError("Array[Float]", input)
}

given fromJson_Array_Long: FromJson[Array[Long]] = new {
  def apply(input: Json) = input match
    case ad: Json.Arr.Dbl =>
      val ds = ad.doubles
      val a = new Array[Long](ds.length)
      aFor(ds){ (d, i) =>
        val l = d.toLong
        if (l == d) a(i) = l else return JsonConversion.arrayBoundsError("Long", d, i)
      }
      Yes(a)
    case aa: Json.Arr.All =>
      val a = new Array[Long](aa.size)
      nFor(aa.size){ i => aa(i) match
        case n: Json.Num =>
          if (n.isLong) a(i) = n.long else return JsonConversion.arrayBoundsError("Long", n, i)
        case x => return JsonConversion.arrayTypeError("Long", x, i)
      }
      Yes(a)
    case _ => JsonConversion.typeError("Array[Long]", input) 
}

given fromJson_Array_Int: FromJson[Array[Int]] = new {
  def apply(input: Json) = input match
    case ad: Json.Arr.Dbl =>
      val ds = ad.doubles
      val a = new Array[Int](ds.length)
      aFor(ds){(d, i) =>
        val j = d.toInt
        if (j == d) a(i) = j else return JsonConversion.arrayBoundsError("Int", d, i)
      }
      Yes(a)
    case aa: Json.Arr.All =>
      val a = new Array[Int](aa.size)
      nFor(aa.size){ i => aa(i) match 
        case n: Json.Num =>
          val d = n.double
          val j = d.toInt
          if (d == j) a(i) = j else return JsonConversion.arrayBoundsError("Int", n, i)
        case x => return JsonConversion.arrayTypeError("Int", x, i)
      }
      Yes(a)
    case _ => JsonConversion.typeError("Array[Int]", input) 
}

given fromJson_Array_Char: FromJson[Array[Char]] = new {
  def apply(input: Json) = input match
    case aa: Json.Arr.All =>
      val a = new Array[Char](aa.size)
      nFor(aa.size){ i => aa(i) match
        case s: Json.Str =>
          val text = s.text
          if (text.length != 1) return Jast.To.error(s"Expected Char in array but found ${JsonConversion.quote(text, 8)} at index $i")
        case x => return JsonConversion.arrayTypeError("Char", x, i)
      }
      Yes(a)
    case s: Json.Str => Yes(s.text.toCharArray)
    case _ => JsonConversion.typeError("Array[Char]", input)
}

given fromJson_Array_Short: FromJson[Array[Short]] = new {
  def apply(input: Json) = input match
    case ad: Json.Arr.Dbl =>
      val ds = ad.doubles
      val a = new Array[Short](ds.length)
      aFor(ds){ (d, i) =>
        val s = d.toShort
        if (s == d) a(i) = s else return JsonConversion.arrayBoundsError("Short", d, i)
      }
      Yes(a)
    case aa: Json.Arr.All =>
      val a = new Array[Short](aa.size)
      nFor(aa.size){ i => aa(i) match
        case n: Json.Num =>
          val d = n.double
          val s = d.toShort
          if (s == d) a(i) = s else return JsonConversion.arrayBoundsError("Short", n, i)
        case x => return JsonConversion.arrayTypeError("Short", x, i)
      }
      Yes(a)
    case _ => JsonConversion.typeError("Array[Short]", input) 
}

given fromJson_Array_Byte: FromJson[Array[Byte]] = new {
  def apply(input: Json) = input match
    case ad: Json.Arr.Dbl =>
      val ds = ad.doubles
      val a = new Array[Byte](ds.length)
      aFor(ds){ (d, i) =>
        val b = d.toByte
        if (b == d) a(i) = b else return JsonConversion.arrayBoundsError("Byte", d, i)
      }
      Yes(a)
    case aa: Json.Arr.All =>
      val a = new Array[Byte](aa.size)
      nFor(aa.size){ i => aa(i) match
        case n: Json.Num =>
          val d = n.double
          val b = d.toByte
          if (b == d) a(i) = b else return JsonConversion.arrayBoundsError("Byte", n, i)
        case x => return JsonConversion.arrayTypeError("Byte", x, i)
      }
      Yes(a)
    case s: Json.Str => JsonConversion.decodeUrl64(s.text)
    case _ => JsonConversion.typeError("Array[Byte]", input) 
}

given [A](using tag: ClassTag[A], fj: FromJson[A]): FromJson[Array[A]] with
  def apply(input: Json) = input match
    case ax: Json.Arr => fj.array(ax)
    case _ => JsonConversion.typeError("Array", input)


given [A](using fj: FromJson[A]): FromJson[Option[A]] with
  def apply(input: Json) = input match
    case _: Json.Null.type => Yes(None)
    case _ => fj(input).map(y => Some(y))

given [L, R](using fjl: FromJson[L], fjr: FromJson[R]): FromJson[Either[L, R]] with
  def apply(input: Json) =
    fjr(input) match
      case Yes(r) => Yes(Right(r))
      case No(re) => fjl(input) match
        case Yes(l) => Yes(Left(l))
        case No(le) => Jast.To.error(s"Left parse failed with $le", why = JastError("Right parse failed", because = re))

given [N, Y](using fjn: FromJson[N], fjy: FromJson[Y]): FromJson[Ok[N, Y]] with
  def apply(input: Json) =
    fjy(input) match
      case y: Yes[Y] => Yes(y)
      case No(ye) => fjn(input) match
        case Yes(n) => Yes(No(n))
        case No(ne) => Jast.To.error(s"No parse failed with $ne", why = JastError("Yes parse failed", because = ye))

given [CC[_], A](using fj: FromJson[A], factory: scala.collection.Factory[A, CC[A]]): FromJson[CC[A]] with
  def apply(input: Json) = input match
    case ja: Json.Arr =>
      val b = factory.newBuilder
      nFor(ja.size){ i => fj(ja(i)) match
        case Yes(a) => b += a
        case No(je) => return Jast.To.error(s"Collection failed on element $i", why = je)
      }
      Yes(b.result)
    case _ => JsonConversion.typeError("collection", input)

given [M[String, _] <: collection.Map[String, _], V](
  using fj: FromJson[V], factory: scala.collection.Factory[(String, V), M[String, V]]
): FromJson[M[String, V]] with
  def apply(input: Json) = input match
    case o: Json.Obj =>
      val b = factory.newBuilder
      val it = o.iterator
      iFor(it){ case (s, j) => fj(j) match
        case Yes(v) => b += s -> v
        case No(je) => return Jast.To.error(s"Map failed on entry $s", why = je)
      }
      Yes(b.result)
    case _ => JsonConversion.typeError("map", input)

given [A, B](using fja: FromJson[A], fjb: FromJson[B]): FromJson[(A, B)] with
  def apply(input: Json): Jast.To[(A, B)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 2) Jast.To.error(s"Cannot create 2-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?))
    case _ => JsonConversion.typeError("2-tuple", input)
  }

given [A, B, C](using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C]): FromJson[(A, B, C)] with
  def apply(input: Json): Jast.To[(A, B, C)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 3) Jast.To.error(s"Cannot create 3-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?))
    case _ => JsonConversion.typeError("3-tuple", input)
  }

given [A, B, C, D](using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D]): FromJson[(A, B, C, D)] with
  def apply(input: Json): Jast.To[(A, B, C, D)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 4) Jast.To.error(s"Cannot create 4-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?, fjd(x(3)).?))
    case _ => JsonConversion.typeError("4-tuple", input)
  }

given [A, B, C, D, E](
  using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E]
): FromJson[(A, B, C, D, E)] with
  def apply(input: Json): Jast.To[(A, B, C, D, E)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 5) Jast.To.error(s"Cannot create 5-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?, fjd(x(3)).?, fje(x(4)).?))
    case _ => JsonConversion.typeError("5-tuple", input)
  }

given [A, B, C, D, E, F](
  using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E], fjf: FromJson[F]
): FromJson[(A, B, C, D, E, F)] with
  def apply(input: Json): Jast.To[(A, B, C, D, E, F)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 6) Jast.To.error(s"Cannot create 6-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?, fjd(x(3)).?, fje(x(4)).?, fjf(x(5)).?))
    case _ => JsonConversion.typeError("6-tuple", input)
  }

given [A, B, C, D, E, F, G](
  using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E], fjf: FromJson[F], fjg: FromJson[G]
): FromJson[(A, B, C, D, E, F, G)] with
  def apply(input: Json): Jast.To[(A, B, C, D, E, F, G)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 7) Jast.To.error(s"Cannot create 7-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?, fjd(x(3)).?, fje(x(4)).?, fjf(x(5)).?, fjg(x(6)).?))
    case _ => JsonConversion.typeError("7-tuple", input)
  }

given [A, B, C, D, E, F, G, H](
  using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E],
        fjf: FromJson[F], fjg: FromJson[G], fjh: FromJson[H]
): FromJson[(A, B, C, D, E, F, G, H)] with
  def apply(input: Json): Jast.To[(A, B, C, D, E, F, G, H)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 8) Jast.To.error(s"Cannot create 8-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?, fjd(x(3)).?, fje(x(4)).?, fjf(x(5)).?, fjg(x(6)).?, fjh(x(7)).?))
    case _ => JsonConversion.typeError("8-tuple", input)
  }

given [A, B, C, D, E, F, G, H, I](
  using fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E],
        fjf: FromJson[F], fjg: FromJson[G], fjh: FromJson[H], fji: FromJson[I]
): FromJson[(A, B, C, D, E, F, G, H, I)] with
  def apply(input: Json): Jast.To[(A, B, C, D, E, F, G, H, I)] = Jast.Ret{ input match
    case x: Json.Arr =>
      if (x.size != 9) Jast.To.error(s"Cannot create 9-tuple from Json.Arr of length ${x.size}")
      Yes((fja(x(0)).?, fjb(x(1)).?, fjc(x(2)).?, fjd(x(3)).?, fje(x(4)).?, fjf(x(5)).?, fjg(x(6)).?, fjh(x(7)).?, fji(x(8)).?))
    case _ => JsonConversion.typeError("9-tuple", input)
  }
