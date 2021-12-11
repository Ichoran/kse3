// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import scala.language.higherKinds

import scala.util.control.NonFatal

import kse.flow._

/** Classes or objects implementing this trait are able to deserialize objects from a JSON representation. */
trait FromJson[A] {
  /** Recover the object from its JSON representation. */
  def apply(input: Json): Jast.To[A]

  /** Recover the object from its JSON representation if there was a JSON representation */
  def apply(input: Jast): Jast.To[A] = input match
    case je: JastError => No(je)
    case js: Json => apply(js)

  /** Recover the object from its JSON representation if there was a Jast.To[Json] parse */
  def apply(input: Jast.To[Json]): Jast.To[A] = input match
    case Yes(j) => apply(j)
    case n: No[_] => n

  /** Recover an array of these objects from their JSON representation */
  def array(input: Json.Arr)(using tag: reflect.ClassTag[A]): Jast.To[Array[A]] =
    var a = new Array[A](input.size)
    var i = 0
    while (i < input.size) {
      val ji = input(i) match
        case jx: Json => jx
        case je: JastError => return Jast.To.error("Error retriving index "+i, why = je)
      apply(ji) match
        case No(e) => return Jast.To.error("Error parsing index " + i, why = e)
        case Yes(x) => a(i) = x
      i += 1
    }
    Yes(a)

  /** Recover a collection of these objects from their JSON representation */
  def to[Coll[_]](input: Json.Arr)(using factory: collection.Factory[A, Coll[A]]): Jast.To[Coll[A]] =
    val b = factory.newBuilder
    var i = 0
    while (i < input.size) {
      val ji = input(i) match
        case jx: Json => jx
        case je: JastError => return Jast.To.error("Error retriving index "+i, why = je)
      apply(ji) match
        case No(e) => return Jast.To.error("Error parsing index " + i, why = e)
        case Yes(x) => b += x
      i += 1
    }
    Yes(b.result)
}
object FromJson {
  /** Helper method that makes an instance of a FromJson */
  def apply[A](name: String)(pf: PartialFunction[Json, Jast.To[A]]): FromJson[A] = new FromJson[A] {
    def apply(json: Json): Jast.To[A] =
      pf.applyOrElse(json, (_: Json) => Jast.To.error(s"Incorrect structure for JSON representation of $name"))
  }

  def obj[A, Z](
    name: String,
    fa: (String, Json => Jast.To[A])
  )(
    zf: (Json.Obj, A) => Ok[JastError, Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        zf(o, a)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  } 

  def obj[A, B, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B])
  )(
    zf: (Json.Obj, A, B) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        zf(o, a, b)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }


  def obj[A, B, C, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C])
  )(
    zf: (Json.Obj, A, B, C) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        zf(o, a, b, c)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }

  def obj[A, B, C, D, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D])
  )(
    zf: (Json.Obj, A, B, C, D) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        val d = o(fd._1) match
          case jj: Json => fd._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fd._1}")
        zf(o, a, b, c, d)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }

  def obj[A, B, C, D, E, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E])
  )(
    zf: (Json.Obj, A, B, C, D, E) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        val d = o(fd._1) match
          case jj: Json => fd._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fd._1}")
        val e = o(fe._1) match
          case jj: Json => fe._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fe._1}")
        zf(o, a, b, c, d, e)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }

  def obj[A, B, C, D, E, F, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F])
  )(
    zf: (Json.Obj, A, B, C, D, E, F) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        val d = o(fd._1) match
          case jj: Json => fd._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fd._1}")
        val e = o(fe._1) match
          case jj: Json => fe._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fe._1}")
        val f = o(ff._1) match
          case jj: Json => ff._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${ff._1}")
        zf(o, a, b, c, d, e, f)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }

  def obj[A, B, C, D, E, F, G, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F]),
    fg: (String, Json => Jast.To[G])
  )(
    zf: (Json.Obj, A, B, C, D, E, F, G) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        val d = o(fd._1) match
          case jj: Json => fd._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fd._1}")
        val e = o(fe._1) match
          case jj: Json => fe._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fe._1}")
        val f = o(ff._1) match
          case jj: Json => ff._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${ff._1}")
        val g = o(fg._1) match
          case jj: Json => fg._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fg._1}")
        zf(o, a, b, c, d, e, f, g)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }

  def obj[A, B, C, D, E, F, G, H, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F]),
    fg: (String, Json => Jast.To[G]),
    fh: (String, Json => Jast.To[H])
  )(
    zf: (Json.Obj, A, B, C, D, E, F, G, H) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        val d = o(fd._1) match
          case jj: Json => fd._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fd._1}")
        val e = o(fe._1) match
          case jj: Json => fe._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fe._1}")
        val f = o(ff._1) match
          case jj: Json => ff._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${ff._1}")
        val g = o(fg._1) match
          case jj: Json => fg._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fg._1}")
        val h = o(fh._1) match
          case jj: Json => fh._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fh._1}")
        zf(o, a, b, c, d, e, f, g, h)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }

  def obj[A, B, C, D, E, F, G, H, I, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F]),
    fg: (String, Json => Jast.To[G]),
    fh: (String, Json => Jast.To[H]),
    fi: (String, Json => Jast.To[I])
  )(
    zf: (Json.Obj, A, B, C, D, E, F, G, H, I) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def apply(json: Json): Jast.To[Z] = Jast.Ret{ json match
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fa._1}")
        }
        val b = o(fb._1) match
          case jj: Json => fb._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fb._1}")
        val c = o(fc._1) match
          case jj: Json => fc._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fc._1}")
        val d = o(fd._1) match
          case jj: Json => fd._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fd._1}")
        val e = o(fe._1) match
          case jj: Json => fe._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fe._1}")
        val f = o(ff._1) match
          case jj: Json => ff._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${ff._1}")
        val g = o(fg._1) match
          case jj: Json => fg._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fg._1}")
        val h = o(fh._1) match
          case jj: Json => fh._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fh._1}")
        val i = o(fi._1) match
          case jj: Json => fi._2(jj).?
          case _ => return Jast.To.error(s"$name is missing field ${fi._1}")
        zf(o, a, b, c, d, e, f, g, h, i)
      case _ => Jast.To.error(s"$name must be encoded in a JSON object")
    }
  }
}

trait JsonCompanion[A] {
  given fromJson: FromJson[A]


 /** Deserialize the object from a string containing its JSON representation. */
  final def parseToJson(input: String): Jast.To[A] = parseToJson(input, 0, input.length, JsonOptions.Default)

  /** Deserialize the object from a string containing its JSON representation. */
  final def parseToJson(input: String, options: JsonOptions): Jast.To[A] = parseToJson(input, 0, input.length, options)

  /** Deserialize the object from a substring containing its JSON representation. */
  final def parseToJson(input: String, i0: Int, iN: Int): Jast.To[A] = parseToJson(input, i0, iN, JsonOptions.Default)

  /** Deserialize the object from a substring containing its JSON representation. */
  def parseToJson(input: String, i0: Int, iN: Int, options: JsonOptions): Jast.To[A] =
    fromJson(JsonStringParser.parse(input, i0, iN, options))


  /** Deserialize the object from an Array[Byte] containing its JSON representation. */
  final def parseToJson(input: Array[Byte]): Jast.To[A] = parseToJson(input, 0, input.length, JsonOptions.Default)

  /** Deserialize the object from an Array[Byte] containing its JSON representation. */
  final def parseToJson(input: Array[Byte], options: JsonOptions): Jast.To[A] = parseToJson(input, 0, input.length, options)

  /** Deserialize the object from an Array[Byte] containing its JSON representation. */
  final def parseToJson(input: Array[Byte], i0: Int, iN: Int): Jast.To[A] = parseToJson(input, i0, iN, JsonOptions.Default)

  /** Deserialize the object from an Array[Char] containing its JSON representation. */
  def parseToJson(input: Array[Byte], i0: Int, iN: Int, options: JsonOptions): Jast.To[A] =
    fromJson(JsonByteArrayParser.parse(input, i0, iN, options))


  /** Deserialize the object from an Array[Char] containing its JSON representation. */
  final def parseToJson(input: Array[Char]): Jast.To[A] = parseToJson(input, 0, input.length, JsonOptions.Default)

  /** Deserialize the object from an Array[Char] containing its JSON representation. */
  final def parseToJson(input: Array[Char], options: JsonOptions): Jast.To[A] = parseToJson(input, 0, input.length, options)

  /** Deserialize the object from an Array[Char] containing its JSON representation. */
  final def parseToJson(input: Array[Char], i0: Int, iN: Int): Jast.To[A] = parseToJson(input, i0, iN, JsonOptions.Default)

  /** Deserialize the object from an Array[Char] containing its JSON representation. */
  def parseToJson(input: Array[Char], i0: Int, iN: Int, options: JsonOptions): Jast.To[A] =
    fromJson(JsonCharArrayParser.parse(input, i0, iN, options))


  /** Deserialize the object from a ByteBuffer containing its JSON representation. */
  final def parseToJson(input: ByteBuffer): Jast.To[A] = parseToJson(input, JsonOptions.Default)

  /** Deserialize the object from a ByteBuffer containing its JSON representation. */
  def parseToJson(input: ByteBuffer, options: JsonOptions): Jast.To[A] =
    fromJson(JsonByteBufferParser.parse(input, options))


  /** Deserialize the object from a CharBuffer containing its JSON representation. */
  final def parseToJson(input: CharBuffer): Jast.To[A] = parseToJson(input, JsonOptions.Default)

  /** Deserialize the object from a CharBuffer containing its JSON representation. */
  def parseToJson(input: CharBuffer, options: JsonOptions): Jast.To[A] =
    fromJson(JsonCharBufferParser.parse(input, options))


  /** Deserialize the object from an InputStream containing its JSON representation. */
  final def parseToJson(input: java.io.InputStream): Jast.To[A] = parseToJson(input, JsonOptions.Default)

  /** Deserialize the object from an InputStream containing its JSON representation. */
  def parseToJson(input: java.io.InputStream, options: JsonOptions): Jast.To[A] =
    fromJson((new JsonCachedByteSourceParser(8000)).setOptions(options).parse(JsonCachedByteSourceParser source input))


  /** Parses the contents of a file to a JSON AST.  The file will be closed.
    *
    * Note: if the file contains additional information beyond the end of the JSON object, it will be ignored.
    */
  final def parseToJson(filename: java.io.File): Jast.To[A] = parseToJson(filename, JsonOptions.Default)

  /** Parses the contents of a file to a JSON AST.  The file will be closed.
    *
    * Note: if the file contains additional information beyond the end of the JSON object, it will be ignored.
    */
  def parseToJson(filename: java.io.File, options: JsonOptions): Jast.To[A] =
    if (!filename.exists) Jast.To.error("File does not exist: "+filename.getPath)
    else {
      try
        val fis = new java.io.FileInputStream(filename)
        try { parseToJson(fis, options) } finally { fis.close }
      catch
        case t if NonFatal(t) => Jast.To.error("File read error: "+t.getClass.getName+" "+t.getMessage) 
    }

  /** Parses the contents of a file to a JSON AST.  The file will be closed.
    *
    * Note: if the file contains additional information beyond the end of the JSON object, it will be ignored.
    */
  final def parseToJson(pathname: java.nio.file.Path): Jast.To[A] = parseToJson(pathname, JsonOptions.Default)

  /** Parses the contents of a file to a JSON AST.  The file will be closed.
    *
    * Note: if the file contains additional information beyond the end of the JSON object, it will be ignored.
    */
  def parseToJson(pathname: java.nio.file.Path, options: JsonOptions): Jast.To[A] =
    if (!java.nio.file.Files.exists(pathname)) Jast.To.error("File does not exist: " + pathname)
    else {
      try
        val is = java.nio.file.Files.newInputStream(pathname)
        try { parseToJson(is, options) } finally { is.close }
      catch
        case t if NonFatal(t) => Jast.To.error("File read error: "+t.getClass.getName+" "+t.getMessage) 
    }
}

trait JsonParse[J <: Jast] {
  final def parse(input: String): J | JastError = parse(input, 0, input.length, JsonOptions.Default)
  final def parse(input: String, options: JsonOptions): J | JastError = parse(input, 0, input.length, options)
  final def parse(input: String, i0: Int, iN: Int): J | JastError = parse(input, i0, iN, JsonOptions.Default)
  def parse(input: String, i0: Int, iN: Int, options: JsonOptions): J | JastError

  final def parse(input: Array[Byte]): J | JastError = parse(input, 0, input.length, JsonOptions.Default)
  final def parse(input: Array[Byte], options: JsonOptions): J | JastError = parse(input, 0, input.length, options)
  final def parse(input: Array[Byte], i0: Int, iN: Int): J | JastError = parse(input, i0, iN, JsonOptions.Default)
  def parse(input: Array[Byte], i0: Int, iN: Int, options: JsonOptions): J | JastError

  final def parse(input: Array[Char]): J | JastError = parse(input, 0, input.length, JsonOptions.Default)
  final def parse(input: Array[Char], options: JsonOptions): J | JastError = parse(input, 0, input.length, options)
  final def parse(input: Array[Char], i0: Int, iN: Int): J | JastError = parse(input, i0, iN, JsonOptions.Default)
  def parse(input: Array[Char], i0: Int, iN: Int, options: JsonOptions): J | JastError

  final def parse(input: ByteBuffer): J | JastError = parse(input, JsonOptions.Default)
  def parse(input: ByteBuffer, options: JsonOptions): J | JastError

  final def parse(input: CharBuffer): J | JastError = parse(input, JsonOptions.Default)
  def parse(input: CharBuffer, options: JsonOptions): J | JastError

  final def parse(input: java.io.InputStream): J | JastError = parse(input, JsonOptions.Default)
  def parse(input: java.io.InputStream, options: JsonOptions): J | JastError

  final def parse(filename: java.io.File): J | JastError = parse(filename.toPath, JsonOptions.Default)
  final def parse(filename: java.io.File, options: JsonOptions): J | JastError = parse(filename.toPath, options)
  final def parse(pathname: java.nio.file.Path): J | JastError = parse(pathname, JsonOptions.Default)
  final def parse(pathname: java.nio.file.Path, options: JsonOptions): J | JastError =
    if (!java.nio.file.Files.exists(pathname)) JastError("File does not exist: " + pathname)
    else {
      try
        val is = java.nio.file.Files.newInputStream(pathname)
        try { parse(is, options) } finally { is.close }
      catch
        case t if NonFatal(t) => JastError("Read error: " + t.getClass.getName + " "  + t.getMessage)
    }
}
object JsonParse {
  trait Companion[J <: Jast] extends JsonParse[J] with JsonCompanion[J] {
    final override def parseToJson(input: String, i0: Int, iN: Int, options: JsonOptions): Jast.To[J] = parse(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: Array[Byte], i0: Int, iN: Int, options: JsonOptions): Jast.To[J] = parse(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: Array[Char], i0: Int, iN: Int, options: JsonOptions): Jast.To[J] = parse(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: ByteBuffer, options: JsonOptions): Jast.To[J] = parse(input, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: CharBuffer, options: JsonOptions): Jast.To[J] = parse(input, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: java.io.InputStream, options: JsonOptions): Jast.To[J] = parse(input, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: java.io.File, options: JsonOptions): Jast.To[J] = parse(input, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])

    final override def parseToJson(input: java.nio.file.Path, options: JsonOptions): Jast.To[J] = parse(input, options) match
      case je: JastError => No(je)
      case jx => Yes(jx.asInstanceOf[J])
  }
}
