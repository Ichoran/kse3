// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.alien


import kse.basics.{given, _}
import kse.flow.{given, _}


/** Generation of kse3-taste Scala bindings from a linked proto3 `Proto.Schema`.
  *
  * Each .proto file becomes one Scala source file.  The mapping is:
  *   - message -> `final case class` with proto3 defaults, nested types in the companion,
  *     `writeTo`/`toBytes` on the class and `readFrom`/`parse` (array and `Mem` forms,
  *     `Ask`-valued) on the companion.
  *   - singular scalars -> `Int`/`Long`/`UInt`/`ULong`/`Float`/`Double`/`Boolean`/`String`/
  *     `Array[Byte]`; message fields and `optional` scalars -> `T Or Unit` (`Alt.unit` when
  *     absent); repeated -> plain `Array` (reference semantics, the kse3 way).
  *   - repeated `uint32`/`uint64` and repeated enums -> `Array[Int]`/`Array[Long]` of raw
  *     bit patterns / numbers, because no `ClassTag` exists for the opaque wrappers.
  *   - proto enum -> opaque type over `Int` with named constants, `number`, and `name` —
  *     open, as proto3 demands: unknown numbers pass through untouched.
  *   - oneof -> a Scala 3 `enum` in the companion, one case per arm plus `Unset`; an arm
  *     whose name would collide with a type it mentions gets an `Arm` suffix.
  *   - map<K, V> -> immutable `Map[K, V]`.
  *
  * Unknown fields are RETAINED by default: each message carries an `unknown` list of
  * `Pb.Unknown` (encounter order, verbatim bytes), re-emitted after the known fields, so a
  * read-modify-write pass preserves what a newer schema added.  Set
  * `Config(retainUnknown = false)` for lean drop-on-read bindings (which is also the
  * behavior a future fixed-layout `Mem.Struct`/`Mem.AoS` target will have by nature).
  * Writers emit in ascending field-number order.  Services generate nothing yet — the
  * schema keeps their descriptions for a future transport layer.
  */
object PbGen {

  /** Knobs for generation: where generated code lands (proto package -> Scala package,
    * identity by default), an extra header line if wanted, and whether messages carry and
    * re-emit unknown fields (they do unless told otherwise).
    */
  final case class Config(pkgOf: String => String = p => p, header: String = "", retainUnknown: Boolean = true)

  /** Generate one Scala source per file in the schema, as (suggested filename, content). */
  def generate(schema: Proto.Schema, config: Config = Config()): Ask[List[(String, String)]] =
    try Is(schema.files.map(f => new Gen(schema, config, f).result))
    catch
      case h: Pb.Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Parse, link, and generate in one go, for (filename, text) sources. */
  def generate(sources: Seq[(String, String)]): Ask[List[(String, String)]] = Or.Ret:
    generate(Proto.read(sources).?).?

  /** Parse, link, and generate a single .proto text. */
  def generate(text: String, filename: String): Ask[List[(String, String)]] = generate(List((filename, text)))


  private val keywords = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends",
    "false", "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match",
    "new", "null", "object", "override", "package", "private", "protected", "return", "sealed",
    "super", "then", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield"
  )

  private def sident(s: String): String = if keywords(s) then "`" + s + "`" else s

  private def camel(s: String): String =
    val sb = new java.lang.StringBuilder
    var up = false
    var k = 0
    while k < s.length do
      val c = s.charAt(k)
      if c == '_' then up = true
      else
        sb.append(if up && sb.length > 0 then c.toUpper else c) __ Unit
        up = false
      k += 1
    sb.toString

  private def pascal(s: String): String =
    val c = camel(s)
    if c.isEmpty then c else c.charAt(0).toUpper.toString + c.substring(1)

  /** track_data.proto -> TrackDataProto.scala */
  private def scalaFileName(protoName: String): String =
    val base0 = protoName.lastIndexOf('/') match
      case -1 => protoName
      case k  => protoName.substring(k + 1)
    val base = if base0.endsWith(".proto") then base0.dropRight(6) else base0
    pascal(base.replace('.', '_').replace('-', '_')) + "Proto.scala"


  private final class Gen(schema: Proto.Schema, config: Config, file: Proto.File) {
    private val sb = new java.lang.StringBuilder
    private val pkg = file.pkg
    private val scalaPkg = config.pkgOf(pkg)

    private def line(indent: Int, s: String): Unit =
      var k = 0
      while k < indent do { sb.append("  ") __ Unit; k += 1 }
      sb.append(s).append('\n') __ Unit

    private def blank(): Unit = sb.append('\n') __ Unit

    // --- names and references ---

    /** The Scala reference for a resolved type: its path within this package if it lives
      * here, its full package-qualified path otherwise.
      */
    private def refOf(fqn: String): String =
      val (filePkg, path) = schema.syms.get(fqn) match
        case Some(Proto.Sym.M(m, f)) => (f.pkg, m.path)
        case Some(Proto.Sym.E(e, f)) => (f.pkg, e.path)
        case None => Pb.fail(s"internal: unresolved '$fqn' survived linking")
      if filePkg == pkg then path
      else if filePkg.isEmpty then path
      else config.pkgOf(filePkg) + "." + path

    private def enumZero(fqn: String): String =
      schema.enumDef(fqn).fold(e => refOf(fqn) + "." + sident(e.values.find(_.number == 0).map(_.name).getOrElse("apply")))(
        _ => Pb.fail(s"internal: '$fqn' is not an enum"))

    // --- per-scalar generation table ---

    private def scalarType(s: Proto.Scalar): String = s match
      case Proto.Scalar.Int32 | Proto.Scalar.SInt32 | Proto.Scalar.SFixed32 => "Int"
      case Proto.Scalar.Int64 | Proto.Scalar.SInt64 | Proto.Scalar.SFixed64 => "Long"
      case Proto.Scalar.UInt32 | Proto.Scalar.Fixed32 => "UInt"
      case Proto.Scalar.UInt64 | Proto.Scalar.Fixed64 => "ULong"
      case Proto.Scalar.Flt => "Float"
      case Proto.Scalar.Dbl => "Double"
      case Proto.Scalar.Bool => "Boolean"
      case Proto.Scalar.Str => "String"
      case Proto.Scalar.Bytes => "Array[Byte]"

    private def scalarDefault(s: Proto.Scalar): String = s match
      case Proto.Scalar.Int32 | Proto.Scalar.SInt32 | Proto.Scalar.SFixed32 => "0"
      case Proto.Scalar.Int64 | Proto.Scalar.SInt64 | Proto.Scalar.SFixed64 => "0L"
      case Proto.Scalar.UInt32 | Proto.Scalar.Fixed32 => "UInt(0)"
      case Proto.Scalar.UInt64 | Proto.Scalar.Fixed64 => "ULong(0L)"
      case Proto.Scalar.Flt => "0.0f"
      case Proto.Scalar.Dbl => "0.0"
      case Proto.Scalar.Bool => "false"
      case Proto.Scalar.Str => "\"\""
      case Proto.Scalar.Bytes => "Array.empty[Byte]"

    /** The wire verb shared by Out emitters and In getters: o.<verb>(n, v) / in.<verb>(). */
    private def verbOf(s: Proto.Scalar): String = s match
      case Proto.Scalar.Int32 => "int32";       case Proto.Scalar.Int64 => "int64"
      case Proto.Scalar.UInt32 => "uint32";     case Proto.Scalar.UInt64 => "uint64"
      case Proto.Scalar.SInt32 => "sint32";     case Proto.Scalar.SInt64 => "sint64"
      case Proto.Scalar.Fixed32 => "fixed32";   case Proto.Scalar.Fixed64 => "fixed64"
      case Proto.Scalar.SFixed32 => "sfixed32"; case Proto.Scalar.SFixed64 => "sfixed64"
      case Proto.Scalar.Flt => "float";         case Proto.Scalar.Dbl => "double"
      case Proto.Scalar.Bool => "bool";         case Proto.Scalar.Str => "string"
      case Proto.Scalar.Bytes => "bytes"

    /** Element type for repeated scalars (bit patterns for the unsigned pair). */
    private def repElemType(s: Proto.Scalar): String = s match
      case Proto.Scalar.UInt32 | Proto.Scalar.Fixed32 => "Int"
      case Proto.Scalar.UInt64 | Proto.Scalar.Fixed64 => "Long"
      case other => scalarType(other)

    private def accOf(s: Proto.Scalar): String = s match
      case Proto.Scalar.Int32 | Proto.Scalar.SInt32 | Proto.Scalar.SFixed32
         | Proto.Scalar.UInt32 | Proto.Scalar.Fixed32 => "Pb.IntAcc"
      case Proto.Scalar.Int64 | Proto.Scalar.SInt64 | Proto.Scalar.SFixed64
         | Proto.Scalar.UInt64 | Proto.Scalar.Fixed64 => "Pb.LongAcc"
      case Proto.Scalar.Flt => "Pb.FloatAcc"
      case Proto.Scalar.Dbl => "Pb.DoubleAcc"
      case Proto.Scalar.Bool => "Pb.BoolAcc"
      case Proto.Scalar.Str => "Pb.RefAcc[String]"
      case Proto.Scalar.Bytes => "Pb.RefAcc[Array[Byte]]"

    /** in.<accVerb>(acc) for packed-or-not repeated scalar reads. */
    private def accVerbOf(s: Proto.Scalar): String = s match
      case Proto.Scalar.Int32 => "int32s";       case Proto.Scalar.Int64 => "int64s"
      case Proto.Scalar.UInt32 => "uint32s";     case Proto.Scalar.UInt64 => "uint64s"
      case Proto.Scalar.SInt32 => "sint32s";     case Proto.Scalar.SInt64 => "sint64s"
      case Proto.Scalar.Fixed32 => "fixed32s";   case Proto.Scalar.Fixed64 => "fixed64s"
      case Proto.Scalar.SFixed32 => "fixed32s";  case Proto.Scalar.SFixed64 => "fixed64s"
      case Proto.Scalar.Flt => "floats";         case Proto.Scalar.Dbl => "doubles"
      case Proto.Scalar.Bool => "bools"
      case Proto.Scalar.Str | Proto.Scalar.Bytes => Pb.fail("internal: strings and bytes have no packed form")

    private def packedVerbOf(s: Proto.Scalar): String = s match
      case Proto.Scalar.Int32 => "packedInt32";     case Proto.Scalar.Int64 => "packedInt64"
      case Proto.Scalar.UInt32 => "packedUInt32";   case Proto.Scalar.UInt64 => "packedUInt64"
      case Proto.Scalar.SInt32 => "packedSInt32";   case Proto.Scalar.SInt64 => "packedSInt64"
      case Proto.Scalar.Fixed32 | Proto.Scalar.SFixed32 => "packedFixed32"
      case Proto.Scalar.Fixed64 | Proto.Scalar.SFixed64 => "packedFixed64"
      case Proto.Scalar.Flt => "packedFloat";       case Proto.Scalar.Dbl => "packedDouble"
      case Proto.Scalar.Bool => "packedBool"
      case Proto.Scalar.Str | Proto.Scalar.Bytes => Pb.fail("internal: strings and bytes have no packed form")

    /** Emit expression for one element of an unpacked repeated scalar, from its array-element form. */
    private def repElemEmit(s: Proto.Scalar, n: Int, v: String): String = s match
      case Proto.Scalar.UInt32 | Proto.Scalar.Fixed32 => s"o.${verbOf(s)}Always($n, UInt($v))"
      case Proto.Scalar.UInt64 | Proto.Scalar.Fixed64 => s"o.${verbOf(s)}Always($n, ULong($v))"
      case other => s"o.${verbOf(other)}Always($n, $v)"

    private def packable(s: Proto.Scalar): Boolean = s != Proto.Scalar.Str && s != Proto.Scalar.Bytes

    // --- field-level generation ---

    private def fieldScalaName(f: Proto.Field): String = sident(camel(f.name))

    private def typeOf(f: Proto.Field): String = f.tpe match
      case Proto.PType.Prim(s) =>
        if f.label == Proto.Label.Rep then s"Array[${repElemType(s)}]"
        else if f.label == Proto.Label.Opt then s"${scalarType(s)} Or Unit"
        else scalarType(s)
      case Proto.PType.MsgT(fqn) =>
        if f.label == Proto.Label.Rep then s"Array[${refOf(fqn)}]" else s"${refOf(fqn)} Or Unit"
      case Proto.PType.EnumT(fqn) =>
        if f.label == Proto.Label.Rep then "Array[Int]"
        else if f.label == Proto.Label.Opt then s"${refOf(fqn)} Or Unit"
        else refOf(fqn)
      case Proto.PType.MapOf(k, v) =>
        val vt = v match
          case Proto.PType.Prim(s)   => scalarType(s)
          case Proto.PType.MsgT(fqn) => refOf(fqn)
          case Proto.PType.EnumT(fqn) => refOf(fqn)
          case _ => Pb.fail("internal: map value cannot be a map")
        s"Map[${scalarType(k)}, $vt]"
      case Proto.PType.Named(ref, _) => Pb.fail(s"internal: unresolved reference '$ref' survived linking")

    private def defaultOf(f: Proto.Field): String = f.tpe match
      case Proto.PType.Prim(s) =>
        if f.label == Proto.Label.Rep then s"Array.empty[${repElemType(s)}]"
        else if f.label == Proto.Label.Opt then "Alt.unit"
        else scalarDefault(s)
      case Proto.PType.MsgT(fqn) =>
        if f.label == Proto.Label.Rep then s"Array.empty[${refOf(fqn)}]" else "Alt.unit"
      case Proto.PType.EnumT(fqn) =>
        if f.label == Proto.Label.Rep then "Array.empty[Int]"
        else if f.label == Proto.Label.Opt then "Alt.unit"
        else enumZero(fqn)
      case Proto.PType.MapOf(_, _) => "Map.empty"
      case Proto.PType.Named(ref, _) => Pb.fail(s"internal: unresolved reference '$ref' survived linking")

    /** The name of the retained-unknowns field, dodging any proto field that claims it. */
    private def unknownNameOf(m: Proto.Message): String =
      val taken = m.fields.map(f => camel(f.name)).toSet ++ m.oneofs.map(o => camel(o.name))
      if !taken("unknown") then "unknown"
      else if !taken("unknownFields") then "unknownFields"
      else Pb.fail(s"${file.name}:${m.pos}: message ${m.name} claims both 'unknown' and 'unknownFields'; rename one or generate with retainUnknown = false")

    // --- oneof case naming, with the shadow-dodging Arm suffix ---

    private def oneofTypeName(o: Proto.Oneof): String = pascal(o.name)

    private def oneofCases(m: Proto.Message, oidx: Int): List[(Proto.Field, String)] =
      val members = m.fields.filter(_.oneof == oidx)
      val shadows = members.flatMap(f => f.tpe match
        case Proto.PType.MsgT(fqn)  => List(refOf(fqn).takeWhile(_ != '.'))
        case Proto.PType.EnumT(fqn) => List(refOf(fqn).takeWhile(_ != '.'))
        case _ => Nil
      ).toSet + oneofTypeName(m.oneofs(oidx)) + "Unset"
      var taken = Set.empty[String]
      members.map: f =>
        val base = pascal(f.name)
        val name = if shadows(base) then base + "Arm" else base
        if taken(name) then Pb.fail(s"${file.name}:${f.pos}: oneof case name '$name' collides with another arm; rename one of the fields")
        taken = taken + name
        (f, name)

    // --- writer ---

    private def writeLines(m: Proto.Message, myRef: String, indent: Int): Unit =
      var oneofEmitted = Set.empty[Int]
      // plain fields in ascending number order, each oneof at its first member's slot
      val slots: List[Proto.Field Or Int] =
        m.fields.sortBy(_.number).flatMap: f =>
          if f.oneof < 0 then List(Is(f))
          else if oneofEmitted(f.oneof) then Nil
          else
            oneofEmitted = oneofEmitted + f.oneof
            List(Alt(f.oneof))
      slots.foreach(s => s.fold(f => writeField(f, indent))(oidx => writeOneof(m, myRef, oidx, indent)))
      if config.retainUnknown then line(indent, s"o.unknowns(${unknownNameOf(m)})")

    private def writeField(f: Proto.Field, indent: Int): Unit =
      val x = fieldScalaName(f)
      val n = f.number
      f.tpe match
        case Proto.PType.Prim(s) => f.label match
          case Proto.Label.Singular => line(indent, s"o.${verbOf(s)}($n, $x)")
          case Proto.Label.Opt      => line(indent, s"$x.fold(v => o.${verbOf(s)}Always($n, v))(_ => ())")
          case Proto.Label.Rep =>
            if packable(s) && f.optioned("packed", true) then line(indent, s"o.${packedVerbOf(s)}($n, $x)")
            else line(indent, s"$x.use()(v => ${repElemEmit(s, n, "v")})")
        case Proto.PType.MsgT(_) => f.label match
          case Proto.Label.Rep =>
            line(indent, s"$x.use(): v =>")
            line(indent + 1, s"val b = Pb.Out()")
            line(indent + 1, s"v.writeTo(b)")
            line(indent + 1, s"o.msg($n, b)")
          case _ =>
            line(indent, s"$x.fold{ v => val b = Pb.Out(); v.writeTo(b); o.msg($n, b) }(_ => ())")
        case Proto.PType.EnumT(_) => f.label match
          case Proto.Label.Singular => line(indent, s"o.int32($n, $x.number)")
          case Proto.Label.Opt      => line(indent, s"$x.fold(v => o.int32Always($n, v.number))(_ => ())")
          case Proto.Label.Rep =>
            if f.optioned("packed", true) then line(indent, s"o.packedInt32($n, $x)")
            else line(indent, s"$x.use()(v => o.int32Always($n, v))")
        case Proto.PType.MapOf(k, v) =>
          line(indent, s"$x.foreach: (mk, mv) =>")
          line(indent + 1, s"val b = Pb.Out()")
          line(indent + 1, s"b.${verbOf(k)}(1, mk)")
          v match
            case Proto.PType.Prim(s)   => line(indent + 1, s"b.${verbOf(s)}(2, mv)")
            case Proto.PType.EnumT(_)  => line(indent + 1, s"b.int32(2, mv.number)")
            case Proto.PType.MsgT(_)   =>
              line(indent + 1, s"val c = Pb.Out()")
              line(indent + 1, s"mv.writeTo(c)")
              line(indent + 1, s"b.msg(2, c)")
            case _ => Pb.fail("internal: map value cannot be a map")
          line(indent + 1, s"o.msg($n, b)")
        case Proto.PType.Named(ref, _) => Pb.fail(s"internal: unresolved reference '$ref' survived linking")

    private def writeOneof(m: Proto.Message, myRef: String, oidx: Int, indent: Int): Unit =
      val ot = s"$myRef.${oneofTypeName(m.oneofs(oidx))}"
      line(indent, s"${sident(camel(m.oneofs(oidx).name))} match")
      oneofCases(m, oidx).foreach: (f, cname) =>
        val n = f.number
        f.tpe match
          case Proto.PType.Prim(s) =>
            line(indent + 1, s"case $ot.$cname(v) => o.${verbOf(s)}Always($n, v)")
          case Proto.PType.EnumT(_) =>
            line(indent + 1, s"case $ot.$cname(v) => o.int32Always($n, v.number)")
          case Proto.PType.MsgT(_) =>
            line(indent + 1, s"case $ot.$cname(v) => val b = Pb.Out(); v.writeTo(b); o.msg($n, b)")
          case _ => Pb.fail("internal: oneof member can only be scalar, enum, or message")
      line(indent + 1, s"case $ot.Unset => ()")

    // --- reader ---

    private def readerLines(m: Proto.Message, myRef: String, indent: Int): Unit =
      // accumulators and working state, one per field slot
      var oneofDeclared = Set.empty[Int]
      m.fields.foreach: f =>
        val x = fieldScalaName(f)
        if f.oneof >= 0 then
          if !oneofDeclared(f.oneof) then
            oneofDeclared = oneofDeclared + f.oneof
            val ov = sident(camel(m.oneofs(f.oneof).name))
            val ot = s"$myRef.${oneofTypeName(m.oneofs(f.oneof))}"
            line(indent, s"var $ov: $ot = $ot.Unset")
        else f.tpe match
          case Proto.PType.Prim(s) if f.label == Proto.Label.Rep =>
            line(indent, s"val $x = new ${accOf(s)}")
          case Proto.PType.MsgT(fqn) if f.label == Proto.Label.Rep =>
            line(indent, s"val $x = new Pb.RefAcc[${refOf(fqn)}]")
          case Proto.PType.EnumT(_) if f.label == Proto.Label.Rep =>
            line(indent, s"val $x = new Pb.IntAcc")
          case _ =>
            line(indent, s"var $x: ${typeOf(f)} = ${defaultOf(f)}")
      if config.retainUnknown then line(indent, s"var ${unknownNameOf(m)} = List.empty[Pb.Unknown]")
      line(indent, "while in.next() do in.field match")
      m.fields.sortBy(_.number).foreach(f => readCase(m, myRef, f, indent + 1))
      if config.retainUnknown then line(indent + 1, s"case _ => ${unknownNameOf(m)} = in.keep() :: ${unknownNameOf(m)}")
      else line(indent + 1, "case _ => in.skip()")
      val args = ctorArgs(m).map(_._2).mkString(", ")
      line(indent, s"$myRef($args)")

    private def readCase(m: Proto.Message, myRef: String, f: Proto.Field, indent: Int): Unit =
      val n = f.number
      if f.oneof >= 0 then
        val ov = sident(camel(m.oneofs(f.oneof).name))
        val ot = s"$myRef.${oneofTypeName(m.oneofs(f.oneof))}"
        val cname = oneofCases(m, f.oneof).find(_._1.number == n).map(_._2).getOrElse(Pb.fail("internal: oneof case vanished"))
        f.tpe match
          case Proto.PType.Prim(s)    => line(indent, s"case $n => $ov = $ot.$cname(in.${verbOf(s)}())")
          case Proto.PType.EnumT(fqn) => line(indent, s"case $n => $ov = $ot.$cname(${refOf(fqn)}(in.int32()))")
          case Proto.PType.MsgT(fqn)  => line(indent, s"case $n => $ov = $ot.$cname(${refOf(fqn)}.readFrom(in.sub()))")
          case _ => Pb.fail("internal: oneof member can only be scalar, enum, or message")
      else
        val x = fieldScalaName(f)
        f.tpe match
          case Proto.PType.Prim(s) => f.label match
            case Proto.Label.Singular => line(indent, s"case $n => $x = in.${verbOf(s)}()")
            case Proto.Label.Opt      => line(indent, s"case $n => $x = Is(in.${verbOf(s)}())")
            case Proto.Label.Rep =>
              if packable(s) then line(indent, s"case $n => in.${accVerbOf(s)}($x)")
              else line(indent, s"case $n => $x += in.${verbOf(s)}()")
          case Proto.PType.MsgT(fqn) => f.label match
            case Proto.Label.Rep => line(indent, s"case $n => $x += ${refOf(fqn)}.readFrom(in.sub())")
            case _               => line(indent, s"case $n => $x = Is(${refOf(fqn)}.readFrom(in.sub()))")
          case Proto.PType.EnumT(fqn) => f.label match
            case Proto.Label.Singular => line(indent, s"case $n => $x = ${refOf(fqn)}(in.int32())")
            case Proto.Label.Opt      => line(indent, s"case $n => $x = Is(${refOf(fqn)}(in.int32()))")
            case Proto.Label.Rep      => line(indent, s"case $n => in.int32s($x)")
          case Proto.PType.MapOf(k, v) =>
            line(indent, s"case $n =>")
            line(indent + 1, "val e = in.sub()")
            line(indent + 1, s"var mk: ${scalarType(k)} = ${scalarDefault(k)}")
            v match
              case Proto.PType.Prim(s)    => line(indent + 1, s"var mv: ${scalarType(s)} = ${scalarDefault(s)}")
              case Proto.PType.EnumT(fqn) => line(indent + 1, s"var mv: ${refOf(fqn)} = ${enumZero(fqn)}")
              case Proto.PType.MsgT(fqn)  => line(indent + 1, s"var mv: ${refOf(fqn)} = ${refOf(fqn)}()")
              case _ => Pb.fail("internal: map value cannot be a map")
            line(indent + 1, "while e.next() do e.field match")
            line(indent + 2, s"case 1 => mk = e.${verbOf(k)}()")
            v match
              case Proto.PType.Prim(s)    => line(indent + 2, s"case 2 => mv = e.${verbOf(s)}()")
              case Proto.PType.EnumT(fqn) => line(indent + 2, s"case 2 => mv = ${refOf(fqn)}(e.int32())")
              case Proto.PType.MsgT(fqn)  => line(indent + 2, s"case 2 => mv = ${refOf(fqn)}.readFrom(e.sub())")
              case _ => ()
            line(indent + 2, "case _ => e.skip()")
            line(indent + 1, s"$x = $x + (mk -> mv)")
          case Proto.PType.Named(ref, _) => Pb.fail(s"internal: unresolved reference '$ref' survived linking")

    // --- declarations ---

    /** The case-class parameters, in declaration order, oneofs at their first member. */
    private def ctorArgs(m: Proto.Message): List[(String, String)] =
      val base = fieldArgs(m)
      if config.retainUnknown then
        val un = unknownNameOf(m)
        base :+ ((s"$un: List[Pb.Unknown] = Nil", s"$un.reverse"))
      else base

    private def fieldArgs(m: Proto.Message): List[(String, String)] =
      var oneofSeen = Set.empty[Int]
      m.fields.flatMap: f =>
        if f.oneof >= 0 then
          if oneofSeen(f.oneof) then Nil
          else
            oneofSeen = oneofSeen + f.oneof
            val ov = sident(camel(m.oneofs(f.oneof).name))
            val ot = s"${m.path}.${oneofTypeName(m.oneofs(f.oneof))}"
            List((s"$ov: $ot = $ot.Unset", ov))
        else
          val x = fieldScalaName(f)
          // readFrom builds repeated fields in accumulators, so the argument differs
          val arg = f.tpe match
            case _ if f.label != Proto.Label.Rep => x
            case Proto.PType.MapOf(_, _) => x
            case _ => x + ".result"
          List((s"$x: ${typeOf(f)} = ${defaultOf(f)}", arg))

    private def genMessage(m: Proto.Message, indent: Int): Unit =
      val myRef = m.path   // path from file root: always valid inside this file's package
      val name = sident(m.name)
      val args = ctorArgs(m)
      if args.isEmpty then line(indent, s"final case class $name() {")
      else
        line(indent, s"final case class $name(")
        args.foreach((decl, _) => line(indent + 1, decl + (if decl == args.last._1 then "" else ",")))
        line(indent, ") {")
      line(indent + 1, "def writeTo(o: Pb.Out): Unit =")
      if m.fields.isEmpty && !config.retainUnknown then line(indent + 2, "()")
      else writeLines(m, myRef, indent + 2)
      line(indent + 1, "def toBytes: Array[Byte] =")
      line(indent + 2, "val o = Pb.Out()")
      line(indent + 2, "writeTo(o)")
      line(indent + 2, "o.result")
      line(indent, "}")
      blank()
      line(indent, s"object $name {")
      m.oneofs.zipWithIndex.foreach: (o, oidx) =>
        line(indent + 1, s"enum ${oneofTypeName(o)} {")
        oneofCases(m, oidx).foreach: (f, cname) =>
          val vt = f.tpe match
            case Proto.PType.Prim(s)    => scalarType(s)
            case Proto.PType.MsgT(fqn)  => refOf(fqn)
            case Proto.PType.EnumT(fqn) => refOf(fqn)
            case _ => Pb.fail("internal: oneof member can only be scalar, enum, or message")
          line(indent + 2, s"case $cname(value: $vt)")
        line(indent + 2, "case Unset")
        line(indent + 1, "}")
        blank()
      m.enums.foreach(e => genEnum(e, indent + 1))
      m.nested.foreach(genMessage(_, indent + 1))
      line(indent + 1, s"def readFrom(in: Pb.In): $myRef = Pb.context(\"${m.name}\"):")
      readerLines(m, myRef, indent + 2)
      line(indent + 1, s"def parse(bs: Array[Byte]): Ask[$myRef] = Pb.decode(bs)(readFrom)")
      line(indent + 1, s"def parse(bs: Array[Byte], i0: Int, iN: Int): Ask[$myRef] = Pb.decode(bs, i0, iN)(readFrom)")
      line(indent + 1, s"def parse(m: Mem[Byte]): Ask[$myRef] = Pb.decode(m)(readFrom)")
      line(indent + 1, s"def parse(m: Mem[Byte], i0: Long, iN: Long): Ask[$myRef] = Pb.decode(m, i0, iN)(readFrom)")
      line(indent, "}")
      blank()

    private def genEnum(e: Proto.EnumDef, indent: Int): Unit =
      val name = sident(e.name)
      line(indent, s"opaque type $name = Int")
      line(indent, s"object $name {")
      line(indent + 1, s"inline def apply(n: Int): $name = n")
      e.values.foreach(v => line(indent + 1, s"val ${sident(v.name)}: $name = ${v.number}"))
      line(indent + 1, s"extension (e: $name) {")
      line(indent + 2, "inline def number: Int = e")
      line(indent + 2, "def name: String = e match")
      var seen = Set.empty[Int]
      e.values.foreach: v =>
        if !seen(v.number) then
          seen = seen + v.number
          line(indent + 3, s"case ${v.number} => \"${v.name}\"")
      line(indent + 3, s"case n => s\"<unknown ${e.name} $$n>\"")
      line(indent + 1, "}")
      line(indent, "}")
      blank()

    def result: (String, String) =
      line(0, s"// Generated by kse.alien.PbGen from ${file.name} -- DO NOT EDIT.")
      if config.header.nonEmpty then line(0, config.header)
      blank()
      if scalaPkg.nonEmpty then
        line(0, s"package ${scalaPkg.split('.').map(sident).mkString(".")}")
        blank()
      blank()
      line(0, "import kse.basics.{given, _}")
      line(0, "import kse.flow.{given, _}")
      line(0, "import kse.maths.{UInt, ULong}")
      line(0, "import kse.alien.Pb")
      blank()
      blank()
      file.enums.foreach(e => genEnum(e, 0))
      file.messages.foreach(m => genMessage(m, 0))
      (scalaFileName(file.name), sb.toString)
  }
}
