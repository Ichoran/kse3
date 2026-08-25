// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.alien


import kse.basics.{given, _}
import kse.flow.{given, _}


/** Parsing and linking of proto3 schema (`.proto`) files, de novo — no protoc anywhere.
  *
  * `Proto.parse` turns one file's text into a `Proto.File` syntax tree; `Proto.link` takes
  * every file of a compilation together, builds the global symbol table, resolves type
  * references the way protoc does (innermost scope owning the first name component wins,
  * and then the whole reference must resolve there), and answers a `Proto.Schema` ready for
  * code generation.  `Proto.read` is the one-file convenience for both steps.
  *
  * Only proto3 is accepted, loudly: a missing `syntax` line means proto2 by spec, and
  * proto2, editions, `required`, `group`, `extend`, and `extensions` all get a refusal that
  * says what they are rather than a parse stumble.  Aggregate option values (message-typed
  * options in text format) are captured as opaque text, not interpreted.
  *
  * Errors are `file:line:col:` prefixed and carried by `Pb.Halt` internally; only the
  * public rims speak `Ask`.
  */
object Proto {

  /** A place in a source file, for error messages (1-based). */
  final case class Pos(line: Int, col: Int) {
    override def toString = s"$line:$col"
  }

  /** The fifteen proto3 scalar field types. */
  enum Scalar {
    case Int32, Int64, UInt32, UInt64, SInt32, SInt64, Fixed32, Fixed64, SFixed32, SFixed64, Flt, Dbl, Bool, Str, Bytes

    /** The name as written in a .proto file. */
    def protoName: String = this match
      case Int32 => "int32";     case Int64 => "int64"
      case UInt32 => "uint32";   case UInt64 => "uint64"
      case SInt32 => "sint32";   case SInt64 => "sint64"
      case Fixed32 => "fixed32"; case Fixed64 => "fixed64"
      case SFixed32 => "sfixed32"; case SFixed64 => "sfixed64"
      case Flt => "float";       case Dbl => "double"
      case Bool => "bool";       case Str => "string";      case Bytes => "bytes"
  }
  object Scalar {
    val byName: Map[String, Scalar] = Scalar.values.map(s => s.protoName -> s).toMap
  }

  /** A field's type: scalar, named-but-unresolved (parse output), resolved message or
    * enum (link output, canonical fully-qualified name without a leading dot), or map.
    */
  enum PType {
    case Prim(scalar: Scalar)
    case Named(ref: String, pos: Pos)
    case MsgT(fqn: String)
    case EnumT(fqn: String)
    case MapOf(key: Scalar, value: PType)
  }

  /** Proto3 field cardinality; `Opt` is the explicit-presence `optional` keyword. */
  enum Label { case Singular, Opt, Rep }

  /** One option, name as written, value as raw text (aggregate values kept opaque). */
  final case class Opt(name: String, value: String, pos: Pos)

  /** One field.  `oneof` is -1 outside a oneof, else an index into the message's oneofs. */
  final case class Field(name: String, number: Int, label: Label, tpe: PType, oneof: Int, options: List[Opt], pos: Pos) {
    def optioned(opt: String, dflt: Boolean): Boolean = options.find(_.name == opt) match
      case Some(o) => o.value == "true"
      case None    => dflt
  }

  final case class Oneof(name: String, pos: Pos)

  /** Reserved field numbers (inclusive ranges) and names for a message or enum. */
  final case class Reserved(ranges: List[(Int, Int)], names: List[String]) {
    def hasNumber(n: Int): Boolean = ranges.exists((a, b) => n >= a && n <= b)
    def hasName(s: String): Boolean = names.contains(s)
  }
  object Reserved { val empty = Reserved(Nil, Nil) }

  /** A message declaration.  `path` is the dotted nesting path within its file (no package);
    * `fqn` is empty until the linker fills it in.
    */
  final case class Message(
    name: String, path: String, fqn: String,
    fields: List[Field], oneofs: List[Oneof],
    nested: List[Message], enums: List[EnumDef],
    reserved: Reserved, options: List[Opt], pos: Pos
  )

  final case class EnumVal(name: String, number: Int, options: List[Opt], pos: Pos)

  /** An enum declaration; `path`/`fqn` as for `Message`. */
  final case class EnumDef(
    name: String, path: String, fqn: String,
    values: List[EnumVal], allowAlias: Boolean,
    reserved: Reserved, options: List[Opt], pos: Pos
  )

  /** An rpc; in/out are `Named` at parse time and `MsgT` once linked. */
  final case class Rpc(name: String, in: PType, inStream: Boolean, out: PType, outStream: Boolean, options: List[Opt], pos: Pos)

  final case class Service(name: String, rpcs: List[Rpc], options: List[Opt], pos: Pos)

  final case class Import(path: String, isPublic: Boolean, pos: Pos)

  /** One parsed .proto file. */
  final case class File(
    name: String, pkg: String,
    imports: List[Import], options: List[Opt],
    messages: List[Message], enums: List[EnumDef], services: List[Service]
  )

  /** What a fully-qualified name resolves to. */
  enum Sym {
    case M(msg: Message, file: File)
    case E(enm: EnumDef, file: File)
  }

  /** A linked compilation: every file, with all type references resolved, plus the global
    * symbol table keyed by canonical fully-qualified name (no leading dot).
    */
  final case class Schema(files: List[File], syms: Map[String, Sym]) {
    def message(fqn: String): Message Or Unit = syms.get(fqn) match
      case Some(Sym.M(m, _)) => Is(m)
      case _ => Alt.unit
    def enumDef(fqn: String): EnumDef Or Unit = syms.get(fqn) match
      case Some(Sym.E(e, _)) => Is(e)
      case _ => Alt.unit
  }


  /** Parse one .proto file's text.  The filename is only for error messages. */
  def parse(text: String, filename: String = "<proto>"): Ask[File] =
    try Is(new Parser(new Lexer(text, filename)).parseFile())
    catch
      case h: Pb.Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Link already-parsed files into a schema: resolve every type reference, protoc-style. */
  def link(files: Seq[File]): Ask[Schema] =
    try Is(Linker.link(files.toList))
    catch
      case h: Pb.Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Parse and link several files given as (filename, text). */
  def read(sources: Seq[(String, String)]): Ask[Schema] = Or.Ret:
    var fs = List.empty[File]
    sources.foreach((name, text) => fs = parse(text, name).? :: fs)
    link(fs.reverse).?

  /** Parse and link a single file. */
  def read(text: String, filename: String = "<proto>"): Ask[Schema] = read(List((filename, text)))


  //////////////////////////////
  /// Lexing                 ///
  //////////////////////////////

  private inline val TEof = 0
  private inline val TId = 1
  private inline val TInt = 2
  private inline val TFlt = 3
  private inline val TStr = 4
  private inline val TSym = 5

  /** One-token-lookahead lexer.  `advance()` loads the next token into the t* fields. */
  private final class Lexer(src: String, val filename: String) {
    private var i = 0
    private var line = 1
    private var lineStart = 0

    var kind: Int = TEof
    var text: String = ""     // ident text, symbol text, or unescaped string value
    var num: Long = 0L
    var tLine = 1
    var tCol = 1

    advance()

    def pos: Pos = Pos(tLine, tCol)

    def bad(msg: String): Nothing = Pb.fail(s"$filename:$tLine:$tCol: $msg")

    private def badHere(msg: String): Nothing = Pb.fail(s"$filename:$line:${i - lineStart + 1}: $msg")

    private def newline(): Unit =
      line += 1
      lineStart = i

    private def skipFluff(): Unit =
      var going = true
      while going && i < src.length do
        val c = src.charAt(i)
        if c == '\n' then { i += 1; newline() }
        else if c == ' ' || c == '\t' || c == '\r' || c == '\f' then i += 1
        else if c == '/' && i + 1 < src.length && src.charAt(i + 1) == '/' then
          i += 2
          while i < src.length && src.charAt(i) != '\n' do i += 1
        else if c == '/' && i + 1 < src.length && src.charAt(i + 1) == '*' then
          i += 2
          var open = true
          while open do
            if i >= src.length then badHere("comment never closes")
            else if src.charAt(i) == '\n' then { i += 1; newline() }
            else if src.charAt(i) == '*' && i + 1 < src.length && src.charAt(i + 1) == '/' then { i += 2; open = false }
            else i += 1
        else going = false

    private def isIdStart(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    private def isIdPart(c: Char): Boolean = isIdStart(c) || (c >= '0' && c <= '9')
    private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
    private def hexVal(c: Char): Int =
      if c >= '0' && c <= '9' then c - '0'
      else if c >= 'a' && c <= 'f' then c - 'a' + 10
      else if c >= 'A' && c <= 'F' then c - 'A' + 10
      else -1

    private def lexString(quote: Char): Unit =
      val sb = new java.lang.StringBuilder
      var open = true
      while open do
        if i >= src.length then badHere("string literal never closes")
        val c = src.charAt(i)
        if c == quote then { i += 1; open = false }
        else if c == '\n' then badHere("string literal runs past end of line")
        else if c == '\\' then
          i += 1
          if i >= src.length then badHere("string literal ends inside an escape")
          val e = src.charAt(i)
          i += 1
          e match
            case 'a' => sb.append('\u0007') __ Unit
            case 'b' => sb.append('\b') __ Unit
            case 'f' => sb.append('\f') __ Unit
            case 'n' => sb.append('\n') __ Unit
            case 'r' => sb.append('\r') __ Unit
            case 't' => sb.append('\t') __ Unit
            case 'v' => sb.append('\u000B') __ Unit
            case '\\' | '\'' | '"' | '?' => sb.append(e) __ Unit
            case 'x' | 'X' =>
              var v = 0
              var k = 0
              while k < 2 && i < src.length && hexVal(src.charAt(i)) >= 0 do
                v = v * 16 + hexVal(src.charAt(i))
                i += 1
                k += 1
              if k == 0 then badHere("\\x escape with no hex digits")
              sb.append(v.toChar) __ Unit
            case 'u' =>
              var v = 0
              var k = 0
              while k < 4 do
                if i >= src.length || hexVal(src.charAt(i)) < 0 then badHere("\\u escape needs 4 hex digits")
                v = v * 16 + hexVal(src.charAt(i))
                i += 1
                k += 1
              sb.append(v.toChar) __ Unit
            case d if d >= '0' && d <= '7' =>
              var v = d - '0'
              var k = 1
              while k < 3 && i < src.length && src.charAt(i) >= '0' && src.charAt(i) <= '7' do
                v = v * 8 + (src.charAt(i) - '0')
                i += 1
                k += 1
              sb.append(v.toChar) __ Unit
            case _ => badHere(s"unknown escape \\$e in string literal")
        else
          sb.append(c) __ Unit
          i += 1
      kind = TStr
      text = sb.toString

    private def lexNumber(): Unit =
      val start = i
      var isFlt = false
      if i < src.length && src.charAt(i) == '0' && i + 1 < src.length && (src.charAt(i + 1) == 'x' || src.charAt(i + 1) == 'X') then
        i += 2
        var v = 0L
        var any = false
        while i < src.length && hexVal(src.charAt(i)) >= 0 do
          val d = hexVal(src.charAt(i))
          if v > (Long.MaxValue - d) / 16 then badHere("hex literal too large")
          v = v * 16 + d
          i += 1
          any = true
        if !any then badHere("hex literal with no digits")
        kind = TInt
        num = v
        text = src.substring(start, i)
      else
        while i < src.length && isDigit(src.charAt(i)) do i += 1
        if i < src.length && src.charAt(i) == '.' then
          isFlt = true
          i += 1
          while i < src.length && isDigit(src.charAt(i)) do i += 1
        if i < src.length && (src.charAt(i) == 'e' || src.charAt(i) == 'E') then
          isFlt = true
          i += 1
          if i < src.length && (src.charAt(i) == '+' || src.charAt(i) == '-') then i += 1
          while i < src.length && isDigit(src.charAt(i)) do i += 1
        text = src.substring(start, i)
        if isFlt then kind = TFlt
        else if text.length > 1 && text.charAt(0) == '0' then
          var v = 0L
          var k = 1
          while k < text.length do
            val c = text.charAt(k)
            if c < '0' || c > '7' then badHere(s"bad octal literal $text")
            v = v * 8 + (c - '0')
            k += 1
          kind = TInt
          num = v
        else
          var v = 0L
          var k = 0
          while k < text.length do
            val d = text.charAt(k) - '0'
            if v > (Long.MaxValue - d) / 10 then badHere(s"integer literal $text too large")
            v = v * 10 + d
            k += 1
          kind = TInt
          num = v

    def advance(): Unit =
      skipFluff()
      tLine = line
      tCol = i - lineStart + 1
      if i >= src.length then
        kind = TEof
        text = "end of file"
      else
        val c = src.charAt(i)
        if isIdStart(c) then
          val start = i
          while i < src.length && isIdPart(src.charAt(i)) do i += 1
          kind = TId
          text = src.substring(start, i)
        else if isDigit(c) then lexNumber()
        else if c == '.' && i + 1 < src.length && isDigit(src.charAt(i + 1)) then lexNumber()
        else if c == '"' || c == '\'' then
          i += 1
          lexString(c)
        else if "{}[]()<>=;,.:-+/".indexOf(c) >= 0 then
          i += 1
          kind = TSym
          text = c.toString
        else badHere(s"unexpected character '$c'")

    /** Capture a balanced `{ ... }` aggregate as raw text (current token must be `{`). */
    def rawAggregate(): String =
      val start = i    // just past the opening brace
      var open = 1
      while open > 0 do
        if i >= src.length then badHere("aggregate option value never closes")
        val c = src.charAt(i)
        if c == '\n' then { i += 1; newline() }
        else if c == '{' then { i += 1; open += 1 }
        else if c == '}' then { open -= 1; if open > 0 then i += 1 }
        else if c == '"' || c == '\'' then
          i += 1
          val q = c
          while i < src.length && src.charAt(i) != q do
            if src.charAt(i) == '\\' then i += 1
            i += 1
          if i >= src.length then badHere("string in aggregate never closes")
          i += 1
        else i += 1
      val body = src.substring(start, i)
      i += 1   // past the closing brace
      advance()
      body.trim
  }


  //////////////////////////////
  /// Parsing                ///
  //////////////////////////////

  private final class Parser(lx: Lexer) {
    import lx.{bad, pos}

    private def isId(s: String): Boolean = lx.kind == TId && lx.text == s
    private def isSym(s: String): Boolean = lx.kind == TSym && lx.text == s

    private def describe: String = lx.kind match
      case TEof => "end of file"
      case TStr => s"string \"${lx.text}\""
      case TInt | TFlt => s"number ${lx.text}"
      case _ => s"'${lx.text}'"

    private def eatSym(s: String): Unit =
      if !isSym(s) then bad(s"expected '$s' but found $describe")
      lx.advance()

    private def eatId(s: String): Unit =
      if !isId(s) then bad(s"expected '$s' but found $describe")
      lx.advance()

    private def ident(what: String): String =
      if lx.kind != TId then bad(s"expected $what but found $describe")
      val s = lx.text
      lx.advance()
      s

    private def string(what: String): String =
      if lx.kind != TStr then bad(s"expected $what but found $describe")
      val sb = new java.lang.StringBuilder(lx.text)
      lx.advance()
      while lx.kind == TStr do   // adjacent string literals concatenate, as in C
        sb.append(lx.text) __ Unit
        lx.advance()
      sb.toString

    private def dottedIdent(what: String): String =
      val sb = new java.lang.StringBuilder(ident(what))
      while isSym(".") do
        lx.advance()
        sb.append('.').append(ident(what)) __ Unit
      sb.toString

    private def intLit(what: String, lo: Long, hi: Long): Long =
      var negative = false
      if isSym("-") then { negative = true; lx.advance() }
      if lx.kind != TInt then bad(s"expected $what but found $describe")
      val v = if negative then -lx.num else lx.num
      if v < lo || v > hi then bad(s"$what $v out of range [$lo, $hi]")
      lx.advance()
      v

    // --- options ---

    private def optionName(): String =
      val sb = new java.lang.StringBuilder
      if isSym("(") then
        lx.advance()
        sb.append('(') __ Unit
        if isSym(".") then { lx.advance(); sb.append('.') __ Unit }
        sb.append(dottedIdent("option name")) __ Unit
        eatSym(")")
        sb.append(')') __ Unit
      else sb.append(ident("option name")) __ Unit
      while isSym(".") do
        lx.advance()
        sb.append('.').append(ident("option name")) __ Unit
      sb.toString

    private def optionValue(): String =
      if lx.kind == TStr then string("option value")
      else if lx.kind == TInt || lx.kind == TFlt then { val s = lx.text; lx.advance(); s }
      else if isSym("-") || isSym("+") then
        val sign = lx.text
        lx.advance()
        if lx.kind != TInt && lx.kind != TFlt && !(lx.kind == TId && (lx.text == "inf" || lx.text == "nan")) then
          bad(s"expected a number after '$sign' but found $describe")
        val s = sign + lx.text
        lx.advance()
        s
      else if lx.kind == TId then dottedIdent("option value")
      else if isSym("{") then lx.rawAggregate()
      else bad(s"expected an option value but found $describe")

    private def optionStmt(): Opt =
      val p = pos
      eatId("option")
      val name = optionName()
      eatSym("=")
      val v = optionValue()
      eatSym(";")
      Opt(name, v, p)

    private def fieldOptions(): List[Opt] =
      if !isSym("[") then Nil
      else
        lx.advance()
        var opts = List.empty[Opt]
        var going = true
        while going do
          val p = pos
          val name = optionName()
          eatSym("=")
          opts = Opt(name, optionValue(), p) :: opts
          if isSym(",") then lx.advance() else going = false
        eatSym("]")
        opts.reverse

    // --- reserved ---

    private def reservedStmt(lo: Long, hi: Long): Reserved =
      eatId("reserved")
      if lx.kind == TStr then
        var names = List.empty[String]
        names = string("reserved name") :: names
        while isSym(",") do
          lx.advance()
          names = string("reserved name") :: names
        eatSym(";")
        Reserved(Nil, names.reverse)
      else
        var ranges = List.empty[(Int, Int)]
        var going = true
        while going do
          val a = intLit("reserved number", lo, hi).toInt
          if isId("to") then
            lx.advance()
            val b = if isId("max") then { lx.advance(); hi.toInt } else intLit("reserved range end", a, hi).toInt
            ranges = (a, b) :: ranges
          else ranges = (a, a) :: ranges
          if isSym(",") then lx.advance() else going = false
        eatSym(";")
        Reserved(ranges.reverse, Nil)

    // --- types and fields ---

    private def fieldType(): PType =
      val p = pos
      if isSym(".") then
        lx.advance()
        PType.Named("." + dottedIdent("type name"), p)
      else
        val first = ident("a type")
        Scalar.byName.get(first) match
          case Some(s) if !isSym(".") => PType.Prim(s)
          case _ =>
            val sb = new java.lang.StringBuilder(first)
            while isSym(".") do
              lx.advance()
              sb.append('.').append(ident("type name")) __ Unit
            PType.Named(sb.toString, p)

    private def fieldNumber(): Int =
      val p = pos
      val n = intLit("field number", 1, 536870911).toInt
      if n >= 19000 && n <= 19999 then Pb.fail(s"${lx.filename}:$p: field number $n is in the range 19000-19999, reserved by protobuf itself")
      n

    private def field(label: Label, oneof: Int): Field =
      val p = pos
      val t = fieldType()
      val name = ident("a field name")
      eatSym("=")
      val n = fieldNumber()
      val opts = fieldOptions()
      eatSym(";")
      Field(name, n, label, t, oneof, opts, p)

    private def mapField(): Field =
      val p = pos
      eatId("map")
      eatSym("<")
      val keyPos = pos
      val key = fieldType() match
        case PType.Prim(s) if s != Scalar.Flt && s != Scalar.Dbl && s != Scalar.Bytes => s
        case PType.Prim(s) => Pb.fail(s"${lx.filename}:$keyPos: ${s.protoName} cannot be a map key (integers, bool, and string only)")
        case _ => Pb.fail(s"${lx.filename}:$keyPos: map keys must be scalar (integers, bool, or string)")
      eatSym(",")
      val value = fieldType()
      eatSym(">")
      val name = ident("a field name")
      eatSym("=")
      val n = fieldNumber()
      val opts = fieldOptions()
      eatSym(";")
      Field(name, n, Label.Singular, PType.MapOf(key, value), -1, opts, p)

    // --- messages ---

    private def refuse(what: String, why: String): Nothing =
      bad(s"'$what' is not supported: $why")

    private def message(path: String): Message =
      val p = pos
      eatId("message")
      val name = ident("a message name")
      val myPath = if path.isEmpty then name else path + "." + name
      eatSym("{")
      var fields = List.empty[Field]
      var oneofs = List.empty[Oneof]
      var nested = List.empty[Message]
      var enums = List.empty[EnumDef]
      var opts = List.empty[Opt]
      var reserved = Reserved.empty
      var going = true
      while going do
        if isSym("}") then { lx.advance(); going = false }
        else if isSym(";") then lx.advance()
        else if lx.kind == TEof then bad(s"message $name never closes")
        else if isId("message") then nested = message(myPath) :: nested
        else if isId("enum") then enums = enumDef(myPath) :: enums
        else if isId("option") then opts = optionStmt() :: opts
        else if isId("reserved") then
          val r = reservedStmt(1, 536870911)
          reserved = Reserved(reserved.ranges ::: r.ranges, reserved.names ::: r.names)
        else if isId("oneof") then
          lx.advance()
          val op = pos
          val oname = ident("a oneof name")
          val oidx = oneofs.length
          oneofs = Oneof(oname, op) :: oneofs
          eatSym("{")
          var inOneof = true
          while inOneof do
            if isSym("}") then { lx.advance(); inOneof = false }
            else if isSym(";") then lx.advance()
            else if lx.kind == TEof then bad(s"oneof $oname never closes")
            else if isId("option") then opts = optionStmt() :: opts
            else if isId("optional") || isId("repeated") || isId("required") then bad(s"a oneof member cannot be ${lx.text}")
            else if isId("map") then bad("a oneof member cannot be a map field")
            else if isId("oneof") then bad("oneofs do not nest")
            else fields = field(Label.Singular, oidx) :: fields
        else if isId("map") then fields = mapField() :: fields
        else if isId("optional") then { lx.advance(); fields = field(Label.Opt, -1) :: fields }
        else if isId("repeated") then { lx.advance(); fields = field(Label.Rep, -1) :: fields }
        else if isId("required") then refuse("required", "it is proto2; kse3 speaks proto3 only")
        else if isId("group") then refuse("group", "groups are proto2; kse3 speaks proto3 only")
        else if isId("extensions") then refuse("extensions", "extension ranges are proto2; kse3 speaks proto3 only")
        else if isId("extend") then refuse("extend", "extending is proto2 (or custom options, which kse3 does not interpret)")
        else if lx.kind == TId || isSym(".") then fields = field(Label.Singular, -1) :: fields
        else bad(s"expected a message element but found $describe")
      val m = Message(name, myPath, "", fields.reverse, oneofs.reverse, nested.reverse, enums.reverse, reserved, opts.reverse, p)
      validateMessage(m)
      m

    private def validateMessage(m: Message): Unit =
      var seenNum = Map.empty[Int, Field]
      var seenName = Map.empty[String, Field]
      m.fields.foreach: f =>
        seenNum.get(f.number) match
          case Some(g) => Pb.fail(s"${lx.filename}:${f.pos}: field number ${f.number} of '${f.name}' already used by '${g.name}' (at ${g.pos}) in message ${m.name}")
          case None => seenNum = seenNum + (f.number -> f)
        seenName.get(f.name) match
          case Some(g) => Pb.fail(s"${lx.filename}:${f.pos}: field name '${f.name}' already used (at ${g.pos}) in message ${m.name}")
          case None => seenName = seenName + (f.name -> f)
        if m.reserved.hasNumber(f.number) then Pb.fail(s"${lx.filename}:${f.pos}: field number ${f.number} of '${f.name}' is reserved in message ${m.name}")
        if m.reserved.hasName(f.name) then Pb.fail(s"${lx.filename}:${f.pos}: field name '${f.name}' is reserved in message ${m.name}")
      var seenType = Map.empty[String, Pos]
      (m.nested.map(x => (x.name, x.pos)) ::: m.enums.map(x => (x.name, x.pos))).foreach: (name, p) =>
        seenType.get(name) match
          case Some(q) => Pb.fail(s"${lx.filename}:$p: '$name' already declared (at $q) in message ${m.name}")
          case None => seenType = seenType + (name -> p)

    private def enumDef(path: String): EnumDef =
      val p = pos
      eatId("enum")
      val name = ident("an enum name")
      val myPath = if path.isEmpty then name else path + "." + name
      eatSym("{")
      var values = List.empty[EnumVal]
      var opts = List.empty[Opt]
      var reserved = Reserved.empty
      var going = true
      while going do
        if isSym("}") then { lx.advance(); going = false }
        else if isSym(";") then lx.advance()
        else if lx.kind == TEof then bad(s"enum $name never closes")
        else if isId("option") then opts = optionStmt() :: opts
        else if isId("reserved") then
          val r = reservedStmt(Int.MinValue, Int.MaxValue)
          reserved = Reserved(reserved.ranges ::: r.ranges, reserved.names ::: r.names)
        else
          val vp = pos
          val vname = ident("an enum value name")
          eatSym("=")
          val vnum = intLit("enum value", Int.MinValue, Int.MaxValue).toInt
          val vopts = fieldOptions()
          eatSym(";")
          values = EnumVal(vname, vnum, vopts, vp) :: values
      val allowAlias = opts.exists(o => o.name == "allow_alias" && o.value == "true")
      val e = EnumDef(name, myPath, "", values.reverse, allowAlias, reserved, opts.reverse, p)
      validateEnum(e)
      e

    private def validateEnum(e: EnumDef): Unit =
      e.values match
        case Nil => Pb.fail(s"${lx.filename}:${e.pos}: enum ${e.name} has no values")
        case v :: _ =>
          if v.number != 0 then Pb.fail(s"${lx.filename}:${v.pos}: the first value of enum ${e.name} must be 0 in proto3 (found ${v.number})")
      var seenNum = Map.empty[Int, EnumVal]
      var seenName = Map.empty[String, EnumVal]
      e.values.foreach: v =>
        seenNum.get(v.number) match
          case Some(w) if !e.allowAlias =>
            Pb.fail(s"${lx.filename}:${v.pos}: enum value ${v.number} of '${v.name}' already used by '${w.name}' (at ${w.pos}); add option allow_alias = true if aliasing is meant")
          case _ => seenNum = seenNum + (v.number -> v)
        seenName.get(v.name) match
          case Some(w) => Pb.fail(s"${lx.filename}:${v.pos}: enum value name '${v.name}' already used (at ${w.pos}) in enum ${e.name}")
          case None => seenName = seenName + (v.name -> v)
        if e.reserved.hasNumber(v.number) then Pb.fail(s"${lx.filename}:${v.pos}: enum value ${v.number} of '${v.name}' is reserved in enum ${e.name}")
        if e.reserved.hasName(v.name) then Pb.fail(s"${lx.filename}:${v.pos}: enum value name '${v.name}' is reserved in enum ${e.name}")

    // --- services ---

    private def service(): Service =
      val p = pos
      eatId("service")
      val name = ident("a service name")
      eatSym("{")
      var rpcs = List.empty[Rpc]
      var opts = List.empty[Opt]
      var going = true
      while going do
        if isSym("}") then { lx.advance(); going = false }
        else if isSym(";") then lx.advance()
        else if lx.kind == TEof then bad(s"service $name never closes")
        else if isId("option") then opts = optionStmt() :: opts
        else if isId("rpc") then
          val rp = pos
          lx.advance()
          val rname = ident("an rpc name")
          eatSym("(")
          val inStream = if isId("stream") then { lx.advance(); true } else false
          val inType = fieldType()
          eatSym(")")
          eatId("returns")
          eatSym("(")
          val outStream = if isId("stream") then { lx.advance(); true } else false
          val outType = fieldType()
          eatSym(")")
          var ropts = List.empty[Opt]
          if isSym("{") then
            lx.advance()
            var inRpc = true
            while inRpc do
              if isSym("}") then { lx.advance(); inRpc = false }
              else if isSym(";") then lx.advance()
              else if lx.kind == TEof then bad(s"rpc $rname never closes")
              else if isId("option") then ropts = optionStmt() :: ropts
              else bad(s"expected an option or '}' in rpc $rname but found $describe")
          else eatSym(";")
          rpcs = Rpc(rname, inType, inStream, outType, outStream, ropts.reverse, rp) :: rpcs
        else bad(s"expected 'rpc' or an option in service $name but found $describe")
      Service(name, rpcs.reverse, opts.reverse, p)

    // --- the file ---

    def parseFile(): File =
      if isId("edition") then refuse("edition", "protobuf editions are not supported; kse3 speaks proto3 only")
      if !isId("syntax") then
        bad(s"a .proto file must open with syntax = \"proto3\"; (a missing syntax line means proto2, which kse3 does not speak) — found $describe")
      lx.advance()
      eatSym("=")
      val syn = string("the syntax name")
      if syn != "proto3" then bad(s"syntax \"$syn\" is not supported: kse3 speaks proto3 only")
      eatSym(";")

      var pkg = ""
      var imports = List.empty[Import]
      var opts = List.empty[Opt]
      var messages = List.empty[Message]
      var enums = List.empty[EnumDef]
      var services = List.empty[Service]
      var seenType = Map.empty[String, Pos]
      def noteType(name: String, p: Pos): Unit =
        seenType.get(name) match
          case Some(q) => Pb.fail(s"${lx.filename}:$p: '$name' already declared (at $q)")
          case None => seenType = seenType + (name -> p)

      while lx.kind != TEof do
        if isSym(";") then lx.advance()
        else if isId("package") then
          if pkg.nonEmpty then bad("package is declared twice")
          lx.advance()
          pkg = dottedIdent("a package name")
          eatSym(";")
        else if isId("import") then
          val p = pos
          lx.advance()
          val isPublic =
            if isId("public") then { lx.advance(); true }
            else if isId("weak") then bad("weak imports are a protoc internal; use a plain import")
            else false
          val path = string("an import path")
          eatSym(";")
          imports = Import(path, isPublic, p) :: imports
        else if isId("option") then opts = optionStmt() :: opts
        else if isId("message") then
          val m = message("")
          noteType(m.name, m.pos)
          messages = m :: messages
        else if isId("enum") then
          val e = enumDef("")
          noteType(e.name, e.pos)
          enums = e :: enums
        else if isId("service") then
          val s = service()
          noteType(s.name, s.pos)
          services = s :: services
        else if isId("extend") then refuse("extend", "extending is proto2 (or custom options, which kse3 does not interpret)")
        else bad(s"expected a top-level declaration but found $describe")

      File(lx.filename, pkg, imports.reverse, opts.reverse, messages.reverse, enums.reverse, services.reverse)
  }


  //////////////////////////////
  /// Linking                ///
  //////////////////////////////

  private object Linker {

    def link(files: List[File]): Schema =
      // Pass 1: the global symbol table (canonical fqn -> declaration) and the namespace set
      // (every dotted prefix that exists, so relative resolution can find enclosing scopes).
      var syms = Map.empty[String, (File, Message | EnumDef)]
      var spaces = Set.empty[String]

      def notePrefixes(fqn: String): Unit =
        var k = fqn.indexOf('.')
        while k >= 0 do
          spaces = spaces + fqn.substring(0, k)
          k = fqn.indexOf('.', k + 1)
        spaces = spaces + fqn

      def enroll(file: File, fqn: String, d: Message | EnumDef, p: Pos): Unit =
        syms.get(fqn) match
          case Some((f2, d2)) =>
            Pb.fail(s"${file.name}:$p: '$fqn' is already declared in ${f2.name}")
          case None =>
            syms = syms + (fqn -> (file, d))
            notePrefixes(fqn)

      def gather(file: File, prefix: String, ms: List[Message], es: List[EnumDef]): Unit =
        ms.foreach: m =>
          val fqn = if prefix.isEmpty then m.name else prefix + "." + m.name
          enroll(file, fqn, m, m.pos)
          gather(file, fqn, m.nested, m.enums)
        es.foreach: e =>
          val fqn = if prefix.isEmpty then e.name else prefix + "." + e.name
          enroll(file, fqn, e, e.pos)

      files.foreach: f =>
        if f.pkg.nonEmpty then notePrefixes(f.pkg)
        gather(f, f.pkg, f.messages, f.enums)

      // Resolution, protoc-style: find the innermost enclosing scope in which the FIRST
      // component of the reference exists (as a type or a namespace); the whole reference
      // must then resolve relative to that scope, or the reference is in error — outer
      // scopes are not consulted further.
      def resolve(file: File, scope: String, ref: String, p: Pos): String =
        if ref.startsWith(".") then
          val fqn = ref.substring(1)
          if syms.contains(fqn) then fqn
          else Pb.fail(s"${file.name}:$p: type '$ref' is not defined")
        else
          val first = ref.indexOf('.') match
            case -1 => ref
            case k  => ref.substring(0, k)
          var s = scope
          var found: String | Null = null
          var going = true
          while going do
            val cand = if s.isEmpty then first else s + "." + first
            if syms.contains(cand) || spaces.contains(cand) then
              found = s
              going = false
            else if s.isEmpty then going = false
            else s = s.lastIndexOf('.') match
              case -1 => ""
              case k  => s.substring(0, k)
          found match
            case null => Pb.fail(s"${file.name}:$p: type '$ref' is not defined (looked outward from ${if scope.isEmpty then "the file root" else scope})")
            case at: String =>
              val fqn = if at.isEmpty then ref else at + "." + ref
              if syms.contains(fqn) then fqn
              else Pb.fail(s"${file.name}:$p: '$first' names a scope at ${if at.isEmpty then "the file root" else at}, but '$fqn' is not a type")

      def resolveType(file: File, scope: String, t: PType, p: Pos): PType = t match
        case PType.Named(ref, rp) =>
          val fqn = resolve(file, scope, ref, rp)
          syms(fqn)._2 match
            case _: Message => PType.MsgT(fqn)
            case _: EnumDef => PType.EnumT(fqn)
        case PType.MapOf(k, v) => PType.MapOf(k, resolveType(file, scope, v, p))
        case other => other

      def resolveMessage(file: File, prefix: String, m: Message): Message =
        val fqn = if prefix.isEmpty then m.name else prefix + "." + m.name
        val fields = m.fields.map(f => f.copy(tpe = resolveType(file, fqn, f.tpe, f.pos)))
        val nested = m.nested.map(resolveMessage(file, fqn, _))
        val enums = m.enums.map(e => e.copy(fqn = fqn + "." + e.name))
        m.copy(fqn = fqn, fields = fields, nested = nested, enums = enums)

      def resolveService(file: File, s: Service): Service =
        val rpcs = s.rpcs.map: r =>
          def msgOnly(t: PType, what: String): PType = resolveType(file, file.pkg, t, r.pos) match
            case ok @ PType.MsgT(_) => ok
            case PType.EnumT(fqn) => Pb.fail(s"${file.name}:${r.pos}: rpc ${r.name} $what '$fqn' is an enum; rpc types must be messages")
            case _ => Pb.fail(s"${file.name}:${r.pos}: rpc ${r.name} $what must be a message type")
          r.copy(in = msgOnly(r.in, "input"), out = msgOnly(r.out, "output"))
        s.copy(rpcs = rpcs)

      val linked = files.map: f =>
        f.copy(
          messages = f.messages.map(resolveMessage(f, f.pkg, _)),
          enums = f.enums.map(e => e.copy(fqn = if f.pkg.isEmpty then e.name else f.pkg + "." + e.name)),
          services = f.services.map(resolveService(f, _))
        )

      // Pass 3: rebuild the symbol table from the resolved trees.
      var out = Map.empty[String, Sym]
      def regather(file: File, ms: List[Message], es: List[EnumDef]): Unit =
        ms.foreach: m =>
          out = out + (m.fqn -> Sym.M(m, file))
          regather(file, m.nested, m.enums)
        es.foreach: e =>
          out = out + (e.fqn -> Sym.E(e, file))
      linked.foreach(f => regather(f, f.messages, f.enums))

      Schema(linked, out)
  }
}
