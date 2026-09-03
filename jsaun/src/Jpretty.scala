// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import kse.basics.{given, _}


/** Fit-aware pretty printing (`Jstyle` with a positive `width`).  The tree is rendered once,
  * flat but with pretty spacing, into a scratch buffer of the target's own kind, recording
  * where every element, key, and value landed (the `Jfmt` span idiom: arrays one slot per
  * element, objects two per entry).  A reflow pass then walks the span tree: a node that fits
  * on the rest of the line is one bulk copy from the scratch; one that does not breaks, each
  * child on its own line -- except all-scalar arrays too long for one line, which wrap into
  * aligned columns.  Numbers are rendered exactly once, in the flat pass.
  *
  * Compact and preserved-format output never come here; `printTo` calls in only from its
  * style-fresh branch.  Children that do carry preserved formatting are emitted into the
  * scratch verbatim by their own `printTo` and treated as opaque tokens: if such a token is
  * multi-line it never fits (its slot is flagged via the span's sign bit) and it is copied
  * through unchanged, keeping its original interior layout.
  *
  * Widths are measured in the target's units -- chars for `Jout.Str`, UTF-8 bytes for
  * `Jout.Bytes` -- so byte output breaks (and pads columns) slightly conservatively for
  * non-ASCII content.
  */
private[jsaun] object Jpretty {

  // Span packing as in Jfmt, plus the (always free) sign bit to flag a span containing a
  // newline, so fit checks need never rescan the scratch.
  private inline def spanOf(a: Int, b: Int, multi: Boolean): Long =
    val s = (a.toLong << 32) | (b.toLong & 0xFFFFFFFFL)
    if multi then s | Long.MinValue else s
  private inline def multi(span: Long): Boolean = span < 0
  private inline def i0(span: Long): Int = ((span >>> 32) & 0x7FFFFFFFL).toInt
  private inline def iN(span: Long): Int = (span & 0xFFFFFFFFL).toInt
  private inline def len(span: Long): Int = iN(span) - i0(span)

  /** The span tree from the flat pass.  `kids(k)` is non-null only where child `k` was
    * itself reflowable; scalars and opaque preserved children live only in `slots`.
    */
  private final class Plan(val obj: Boolean, val nums: Boolean, val slots: Array[Long], val kids: Array[Plan] | Null, val self: Long) {}

  private def scratchFor(out: Jout): Jout =
    val sc: Jout = out match
      case _: Jout.Bytes => new Jout.Bytes(style = out.style)
      case _ => new Jout.Str(style = out.style)   // Str scratch serves any non-Bytes target via add(String)
    sc.ignoreFmt = out.ignoreFmt
    sc.depth = out.depth
    sc

  private inline def fresh(fmt: Jfmt | Jfmt.Local | Null, sc: Jout): Boolean =
    sc.ignoreFmt || (fmt eq null)

  /** Reflow `j` (a collection already known to be style-fresh and nonempty) onto `out`. */
  def printTo(j: Json, out: Jout): Unit =
    val sc = scratchFor(out)
    flat(j, sc) match
      case p: Plan => reflow(p, out, sc, out.column, out.depth, out.style)
      case null => sc.copyTo(out, 0, sc.pos)   // cannot happen (j is fresh), but verbatim is safe

  /** Write `j` flat (single line, spaced per style) into `sc`, answering its `Plan` if it was
    * reflowable and null if it went out as an opaque token (scalar or preserved formatting).
    */
  private def flat(j: Json, sc: Jout): Plan | Null =
    val st = sc.style
    val sep = if st.spaceAfterComma then ", " else ","
    j match
      case a: Jarr.A if fresh(a.fmt, sc) =>
        val start = sc.pos
        val slots = new Array[Long](a.n)
        var kids: Array[Plan] | Null = null
        var nums = true
        var ml = false
        sc.add('[')
        var k = 0
        while k < a.n do
          if k > 0 then sc.add(sep)
          val e0 = sc.pos
          val v = a.vs(k)
          val p = flat(v, sc)
          val m = p match
            case q: Plan =>
              val ks = kids match
                case null =>
                  val ks2 = new Array[Plan](a.n)
                  kids = ks2
                  ks2
                case ks2 => ks2
              ks(k) = q
              multi(q.self)
            case null => v match
              case _: Jarr | _: Jobj => sc.containsNewline(e0, sc.pos)
              case _ => false
          if m then ml = true
          if !v.isInstanceOf[Jnum] then nums = false
          slots(k) = spanOf(e0, sc.pos, m)
          k += 1
        sc.add(']')
        new Plan(false, nums, slots, kids, spanOf(start, sc.pos, ml))
      case d: Jarr.D if fresh(d.fmt, sc) =>
        val start = sc.pos
        val slots = new Array[Long](d.n)
        sc.add('[')
        var k = 0
        while k < d.n do
          if k > 0 then sc.add(sep)
          val e0 = sc.pos
          Jnum.printDblArr(sc, d.xs(k))
          slots(k) = spanOf(e0, sc.pos, false)
          k += 1
        sc.add(']')
        new Plan(false, true, slots, null, spanOf(start, sc.pos, false))
      case f: Jarr.F if fresh(f.fmt, sc) =>
        val start = sc.pos
        val slots = new Array[Long](f.n)
        sc.add('[')
        var k = 0
        while k < f.n do
          if k > 0 then sc.add(sep)
          val e0 = sc.pos
          Jnum.printDblArr(sc, f.xs(k).toDouble)
          slots(k) = spanOf(e0, sc.pos, false)
          k += 1
        sc.add(']')
        new Plan(false, true, slots, null, spanOf(start, sc.pos, false))
      case i: Jarr.I if fresh(i.fmt, sc) =>
        val start = sc.pos
        val slots = new Array[Long](i.n)
        sc.add('[')
        var k = 0
        while k < i.n do
          if k > 0 then sc.add(sep)
          val e0 = sc.pos
          sc.add(i.xs(k).toLong)
          slots(k) = spanOf(e0, sc.pos, false)
          k += 1
        sc.add(']')
        new Plan(false, true, slots, null, spanOf(start, sc.pos, false))
      case o: Jobj if fresh(o.fmt, sc) =>
        val mid = if st.spaceAfterColon then ": " else ":"
        val start = sc.pos
        val live = o.size
        val slots = new Array[Long](2 * live)
        var kids: Array[Plan] | Null = null
        var ml = false
        sc.add('{')
        var k = 0
        var w = 0   // entry ordinal: holes leave the array position ahead of it
        while k < o.n do
          val key = o.ks(k)
          if key ne null then
            if w > 0 then sc.add(sep)
            val k0 = sc.pos
            Jstr.encodeTo(sc, key)
            slots(2 * w) = spanOf(k0, sc.pos, false)
            sc.add(mid)
            val v0 = sc.pos
            val v = o.vs(k)
            val m = flat(v, sc) match
              case q: Plan =>
                val ks = kids match
                  case null =>
                    val ks2 = new Array[Plan](live)
                    kids = ks2
                    ks2
                  case ks2 => ks2
                ks(w) = q
                multi(q.self)
              case null => v match
                case _: Jarr | _: Jobj => sc.containsNewline(v0, sc.pos)
                case _ => false
            if m then ml = true
            slots(2 * w + 1) = spanOf(v0, sc.pos, m)
            w += 1
          k += 1
        sc.add('}')
        new Plan(true, false, slots, kids, spanOf(start, sc.pos, ml))
      case _ =>
        j.printTo(sc)
        null

  /** Emit `p` onto `out`: verbatim if it fits from column `col`, else broken with children
    * at indent `depth + 1` and the closer at `depth`.
    */
  private def reflow(p: Plan, out: Jout, sc: Jout, col: Int, depth: Int, st: Jstyle): Unit =
    if p.slots.length == 0 || (!multi(p.self) && col + len(p.self) <= st.width) then
      sc.copyTo(out, i0(p.self), iN(p.self))
    else if p.obj then
      val padIn = Jstyle.pad(st.indent, depth + 1)
      val mid = if st.spaceAfterColon then ": " else ":"
      out.add('{')
      var k = 0
      val n = p.slots.length >> 1
      while k < n do
        if k > 0 then out.add(',')
        out.add(padIn)
        val ks = p.slots(2 * k)
        sc.copyTo(out, i0(ks), iN(ks))
        out.add(mid)
        val vs = p.slots(2 * k + 1)
        val kid = p.kids match
          case null => null
          case a => a(k)
        kid match
          case q: Plan => reflow(q, out, sc, (depth + 1) * st.indent.length + len(ks) + mid.length, depth + 1, st)
          case null => sc.copyTo(out, i0(vs), iN(vs))
        k += 1
      out.add(Jstyle.pad(st.indent, depth))
      out.add('}')
    else if (p.kids eq null) && !multi(p.self) then grid(p, out, sc, depth, st)
    else
      val padIn = Jstyle.pad(st.indent, depth + 1)
      out.add('[')
      var k = 0
      while k < p.slots.length do
        if k > 0 then out.add(',')
        out.add(padIn)
        val s = p.slots(k)
        val kid = p.kids match
          case null => null
          case a => a(k)
        kid match
          case q: Plan => reflow(q, out, sc, (depth + 1) * st.indent.length, depth + 1, st)
          case null => sc.copyTo(out, i0(s), iN(s))
        k += 1
      out.add(Jstyle.pad(st.indent, depth))
      out.add(']')

  /** An all-scalar array that outgrew its line: wrap the elements into columns sized by the
    * widest, right-aligning when everything is numeric.
    */
  private def grid(p: Plan, out: Jout, sc: Jout, depth: Int, st: Jstyle): Unit =
    val n = p.slots.length
    var colw = 0
    var k = 0
    while k < n do
      val w = len(p.slots(k))
      if w > colw then colw = w
      k += 1
    val sepw = if st.spaceAfterComma then 2 else 1
    val avail = st.width - (depth + 1) * st.indent.length
    var ncols = (avail + sepw) / (colw + sepw)
    if ncols < 1 then ncols = 1
    val padIn = Jstyle.pad(st.indent, depth + 1)
    out.add('[')
    out.add(padIn)
    k = 0
    while k < n do
      val s = p.slots(k)
      if p.nums && ncols > 1 then spaces(out, colw - len(s))
      sc.copyTo(out, i0(s), iN(s))
      if k < n - 1 then
        out.add(',')
        if (k + 1) % ncols == 0 then out.add(padIn)
        else
          if !p.nums && ncols > 1 then spaces(out, colw - len(s))
          if st.spaceAfterComma then out.add(' ')
      k += 1
    out.add(Jstyle.pad(st.indent, depth))
    out.add(']')

  private val plenty = " ".repeat(32)
  private def spaces(out: Jout, n: Int): Unit =
    var k = n
    while k > 32 do
      out.add(plenty, 0, 32)
      k -= 32
    if k > 0 then out.add(plenty, 0, k)
}
