// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-2026 Rex Kerr

package kse.maths.stringmaths

import java.lang.{Math => jm}
import java.lang.Long.{divideUnsigned, remainderUnsigned, compareUnsigned}

import kse.basics._
import kse.basics.intervals._
import kse.maths.ULong


/** Semantic (natural-order) string comparison: numbers compare by numeric value, not
  * character by character, so "file2" sorts before "file10" and "v0.7.3" before "v0.12.1".
  *
  * The object itself is the default ordering (unsigned integers only):
  * {{{
  *   List("file10", "file2").sorted(using SemanticOrder)   // List("file2", "file10")
  * }}}
  *
  * Everything that is not a number compares by code unit (case matters); a number measured
  * against non-number text stands at its first significant digit, so signs and leading
  * zeros do not scatter numbers among the symbol characters.  Numbers of equal value are
  * tied by notation first--Arabic before Roman before spoken--and within one notation the
  * shorter text wins ("7" before "007", "1.0" before "1.000" once decimals are enabled),
  * then code-unit order ("IX" before "ix"); ties break only if nothing later decides.  The
  * order is total, and no two distinct strings compare as equal (`semVer` excepted).
  *
  * Number conventions are chosen with `of()` or the presets: `signed` (leading minus),
  * `decimal` (fractions), `scientific` (exponents), `prose` (comma-grouped digits and
  * spoken numbers: "draft-two", "86,400"), `outline` (Roman numerals: "III.3.A.iv.2.b"),
  * `version` and `semVer` (dotted versions with SemVer pre-release rules).  Custom
  * conventions implement `Judge`, which is handed interval slices of the strings and makes
  * every call; wrap one with `SemanticOrder(judge)` to get an `Ordering[String]`.
  */
object SemanticOrder extends Ordering[String] {
  /** Coarse character classes: decimal digits, letters, and everything else. */
  enum Kind {
    case Digits, Text, Other
  }

  /** The coarse class of one character (Unicode digits and letters count). */
  def kindOf(c: Char): Kind =
    if Character.isDigit(c) then Kind.Digits
    else if Character.isLetter(c) then Kind.Text
    else Kind.Other

  /** The maximal same-kind run starting at `i`, which must be in bounds. */
  def run(s: String, i: Int): Iv =
    val kind = kindOf(s.charAt(i))
    var j = i + 1
    while j < s.length && kindOf(s.charAt(j)) == kind do j += 1
    Iv(i, j)


  ///////////////////////////////////
  /// The judge/engine visitation ///
  ///////////////////////////////////

  /** A comparison convention.  The engine (`compareWith`) walks both strings in lockstep,
    * asking the judge to `claim` the token at each unconsumed position and to `judge` the
    * resulting pair; the engine does only bookkeeping.  Verdicts are in the companion:
    * `Hard` (+-2) decides; `Soft` (+-1) notes that the values match but the text does not,
    * and the first such note breaks the tie if nothing ever decides; `Same` (0) continues;
    * `Ragged` (3, unsigned) says the tokens agree over their common length but differ in
    * length, so the engine advances both sides by the common length and re-claims (this is
    * how ragged text runs realign).  An empty interval marks an exhausted side--even the
    * nothing-sorts-first rule is the judge's call.  A claim on a non-exhausted side must
    * consume at least one character. */
  trait Judge {
    /** The token starting at `i` under this convention (`i < s.length` is guaranteed). */
    def claim(s: String, i: Int): Iv

    /** The verdict on a pair of claimed tokens; an empty interval is an exhausted side. */
    def judge(a: String, ai: Iv, b: String, bi: Iv): Int
  }
  object Judge {
    inline val Same = 0
    inline val Soft = 1
    inline val Hard = 2
    inline val Ragged = 3
  }

  /** Compare two strings under an arbitrary `Judge`; sign convention as `Ordering.compare`. */
  def compareWith(a: String, b: String)(judge: Judge): Int =
    var i = 0
    var k = 0
    var soft = 0
    var hard = 0
    while hard == 0 && (i < a.length || k < b.length) do
      val ai = if i < a.length then judge.claim(a, i) else Iv(i, i)
      val bk = if k < b.length then judge.claim(b, k) else Iv(k, k)
      val v = judge.judge(a, ai, b, bk)
      if v == Judge.Ragged then
        val la = ai.iN - ai.i0
        val lb = bk.iN - bk.i0
        val n = if la < lb then la else lb
        if n <= 0 then throw new IllegalStateException("SemanticOrder.Judge gave a ragged verdict without progress")
        i += n
        k += n
      else if v >= Judge.Hard then hard = 1
      else if v <= -Judge.Hard then hard = -1
      else
        if v != 0 && soft == 0 then soft = v
        if ai.iN <= i && bk.iN <= k then throw new IllegalStateException("SemanticOrder.Judge did not advance")
        if ai.iN > i then i = ai.iN
        if bk.iN > k then k = bk.iN
    if hard != 0 then hard else soft


  //////////////////////////////////
  /// Shared comparison kernels  ///
  //////////////////////////////////

  /** Code-unit comparison of two token spans: `+-Hard` at the first difference, `Same` if
    * textually identical, `Ragged` if one is a proper prefix of the other. */
  def spanCompare(a: String, ai: Iv, b: String, bi: Iv): Int =
    val la = ai.iN - ai.i0
    val lb = bi.iN - bi.i0
    val n = if la < lb then la else lb
    var x = 0
    var c = 0
    while c == 0 && x < n do
      c = a.charAt(ai.i0 + x) - b.charAt(bi.i0 + x)
      x += 1
    if c < 0 then -Judge.Hard
    else if c > 0 then Judge.Hard
    else if la == lb then Judge.Same
    else Judge.Ragged

  /** The extent of a digit-based number token starting at `i` under the given conventions.
    * `i` must start one: a digit, or a sign directly followed by a digit if `negatives`.
    * With `commas`, strictly grouped thousands separators glue ("86,400" but not "1,2345").
    * A dangling '.' or 'e' (nothing glueable after it) is left unconsumed. */
  def numToken(s: String, i: Int, negatives: Boolean = false, decimals: Boolean = false, exponents: Boolean = false, commas: Boolean = false): Iv =
    var j = i
    if negatives && (s.charAt(j) == '-' || s.charAt(j) == '+') then j += 1
    val d0 = j
    while j < s.length && Character.isDigit(s.charAt(j)) do j += 1
    if commas && j > d0 && j - d0 <= 3 then
      var going = true
      while going do
        if j + 3 < s.length && s.charAt(j) == ',' &&
           Character.isDigit(s.charAt(j + 1)) && Character.isDigit(s.charAt(j + 2)) && Character.isDigit(s.charAt(j + 3)) &&
           (j + 4 >= s.length || !Character.isDigit(s.charAt(j + 4)))
        then j += 4
        else going = false
    if decimals && j + 1 < s.length && s.charAt(j) == '.' && Character.isDigit(s.charAt(j + 1)) then
      j += 2
      while j < s.length && Character.isDigit(s.charAt(j)) do j += 1
    if exponents && j + 1 < s.length && (s.charAt(j) == 'e' || s.charAt(j) == 'E') then
      var p = j + 1
      if s.charAt(p) == '+' || s.charAt(p) == '-' then p += 1
      if p < s.length && Character.isDigit(s.charAt(p)) then
        j = p + 1
        while j < s.length && Character.isDigit(s.charAt(j)) do j += 1
    Iv(i, j)

  /** The within-notation equal-value tiebreak: shorter token wins, then code-unit order. */
  def numTie(a: String, ai: Iv, b: String, bi: Iv): Int =
    val la = ai.iN - ai.i0
    val lb = bi.iN - bi.i0
    if la != lb then (if la < lb then -Judge.Soft else Judge.Soft)
    else
      var x = 0
      var c = 0
      while c == 0 && x < la do
        c = a.charAt(ai.i0 + x) - b.charAt(bi.i0 + x)
        x += 1
      if c < 0 then -Judge.Soft else if c > 0 then Judge.Soft else Judge.Same

  private def digitsIn(s: String, i0: Int, iN: Int): Int =
    var n = 0
    var p = i0
    while p < iN do
      if s.charAt(p) != ',' then n += 1
      p += 1
    n

  private def digitCountU(v: Long): Int =
    var n = 1
    var w = v
    while compareUnsigned(w, 10L) >= 0 do
      w = divideUnsigned(w, 10L)
      n += 1
    n

  private def leadingDigitU(v: Long): Int =
    if v == 0L then 0
    else
      var w = v
      while compareUnsigned(w, 10L) >= 0 do w = divideUnsigned(w, 10L)
      w.toInt

  /** Numeric-value comparison of two digit-based number tokens (syntax superset: optional
    * sign, digits with optional comma grouping, optional '.'+digits fraction, optional
    * exponent).  `+-Hard` when the values differ; equal values fall to `numTie`.  Unicode
    * digits are valued via `Character.digit`.  Exponents saturate at 10^15, so tokens with
    * absurdly longer exponents compare by digits, not value--degenerate but still total. */
  def numCompare(a: String, ai: Iv, b: String, bi: Iv): Int =
    // Parse a into sign, digit spans, and (saturated) exponent
    var p = ai.i0
    var aNeg = false
    if a.charAt(p) == '+' then p += 1
    else if a.charAt(p) == '-' then
      aNeg = true
      p += 1
    val ad0 = p
    while p < ai.iN && { val c = a.charAt(p); c == ',' || Character.isDigit(c) } do p += 1
    val adN = p
    var af0 = p
    var afN = p
    if p < ai.iN && a.charAt(p) == '.' then
      p += 1
      af0 = p
      while p < ai.iN && Character.isDigit(a.charAt(p)) do p += 1
      afN = p
    var aExp = 0L
    if p < ai.iN && (a.charAt(p) == 'e' || a.charAt(p) == 'E') then
      p += 1
      var eneg = false
      if p < ai.iN && (a.charAt(p) == '+' || a.charAt(p) == '-') then
        eneg = a.charAt(p) == '-'
        p += 1
      while p < ai.iN do
        if aExp < 1000000000000000L then aExp = 10 * aExp + Character.digit(a.charAt(p), 10)
        p += 1
      if eneg then aExp = -aExp
    var q = bi.i0
    var bNeg = false
    if b.charAt(q) == '+' then q += 1
    else if b.charAt(q) == '-' then
      bNeg = true
      q += 1
    val bd0 = q
    while q < bi.iN && { val c = b.charAt(q); c == ',' || Character.isDigit(c) } do q += 1
    val bdN = q
    var bf0 = q
    var bfN = q
    if q < bi.iN && b.charAt(q) == '.' then
      q += 1
      bf0 = q
      while q < bi.iN && Character.isDigit(b.charAt(q)) do q += 1
      bfN = q
    var bExp = 0L
    if q < bi.iN && (b.charAt(q) == 'e' || b.charAt(q) == 'E') then
      q += 1
      var eneg = false
      if q < bi.iN && (b.charAt(q) == '+' || b.charAt(q) == '-') then
        eneg = b.charAt(q) == '-'
        q += 1
      while q < bi.iN do
        if bExp < 1000000000000000L then bExp = 10 * bExp + Character.digit(b.charAt(q), 10)
        q += 1
      if eneg then bExp = -bExp
    // Find each leading significant digit; magnitude is its decimal position
    var az = ad0
    while az < adN && { val c = a.charAt(az); c == ',' || Character.digit(c, 10) == 0 } do az += 1
    var aZero = false
    var as2 = af0
    var aMag = 0L
    if az < adN then aMag = aExp + (digitsIn(a, az, adN) - 1)
    else
      while as2 < afN && Character.digit(a.charAt(as2), 10) == 0 do as2 += 1
      if as2 == afN then aZero = true
      else aMag = aExp - (as2 - af0) - 1
    var bz = bd0
    while bz < bdN && { val c = b.charAt(bz); c == ',' || Character.digit(c, 10) == 0 } do bz += 1
    var bZero = false
    var bs2 = bf0
    var bMag = 0L
    if bz < bdN then bMag = bExp + (digitsIn(b, bz, bdN) - 1)
    else
      while bs2 < bfN && Character.digit(b.charAt(bs2), 10) == 0 do bs2 += 1
      if bs2 == bfN then bZero = true
      else bMag = bExp - (bs2 - bf0) - 1
    if aZero && bZero then numTie(a, ai, b, bi)
    else if aZero then (if bNeg then Judge.Hard else -Judge.Hard)
    else if bZero then (if aNeg then -Judge.Hard else Judge.Hard)
    else if aNeg != bNeg then (if aNeg then -Judge.Hard else Judge.Hard)
    else
      val flip = if aNeg then -1 else 1
      if aMag != bMag then (if aMag > bMag then Judge.Hard else -Judge.Hard) * flip
      else
        // Equal magnitude: compare significant digit streams, trailing zeros trimmed
        val as1 = az
        var ae1 = adN
        val a2 = if az < adN then af0 else as2
        var a2N = afN
        while a2N > a2 && Character.digit(a.charAt(a2N - 1), 10) == 0 do a2N -= 1
        if a2N == a2 then
          while ae1 > as1 && { val c = a.charAt(ae1 - 1); c == ',' || Character.digit(c, 10) == 0 } do ae1 -= 1
        val bs1 = bz
        var be1 = bdN
        val b2 = if bz < bdN then bf0 else bs2
        var b2N = bfN
        while b2N > b2 && Character.digit(b.charAt(b2N - 1), 10) == 0 do b2N -= 1
        if b2N == b2 then
          while be1 > bs1 && { val c = b.charAt(be1 - 1); c == ',' || Character.digit(c, 10) == 0 } do be1 -= 1
        var pa = as1
        var paF = as1 >= ae1
        if paF then pa = a2
        var pb = bs1
        var pbF = bs1 >= be1
        if pbF then pb = b2
        var ans = 0
        var going = true
        while going do
          var da = -1
          var scanning = true
          while scanning do
            if !paF then
              if pa < ae1 then
                val c = a.charAt(pa)
                pa += 1
                if c != ',' then
                  da = Character.digit(c, 10)
                  scanning = false
              else
                paF = true
                pa = a2
            else if pa < a2N then
              da = Character.digit(a.charAt(pa), 10)
              pa += 1
              scanning = false
            else scanning = false
          var db = -1
          scanning = true
          while scanning do
            if !pbF then
              if pb < be1 then
                val c = b.charAt(pb)
                pb += 1
                if c != ',' then
                  db = Character.digit(c, 10)
                  scanning = false
              else
                pbF = true
                pb = b2
            else if pb < b2N then
              db = Character.digit(b.charAt(pb), 10)
              pb += 1
              scanning = false
            else scanning = false
          if da < 0 && db < 0 then going = false
          else
            val x = if da < 0 then 0 else da
            val y = if db < 0 then 0 else db
            if x != y then
              ans = if x > y then Judge.Hard else -Judge.Hard
              going = false
        if ans != 0 then ans * flip else numTie(a, ai, b, bi)

  // Digit-token vs pure unsigned value (spoken or Roman); verdict from the digit side's
  // perspective.  Equal values tie softly in the digit side's favor (Arabic notation first).
  private def mixedCompare(a: String, ai: Iv, vb: Long): Int =
    var p = ai.i0
    var aNeg = false
    if a.charAt(p) == '+' then p += 1
    else if a.charAt(p) == '-' then
      aNeg = true
      p += 1
    val ad0 = p
    while p < ai.iN && { val c = a.charAt(p); c == ',' || Character.isDigit(c) } do p += 1
    val adN = p
    var af0 = p
    var afN = p
    if p < ai.iN && a.charAt(p) == '.' then
      p += 1
      af0 = p
      while p < ai.iN && Character.isDigit(a.charAt(p)) do p += 1
      afN = p
    var aExp = 0L
    if p < ai.iN && (a.charAt(p) == 'e' || a.charAt(p) == 'E') then
      p += 1
      var eneg = false
      if p < ai.iN && (a.charAt(p) == '+' || a.charAt(p) == '-') then
        eneg = a.charAt(p) == '-'
        p += 1
      while p < ai.iN do
        if aExp < 1000000000000000L then aExp = 10 * aExp + Character.digit(a.charAt(p), 10)
        p += 1
      if eneg then aExp = -aExp
    var az = ad0
    while az < adN && { val c = a.charAt(az); c == ',' || Character.digit(c, 10) == 0 } do az += 1
    var aZero = false
    var as2 = af0
    var aMag = 0L
    if az < adN then aMag = aExp + (digitsIn(a, az, adN) - 1)
    else
      while as2 < afN && Character.digit(a.charAt(as2), 10) == 0 do as2 += 1
      if as2 == afN then aZero = true
      else aMag = aExp - (as2 - af0) - 1
    if aZero && vb == 0L then -Judge.Soft
    else if aZero then -Judge.Hard
    else if vb == 0L then (if aNeg then -Judge.Hard else Judge.Hard)
    else if aNeg then -Judge.Hard
    else
      val vMag = (digitCountU(vb) - 1).toLong
      if aMag != vMag then (if aMag > vMag then Judge.Hard else -Judge.Hard)
      else
        var w = vb
        while remainderUnsigned(w, 10L) == 0L do w = divideUnsigned(w, 10L)
        val wl = digitCountU(w)
        val as1 = az
        var ae1 = adN
        val a2 = if az < adN then af0 else as2
        var a2N = afN
        while a2N > a2 && Character.digit(a.charAt(a2N - 1), 10) == 0 do a2N -= 1
        if a2N == a2 then
          while ae1 > as1 && { val c = a.charAt(ae1 - 1); c == ',' || Character.digit(c, 10) == 0 } do ae1 -= 1
        var pa = as1
        var paF = as1 >= ae1
        if paF then pa = a2
        var x = 0
        var ans = 0
        var going = true
        while going do
          var da = -1
          var scanning = true
          while scanning do
            if !paF then
              if pa < ae1 then
                val c = a.charAt(pa)
                pa += 1
                if c != ',' then
                  da = Character.digit(c, 10)
                  scanning = false
              else
                paF = true
                pa = a2
            else if pa < a2N then
              da = Character.digit(a.charAt(pa), 10)
              pa += 1
              scanning = false
            else scanning = false
          val dv =
            if x >= wl then -1
            else
              var t = w
              var k = wl - 1 - x
              while k > 0 do
                t = divideUnsigned(t, 10L)
                k -= 1
              remainderUnsigned(t, 10L).toInt
          if da < 0 && dv < 0 then going = false
          else
            val u = if da < 0 then 0 else da
            val v = if dv < 0 then 0 else dv
            if u != v then
              ans = if u > v then Judge.Hard else -Judge.Hard
              going = false
          x += 1
        if ans != 0 then ans else -Judge.Soft

  // Both tokens are pure values; ranks order equal values by notation (Roman before spoken)
  private def valueCompare(a: String, ai: Iv, va: Long, ra: Int, b: String, bi: Iv, vb: Long, rb: Int): Int =
    val c = compareUnsigned(va, vb)
    if c > 0 then Judge.Hard
    else if c < 0 then -Judge.Hard
    else if ra != rb then (if ra < rb then -Judge.Soft else Judge.Soft)
    else numTie(a, ai, b, bi)


  //////////////////////////////
  /// The built-in orderings ///
  //////////////////////////////

  // Where a number token stands when measured against non-number text: at its first
  // significant digit, ASCII-normalized.  Using the token's own first character instead
  // breaks transitivity--a signed "+7" would sort below "-" by sign yet above "0" by value.
  private def anchorOf(s: String, iv: Iv): Char =
    var p = iv.i0
    var d = 0
    while p < iv.iN && d <= 0 do
      val e = Character.digit(s.charAt(p), 10)
      if e > 0 then d = e
      p += 1
    ('0' + d).toChar

  // Exactly one of the tokens is a number; anchors can never be equal (digit vs non-digit)
  private def anchorCompare(a: String, ai: Iv, aNum: Boolean, b: String, bi: Iv, bNum: Boolean): Int =
    val ca = if aNum then anchorOf(a, ai) else a.charAt(ai.i0)
    val cb = if bNum then anchorOf(b, bi) else b.charAt(bi.i0)
    if ca < cb then -Judge.Hard else Judge.Hard

  private final class NumJudge(
    negatives: Boolean, decimals: Boolean, exponents: Boolean,
    commas: Boolean, spoken: Boolean, roman: Boolean, binder: Char
  ) extends Judge {
    private def numStart(s: String, i: Int): Boolean =
      Character.isDigit(s.charAt(i)) ||
      ( negatives && (s.charAt(i) == '-' || s.charAt(i) == '+') &&
        i + 1 < s.length && Character.isDigit(s.charAt(i + 1)) &&
        (i == 0 || !Character.isDigit(s.charAt(i - 1))) )

    def claim(s: String, i: Int): Iv =
      val c = s.charAt(i)
      if numStart(s, i) then numToken(s, i, negatives, decimals, exponents, commas)
      else if Character.isLetter(c) then
        var t = Iv(i, i)
        if spoken then t = SpokenNumber.find(s, i, binder)
        if t.iN == i && roman then
          val rr = RomanNumber.find(s, i)
          if rr.iN > i && (rr.iN >= s.length || !Character.isLetter(s.charAt(rr.iN))) then t = rr
        if t.iN > i then t else run(s, i)
      else
        val r = run(s, i)
        if negatives then
          // A sign that starts a number token ends this run early
          var j = i + 1
          while j < r.iN && !numStart(s, j) do j += 1
          Iv(i, j)
        else r

    // Token notations: 0 = not a number, 1 = Arabic digits, 2 = spoken, 3 = Roman
    private def tokKind(s: String, iv: Iv): Int =
      val c = s.charAt(iv.i0)
      if numStart(s, iv.i0) then 1
      else if Character.isLetter(c) then
        if spoken && SpokenNumber.find(s, iv.i0, binder) == iv then 2
        else if roman && RomanNumber.find(s, iv.i0) == iv && (iv.iN >= s.length || !Character.isLetter(s.charAt(iv.iN))) then 3
        else 0
      else 0

    private def tokValue(s: String, iv: Iv, kind: Int): Long =
      if kind == 2 then SpokenNumber.valueOf(s, iv, binder).signed else RomanNumber.valueOf(s, iv).signed

    // Equal values tie by notation: Arabic, then Roman, then spoken
    private def catRank(kind: Int): Int =
      if kind == 1 then 0 else if kind == 3 then 1 else 2

    private def tokAnchor(s: String, iv: Iv, kind: Int): Char =
      if kind == 0 then s.charAt(iv.i0)
      else if kind == 1 then anchorOf(s, iv)
      else ('0' + leadingDigitU(tokValue(s, iv, kind))).toChar

    def judge(a: String, ai: Iv, b: String, bi: Iv): Int =
      if ai.i0 == ai.iN then -Judge.Hard
      else if bi.i0 == bi.iN then Judge.Hard
      else
        val ka = tokKind(a, ai)
        val kb = tokKind(b, bi)
        if ka == 0 && kb == 0 then spanCompare(a, ai, b, bi)
        else if ka == 0 || kb == 0 then
          val ca = tokAnchor(a, ai, ka)
          val cb = tokAnchor(b, bi, kb)
          if ca < cb then -Judge.Hard else Judge.Hard
        else if ka == 1 && kb == 1 then numCompare(a, ai, b, bi)
        else if ka == 1 then mixedCompare(a, ai, tokValue(b, bi, kb))
        else if kb == 1 then -mixedCompare(b, bi, tokValue(a, ai, ka))
        else valueCompare(a, ai, tokValue(a, ai, ka), catRank(ka), b, bi, tokValue(b, bi, kb), catRank(kb))
  }

  private final class VerJudge(strict: Boolean) extends Judge {
    def claim(s: String, i: Int): Iv =
      if strict && s.charAt(i) == '+' then Iv(i, s.length)   // Build metadata: all the rest
      else
        val r = run(s, i)
        if strict && kindOf(s.charAt(i)) == Kind.Other then
          var j = i + 1
          while j < r.iN && s.charAt(j) != '+' do j += 1
          Iv(i, j)
        else r

    private def preRelease(s: String, iv: Iv): Boolean =
      s.charAt(iv.i0) == '-' && iv.i0 > 0 && Character.isDigit(s.charAt(iv.i0 - 1))

    def judge(a: String, ai: Iv, b: String, bi: Iv): Int =
      val aEnd = ai.i0 == ai.iN || (strict && a.charAt(ai.i0) == '+')
      val bEnd = bi.i0 == bi.iN || (strict && b.charAt(bi.i0) == '+')
      if aEnd && bEnd then Judge.Same
      else if aEnd then (if preRelease(b, bi) then Judge.Hard else -Judge.Hard)
      else if bEnd then (if preRelease(a, ai) then -Judge.Hard else Judge.Hard)
      else
        val an = Character.isDigit(a.charAt(ai.i0))
        val bn = Character.isDigit(b.charAt(bi.i0))
        if an && bn then numCompare(a, ai, b, bi)
        else if an != bn then anchorCompare(a, ai, an, b, bi, bn)
        else spanCompare(a, ai, b, bi)
  }

  private val natural: Judge = new NumJudge(false, false, false, false, false, false, '-')

  /** The default semantic comparison: unsigned integer runs by value, all else by code unit. */
  def compare(a: String, b: String): Int = compareWith(a, b)(natural)

  private final class Via(judge: Judge) extends Ordering[String] {
    def compare(a: String, b: String): Int = compareWith(a, b)(judge)
  }

  /** An `Ordering[String]` running a custom `Judge`. */
  def apply(judge: Judge): Ordering[String] = new Via(judge)

  /** A semantic ordering with number syntax extended as requested; all-false is the default
    * order.  `commas` glues strict thousands grouping ("86,400"); `spoken` recognizes
    * spelled-out numbers ("draft-two", words joined by `binder` or '-'); `roman` recognizes
    * Roman numerals spanning a whole letter run (beware: "mix" is 1009).  All notations
    * share one value space, so "draft-2" < "draft-three" < "draft,004" orders sensibly. */
  def of(negatives: Boolean = false, decimals: Boolean = false, exponents: Boolean = false,
         commas: Boolean = false, spoken: Boolean = false, roman: Boolean = false, binder: Char = '-'): Ordering[String] =
    if !negatives && !decimals && !exponents && !commas && !spoken && !roman then this
    else new Via(new NumJudge(negatives, decimals, exponents, commas, spoken, roman, binder))

  /** Numbers may take a leading sign when not directly preceded by a digit: "a-5" sorts
    * before "a-3", but in "5-3" the dash is just a dash. */
  val signed: Ordering[String] = of(negatives = true)

  /** Signed numbers with decimal fractions: "1.09" sorts before "1.1".  Not for dotted
    * version strings, where "1.2.3" must read as 1, 2, 3--use `version` for those. */
  val decimal: Ordering[String] = of(negatives = true, decimals = true)

  /** Signed decimals with exponents: "3e1" sorts before "2E2". */
  val scientific: Ordering[String] = of(negatives = true, decimals = true, exponents = true)

  /** Everyday text ordering: comma-grouped digits ("86,400" > "3600") and spoken numbers
    * ("draft-one" < "draft-2" < "draft-three"). */
  val prose: Ordering[String] = of(commas = true, spoken = true)

  /** Outline ordering: Roman numerals join digits in one value space, so
    * "III.3.A.iv.2.b"-style labels sort sensibly ("V" < "IX").  Opt-in because any word
    * spelled entirely in Roman letters ("mix") reads as a number. */
  val outline: Ordering[String] = of(roman = true)

  /** Version-string ordering: the default order plus the SemVer pre-release rule, so
    * "1.0.0-alpha" sorts before "1.0.0".  Total, unlike `semVer`: build metadata simply
    * compares as text, so distinct strings never compare equal. */
  val version: Ordering[String] = new Via(new VerJudge(false))

  /** Strict SemVer precedence: `version` but with "+build" metadata ignored, so versions
    * differing only in metadata compare equal (0).  Beware in sorted maps and sets, which
    * treat compare-0 keys as duplicates; prefer `version` when a total order matters.
    * Assumes well-formed versions; arbitrary strings degrade to roughly the default order. */
  val semVer: Ordering[String] = new Via(new VerJudge(true))
}


/** English spelled-out numbers: eager parsing ("three-hundred-and-five", "draft-one") and
  * production over the full `ULong` range.  Words must be complete letter runs joined by a
  * binding character (plus '-', always accepted, and "and" glue), so "oneself" is not 1.
  * Parsing is case-insensitive; production is lowercase, either conventional
  * ("fifty-six thousand three hundred and ninety-two") or fully bound
  * ("three-hundred-and-five"). */
object SpokenNumber {
  private val subWords = Array(
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
    "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  private val subValues = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 40, 50, 60, 70, 80, 90)
  private val scaleWords = Array("thousand", "million", "billion", "trillion", "quadrillion", "quintillion")
  private val scaleValues = Array(1000L, 1000000L, 1000000000L, 1000000000000L, 1000000000000000L, 1000000000000000000L)

  private def isWord(s: String, p: Int, q: Int, w: String): Boolean =
    q - p == w.length && s.regionMatches(true, p, w, 0, w.length)

  private def subAt(s: String, p: Int, q: Int): Int =
    var v = -1
    var k = 0
    while v < 0 && k < subWords.length do
      if isWord(s, p, q, subWords(k)) then v = subValues(k)
      k += 1
    v

  private def scaleAt(s: String, p: Int, q: Int): Long =
    var v = -1L
    var k = 0
    while v < 0 && k < scaleWords.length do
      if isWord(s, p, q, scaleWords(k)) then v = scaleValues(k)
      k += 1
    v

  // One walk serves both find and valueOf.  States: 0 group start, 1 after tens,
  // 2 after complete sub, 3 after hundred, 4 after "and", 5 after scale word.
  private def scan(s: String, i0: Int, binder: Char, wantEnd: Boolean): Long =
    var p = i0
    var total = 0L
    var hund = 0L
    var sub = 0L
    var floor = Long.MaxValue
    var goodEnd = i0
    var goodVal = 0L
    var state = 0
    var going = p < s.length && Character.isLetter(s.charAt(p))
    while going do
      var q = p
      while q < s.length && Character.isLetter(s.charAt(q)) do q += 1
      var complete = false
      val sv = subAt(s, p, q)
      if sv > 0 then
        if state == 0 || state == 3 || state == 4 || state == 5 || (state == 1 && sv < 10) then
          if state == 1 then sub += sv
          else sub = sv
          state = if sv >= 20 then 1 else 2
          complete = true
        else going = false
      else if isWord(s, p, q, "hundred") then
        if (state == 1 || state == 2) && hund == 0L && sub > 0L then
          hund = sub * 100L
          sub = 0L
          state = 3
          complete = true
        else going = false
      else if isWord(s, p, q, "and") then
        if state == 3 || state == 5 then state = 4
        else going = false
      else if isWord(s, p, q, "zero") then
        if p == i0 then
          goodEnd = q
          goodVal = 0L
        going = false
      else
        val sc = scaleAt(s, p, q)
        val part = hund + sub
        if sc > 0L && (state == 1 || state == 2 || state == 3) && part > 0L && sc < floor && jm.multiplyHigh(part, sc) == 0L then
          val t2 = total + part * sc
          if java.lang.Long.compareUnsigned(t2, total) < 0 then going = false
          else
            total = t2
            hund = 0L
            sub = 0L
            floor = sc
            state = 5
            complete = true
        else going = false
      if going && complete then
        val v2 = total + hund + sub
        if java.lang.Long.compareUnsigned(v2, total) >= 0 then
          goodEnd = q
          goodVal = v2
        else going = false
      if going then
        if q < s.length && (s.charAt(q) == binder || s.charAt(q) == '-') && q + 1 < s.length && Character.isLetter(s.charAt(q + 1)) then p = q + 1
        else going = false
    if wantEnd then goodEnd.toLong else goodVal

  /** The extent of the longest spoken number starting at `i` (empty if there is none). */
  def find(s: String, i: Int, binder: Char = '-'): Iv =
    if i >= s.length then Iv(i, i)
    else Iv(i, scan(s, i, binder, wantEnd = true).toInt)

  /** The value of a spoken number found by `find` (with the same binder). */
  def valueOf(s: String, iv: Iv, binder: Char = '-'): ULong =
    ULong.wrap(scan(s, iv.i0, binder, wantEnd = false))

  /** English words for any `ULong`, lowercase.  Conventionally bound by default: spaces
    * between words, '-' inside compounds like "fifty-six", "and" before a final sub-hundred
    * part.  With `bindAll`, every separator is `binder` instead: "three-hundred-and-five". */
  def text(value: ULong, binder: Char = '-', bindAll: Boolean = false): String =
    val v = value.signed
    if v == 0L then "zero"
    else
      val sep = if bindAll then binder else ' '
      val hyp = if bindAll then binder else '-'
      val sb = new java.lang.StringBuilder
      def sub(r: Int): Unit =
        if r < 20 then sb append subWords(r - 1) __ Unit
        else
          sb append subWords(17 + r / 10)
          if r % 10 > 0 then
            sb append hyp
            sb append subWords(r % 10 - 1) __ Unit
      def group(g: Int): Unit =
        val h = g / 100
        val r = g % 100
        if h > 0 then
          sb append subWords(h - 1)
          sb append sep
          sb append "hundred" __ Unit
        if r > 0 then
          if h > 0 then
            sb append sep
            sb append "and"
            sb append sep __ Unit
          sub(r)
      val gs = new Array[Int](7)
      gs(6) = java.lang.Long.divideUnsigned(v, 1000000000000000000L).toInt
      var t = java.lang.Long.remainderUnsigned(v, 1000000000000000000L)
      var k = 0
      while k < 6 do
        gs(k) = java.lang.Long.remainderUnsigned(t, 1000L).toInt
        t = java.lang.Long.divideUnsigned(t, 1000L)
        k += 1
      var first = true
      k = 6
      while k > 0 do
        if gs(k) > 0 then
          if !first then sb append sep __ Unit
          group(gs(k))
          sb append sep
          sb append scaleWords(k - 1)
          first = false
        k -= 1
      if gs(0) > 0 then
        if !first then
          sb append sep
          if gs(0) < 100 then
            sb append "and"
            sb append sep __ Unit
        group(gs(0))
      sb.toString
}


/** Roman numerals: strict subtractive grammar (M* (CM|CD|D?C{0,3}) (XC|XL|L?X{0,3})
  * (IX|IV|V?I{0,3})), uniform case only, parsed eagerly; production for 1-3999.  Note that
  * any string of Roman letters that happens to satisfy the grammar is a number ("mix" is
  * 1009), which is why Roman recognition is opt-in for orderings. */
object RomanNumber {
  private def scan(s: String, i0: Int, wantEnd: Boolean): Long =
    val c0 = s.charAt(i0)
    val lower = c0 >= 'a' && c0 <= 'z'
    def dv(p: Int): Int =
      if p >= s.length then -1
      else
        var c = s.charAt(p)
        if lower then
          if c >= 'a' && c <= 'z' then c = (c - 32).toChar else return -1
        else if !(c >= 'A' && c <= 'Z') then return -1
        c match
          case 'M' => 1000
          case 'D' => 500
          case 'C' => 100
          case 'L' => 50
          case 'X' => 10
          case 'V' => 5
          case 'I' => 1
          case _   => -1
    var p = i0
    var value = 0L
    while dv(p) == 1000 do
      value += 1000
      p += 1
    if dv(p) == 100 && dv(p + 1) == 1000 then
      value += 900
      p += 2
    else if dv(p) == 100 && dv(p + 1) == 500 then
      value += 400
      p += 2
    else
      if dv(p) == 500 then
        value += 500
        p += 1
      var n = 0
      while n < 3 && dv(p) == 100 do
        value += 100
        p += 1
        n += 1
    if dv(p) == 10 && dv(p + 1) == 100 then
      value += 90
      p += 2
    else if dv(p) == 10 && dv(p + 1) == 50 then
      value += 40
      p += 2
    else
      if dv(p) == 50 then
        value += 50
        p += 1
      var n = 0
      while n < 3 && dv(p) == 10 do
        value += 10
        p += 1
        n += 1
    if dv(p) == 1 && dv(p + 1) == 10 then
      value += 9
      p += 2
    else if dv(p) == 1 && dv(p + 1) == 5 then
      value += 4
      p += 2
    else
      if dv(p) == 5 then
        value += 5
        p += 1
      var n = 0
      while n < 3 && dv(p) == 1 do
        value += 1
        p += 1
        n += 1
    if wantEnd then p.toLong else value

  /** The extent of the longest valid Roman numeral starting at `i` (empty if none). */
  def find(s: String, i: Int): Iv =
    if i >= s.length then Iv(i, i)
    else Iv(i, scan(s, i, wantEnd = true).toInt)

  /** The value of a Roman numeral found by `find`. */
  def valueOf(s: String, iv: Iv): ULong =
    ULong.wrap(scan(s, iv.i0, wantEnd = false))

  private val steps = Array(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
  private val glyphsU = Array("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")
  private val glyphsL = Array("m", "cm", "d", "cd", "c", "xc", "l", "xl", "x", "ix", "v", "iv", "i")

  /** The Roman numeral for 1-3999 (empty string outside that range), upper or lowercase. */
  def text(value: Int, lower: Boolean = false): String =
    if value < 1 || value > 3999 then ""
    else
      val g = if lower then glyphsL else glyphsU
      val sb = new java.lang.StringBuilder
      var v = value
      var k = 0
      while v > 0 do
        if v >= steps(k) then
          sb append g(k)
          v -= steps(k)
        else k += 1
      sb.toString
}
