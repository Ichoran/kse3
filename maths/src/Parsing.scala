// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.maths


import scala.annotation.targetName

import kse.basics.Mem


/** Covering text-to-integer parsing with in-band failure: each parser consumes exactly
  * `[i0, iN)` as one number and answers a primitive, with no boxing on either the success
  * or the failure path.  Failure is signalled by a sentinel value, and the sentinels are
  * structureless nowhere points -- values that carry no meaning and arise essentially
  * never in real data:
  *
  *   - `long` answers [[failLong]], `-9170187325617826341`.
  *   - `uLong` answers [[failULong]], `18276556748091725275`.
  *   - `hex` answers [[failHex]], `c7a16d3e52b84f19`.
  *
  * A boundary value like `-2^63`, all-ones, or `0x8000000000000000` would have been the
  * wrong choice: those are values callers genuinely have, so the disambiguation below
  * would be live rather than vestigial.
  *
  * A sentinel is still a reachable value -- the input can spell it -- so a caller who
  * cares about that one input in 10^19 confirms with the matching `spellsFail...`, which
  * answers whether the input really does spell the sentinel:
  * {{{
  * val x = Parse.long(s)
  * if x == Parse.failLong && !Parse.spellsFailLong(s) then // ... the parse failed
  * }}}
  * A caller wanting a narrower type range-checks the wide result instead, at which point
  * the sentinel is out of range and no confirmation is needed at all.
  *
  * The grammar matches Grok's number tokens: decimal signed takes an optional `+` or `-`,
  * decimal unsigned an optional `+`, and hex no sign and no `0x` prefix (case-insensitive
  * digits); leading zeros are legal and not significant everywhere.  The floating-point
  * counterparts live in [[EiselLemire]], with their own in-band sentinel (a NaN payload).
  */
object Parse {

  /** The value `long` answers on failure: a nowhere point, `-9170187325617826341`.  The
    * input can also spell it; confirm with [[spellsFailLong]] if that matters.
    */
  inline def failLong: Long = 0x80BCF3368CC295DBL

  /** The value `uLong` answers on failure: a nowhere point, `18276556748091725275`.  The
    * input can also spell it; confirm with [[spellsFailULong]] if that matters.
    */
  inline def failULong: ULong = ULong.wrap(0xFDA35F876F4695DBL)

  /** The value `hex` answers on failure: a nowhere point, `c7a16d3e52b84f19`.  The input
    * can also spell it; confirm with [[spellsFailHex]] if that matters.
    */
  inline def failHex: Long = 0xC7A16D3E52B84F19L

  // Derived from the sentinels themselves, so the two can never drift apart
  private val failLongDigits: String = java.lang.Long.toString(-failLong)
  private val failULongDigits: String = java.lang.Long.toUnsignedString(failULong.signed)
  private val failHexDigits: String = java.lang.Long.toHexString(failHex)


  //////////////////////////////////////////////////////////////
  /// The engines, templated over character access like EL's ///
  //////////////////////////////////////////////////////////////

  // Accumulates the magnitude as an unsigned Long: 19 significant digits at most 9.99e18 fit
  // u64 exactly, so the sign cases reduce to unsigned-range checks on the raw bits.
  private inline def longImpl(inline at: Long => Int)(i0: Long, iN: Long): Long =
    var j = i0
    var c = if j < iN then at(j) else -1
    var neg = false
    if c == '-' || c == '+' then
      neg = c == '-'
      j += 1
      c = if j < iN then at(j) else -1
    var any = false
    while c == '0' do
      any = true
      j += 1
      c = if j < iN then at(j) else -1
    var m = 0L
    var nd = 0
    while c >= '0' && c <= '9' && nd < 19 do
      m = m*10 + (c - '0')
      nd += 1
      j += 1
      c = if j < iN then at(j) else -1
    if c >= '0' && c <= '9' then failLong            // a 20th significant digit never fits
    else if j != iN || !(any || nd > 0) then failLong
    else if neg then
      if m < 0 && m != Long.MinValue then failLong   // magnitude above 2^63
      else -m                                        // -(2^63) negates to itself, correctly
    else if m < 0 then failLong                      // magnitude above 2^63 - 1
    else m

  // Answers the u64 bit pattern in a raw Long; overflow is caught stepwise against
  // (2^64-1)/10 = 1844674407370955161, so no digit count is needed.
  private inline def uLongImpl(inline at: Long => Int)(i0: Long, iN: Long): Long =
    var j = i0
    var c = if j < iN then at(j) else -1
    if c == '+' then
      j += 1
      c = if j < iN then at(j) else -1
    var any = false
    while c == '0' do
      any = true
      j += 1
      c = if j < iN then at(j) else -1
    var m = 0L
    var bad = false
    while c >= '0' && c <= '9' && !bad do
      val d = c - '0'
      if java.lang.Long.compareUnsigned(m, 1844674407370955161L) > 0 || (m == 1844674407370955161L && d > 5) then bad = true
      else
        m = m*10 + d
        any = true
        j += 1
        c = if j < iN then at(j) else -1
    if bad || j != iN || !any then failULong.signed
    else m

  private inline def hexImpl(inline at: Long => Int)(i0: Long, iN: Long): Long =
    var j = i0
    var c = if j < iN then at(j) else -1
    var any = false
    while c == '0' do
      any = true
      j += 1
      c = if j < iN then at(j) else -1
    var m = 0L
    var nd = 0
    var v = if c >= '0' && c <= '9' then c - '0' else { val lc = c | 0x20; if lc >= 'a' && lc <= 'f' then lc - 'a' + 10 else -1 }
    while v >= 0 && nd < 16 do
      m = (m << 4) | v
      nd += 1
      j += 1
      c = if j < iN then at(j) else -1
      v = if c >= '0' && c <= '9' then c - '0' else { val lc = c | 0x20; if lc >= 'a' && lc <= 'f' then lc - 'a' + 10 else -1 }
    if v >= 0 then failHex                           // a 17th significant digit never fits
    else if j != iN || !(any || nd > 0) then failHex
    else m

  // Whether [j, iN) is exactly `canon`; `fold` lowercases the input for hex (which leaves
  // digits alone, as '0' | 0x20 is '0')
  private inline def tailIs(inline at: Long => Int)(j: Long, iN: Long, canon: String, fold: Boolean): Boolean =
    if iN - j != canon.length then false
    else
      var k = 0
      var good = true
      while good && k < canon.length do
        val c = at(j + k)
        if (if fold then c | 0x20 else c) != canon.charAt(k) then good = false
        k += 1
      good

  private inline def spellsFailLongImpl(inline at: Long => Int)(i0: Long, iN: Long): Boolean =
    var j = i0
    if j < iN && at(j) == '-' then
      j += 1
      while j < iN && at(j) == '0' do j += 1
      tailIs(at)(j, iN, failLongDigits, false)
    else false

  private inline def spellsFailULongImpl(inline at: Long => Int)(i0: Long, iN: Long): Boolean =
    var j = i0
    if j < iN && at(j) == '+' then j += 1
    while j < iN && at(j) == '0' do j += 1
    tailIs(at)(j, iN, failULongDigits, false)

  private inline def spellsFailHexImpl(inline at: Long => Int)(i0: Long, iN: Long): Boolean =
    var j = i0
    while j < iN && at(j) == '0' do j += 1
    tailIs(at)(j, iN, failHexDigits, true)


  ///////////////////////////////////////////////////
  /// Monomorphic instantiations, one per source  ///
  ///////////////////////////////////////////////////

  private inline def strAt(s: String): Long => Int = j => s.charAt(j.toInt)
  private inline def arrBAt(ab: Array[Byte]): Long => Int = j => ab(j.toInt) & 0xFF
  private inline def arrCAt(ac: Array[Char]): Long => Int = j => ac(j.toInt)
  private inline def memBAt(mb: Mem[Byte]): Long => Int = j => mb.getB(j) & 0xFF
  private inline def memCAt(mc: Mem[Char]): Long => Int = j => mc.getC(j)

  private def longStr(s: String, i0: Int, iN: Int): Long = longImpl(strAt(s))(i0, iN)
  private def longArrB(ab: Array[Byte], i0: Int, iN: Int): Long = longImpl(arrBAt(ab))(i0, iN)
  private def longArrC(ac: Array[Char], i0: Int, iN: Int): Long = longImpl(arrCAt(ac))(i0, iN)
  private def longMemB(mb: Mem[Byte], i0: Long, iN: Long): Long = longImpl(memBAt(mb))(i0, iN)
  private def longMemC(mc: Mem[Char], i0: Long, iN: Long): Long = longImpl(memCAt(mc))(i0, iN)

  private def uLongStr(s: String, i0: Int, iN: Int): Long = uLongImpl(strAt(s))(i0, iN)
  private def uLongArrB(ab: Array[Byte], i0: Int, iN: Int): Long = uLongImpl(arrBAt(ab))(i0, iN)
  private def uLongArrC(ac: Array[Char], i0: Int, iN: Int): Long = uLongImpl(arrCAt(ac))(i0, iN)
  private def uLongMemB(mb: Mem[Byte], i0: Long, iN: Long): Long = uLongImpl(memBAt(mb))(i0, iN)
  private def uLongMemC(mc: Mem[Char], i0: Long, iN: Long): Long = uLongImpl(memCAt(mc))(i0, iN)

  private def hexStr(s: String, i0: Int, iN: Int): Long = hexImpl(strAt(s))(i0, iN)
  private def hexArrB(ab: Array[Byte], i0: Int, iN: Int): Long = hexImpl(arrBAt(ab))(i0, iN)
  private def hexArrC(ac: Array[Char], i0: Int, iN: Int): Long = hexImpl(arrCAt(ac))(i0, iN)
  private def hexMemB(mb: Mem[Byte], i0: Long, iN: Long): Long = hexImpl(memBAt(mb))(i0, iN)
  private def hexMemC(mc: Mem[Char], i0: Long, iN: Long): Long = hexImpl(memCAt(mc))(i0, iN)

  private def sfLongStr(s: String, i0: Int, iN: Int): Boolean = spellsFailLongImpl(strAt(s))(i0, iN)
  private def sfLongArrB(ab: Array[Byte], i0: Int, iN: Int): Boolean = spellsFailLongImpl(arrBAt(ab))(i0, iN)
  private def sfLongArrC(ac: Array[Char], i0: Int, iN: Int): Boolean = spellsFailLongImpl(arrCAt(ac))(i0, iN)
  private def sfLongMemB(mb: Mem[Byte], i0: Long, iN: Long): Boolean = spellsFailLongImpl(memBAt(mb))(i0, iN)
  private def sfLongMemC(mc: Mem[Char], i0: Long, iN: Long): Boolean = spellsFailLongImpl(memCAt(mc))(i0, iN)

  private def sfULongStr(s: String, i0: Int, iN: Int): Boolean = spellsFailULongImpl(strAt(s))(i0, iN)
  private def sfULongArrB(ab: Array[Byte], i0: Int, iN: Int): Boolean = spellsFailULongImpl(arrBAt(ab))(i0, iN)
  private def sfULongArrC(ac: Array[Char], i0: Int, iN: Int): Boolean = spellsFailULongImpl(arrCAt(ac))(i0, iN)
  private def sfULongMemB(mb: Mem[Byte], i0: Long, iN: Long): Boolean = spellsFailULongImpl(memBAt(mb))(i0, iN)
  private def sfULongMemC(mc: Mem[Char], i0: Long, iN: Long): Boolean = spellsFailULongImpl(memCAt(mc))(i0, iN)

  private def sfHexStr(s: String, i0: Int, iN: Int): Boolean = spellsFailHexImpl(strAt(s))(i0, iN)
  private def sfHexArrB(ab: Array[Byte], i0: Int, iN: Int): Boolean = spellsFailHexImpl(arrBAt(ab))(i0, iN)
  private def sfHexArrC(ac: Array[Char], i0: Int, iN: Int): Boolean = spellsFailHexImpl(arrCAt(ac))(i0, iN)
  private def sfHexMemB(mb: Mem[Byte], i0: Long, iN: Long): Boolean = spellsFailHexImpl(memBAt(mb))(i0, iN)
  private def sfHexMemC(mc: Mem[Char], i0: Long, iN: Long): Boolean = spellsFailHexImpl(memCAt(mc))(i0, iN)


  ///////////////////////
  /// The public API  ///
  ///////////////////////

  /** The `Long` whose decimal rendering (optional sign, leading zeros allowed) occupies
    * exactly `[i0, iN)` of `s`, or [[failLong]] (see there) if that range is not one.
    */
  def long(s: String, i0: Int, iN: Int): Long = longStr(s, i0, iN)

  /** As the range `long`, over the whole string. */
  inline def long(s: String): Long = long(s, 0, s.length)

  /** As the String `long`, over ASCII bytes. */
  def long(ab: Array[Byte], i0: Int, iN: Int): Long = longArrB(ab, i0, iN)

  /** As the String `long`, over a whole ASCII byte array. */
  inline def long(ab: Array[Byte]): Long = long(ab, 0, ab.length)

  /** As the String `long`, over chars. */
  def long(ac: Array[Char], i0: Int, iN: Int): Long = longArrC(ac, i0, iN)

  /** As the String `long`, over a whole char array. */
  inline def long(ac: Array[Char]): Long = long(ac, 0, ac.length)

  /** As the String `long`, over ASCII bytes in memory. */
  @targetName("longMemByte")
  def long(mb: Mem[Byte], i0: Long, iN: Long): Long = longMemB(mb, i0, iN)

  /** As the String `long`, over all the ASCII bytes in memory. */
  @targetName("longMemByteAll")
  inline def long(mb: Mem[Byte]): Long = long(mb, 0L, mb.length)

  /** As the String `long`, over chars in memory; positions index chars, not bytes. */
  @targetName("longMemChar")
  def long(mc: Mem[Char], i0: Long, iN: Long): Long = longMemC(mc, i0, iN)

  /** As the String `long`, over all the chars in memory. */
  @targetName("longMemCharAll")
  inline def long(mc: Mem[Char]): Long = long(mc, 0L, mc.length)

  /** The `ULong` whose decimal rendering (optional `+`, leading zeros allowed) occupies
    * exactly `[i0, iN)` of `s`, or [[failULong]] (see there) if that range is not one.
    */
  def uLong(s: String, i0: Int, iN: Int): ULong = ULong.wrap(uLongStr(s, i0, iN))

  /** As the range `uLong`, over the whole string. */
  inline def uLong(s: String): ULong = uLong(s, 0, s.length)

  /** As the String `uLong`, over ASCII bytes. */
  def uLong(ab: Array[Byte], i0: Int, iN: Int): ULong = ULong.wrap(uLongArrB(ab, i0, iN))

  /** As the String `uLong`, over a whole ASCII byte array. */
  inline def uLong(ab: Array[Byte]): ULong = uLong(ab, 0, ab.length)

  /** As the String `uLong`, over chars. */
  def uLong(ac: Array[Char], i0: Int, iN: Int): ULong = ULong.wrap(uLongArrC(ac, i0, iN))

  /** As the String `uLong`, over a whole char array. */
  inline def uLong(ac: Array[Char]): ULong = uLong(ac, 0, ac.length)

  /** As the String `uLong`, over ASCII bytes in memory. */
  @targetName("uLongMemByte")
  def uLong(mb: Mem[Byte], i0: Long, iN: Long): ULong = ULong.wrap(uLongMemB(mb, i0, iN))

  /** As the String `uLong`, over all the ASCII bytes in memory. */
  @targetName("uLongMemByteAll")
  inline def uLong(mb: Mem[Byte]): ULong = uLong(mb, 0L, mb.length)

  /** As the String `uLong`, over chars in memory; positions index chars, not bytes. */
  @targetName("uLongMemChar")
  def uLong(mc: Mem[Char], i0: Long, iN: Long): ULong = ULong.wrap(uLongMemC(mc, i0, iN))

  /** As the String `uLong`, over all the chars in memory. */
  @targetName("uLongMemCharAll")
  inline def uLong(mc: Mem[Char]): ULong = uLong(mc, 0L, mc.length)

  /** The `Long` bit pattern of the bare hexadecimal (case-insensitive, no sign, no `0x`
    * prefix, at most 16 significant digits, leading zeros allowed) that occupies exactly
    * `[i0, iN)` of `s`, or [[failHex]] (see there) if that range is not one.
    */
  def hex(s: String, i0: Int, iN: Int): Long = hexStr(s, i0, iN)

  /** As the range `hex`, over the whole string. */
  inline def hex(s: String): Long = hex(s, 0, s.length)

  /** As the String `hex`, over ASCII bytes. */
  def hex(ab: Array[Byte], i0: Int, iN: Int): Long = hexArrB(ab, i0, iN)

  /** As the String `hex`, over a whole ASCII byte array. */
  inline def hex(ab: Array[Byte]): Long = hex(ab, 0, ab.length)

  /** As the String `hex`, over chars. */
  def hex(ac: Array[Char], i0: Int, iN: Int): Long = hexArrC(ac, i0, iN)

  /** As the String `hex`, over a whole char array. */
  inline def hex(ac: Array[Char]): Long = hex(ac, 0, ac.length)

  /** As the String `hex`, over ASCII bytes in memory. */
  @targetName("hexMemByte")
  def hex(mb: Mem[Byte], i0: Long, iN: Long): Long = hexMemB(mb, i0, iN)

  /** As the String `hex`, over all the ASCII bytes in memory. */
  @targetName("hexMemByteAll")
  inline def hex(mb: Mem[Byte]): Long = hex(mb, 0L, mb.length)

  /** As the String `hex`, over chars in memory; positions index chars, not bytes. */
  @targetName("hexMemChar")
  def hex(mc: Mem[Char], i0: Long, iN: Long): Long = hexMemC(mc, i0, iN)

  /** As the String `hex`, over all the chars in memory. */
  @targetName("hexMemCharAll")
  inline def hex(mc: Mem[Char]): Long = hex(mc, 0L, mc.length)

  /** Whether `[i0, iN)` of `s` spells [[failLong]], i.e. whether that value answered by
    * `long` on this input was a true parse rather than a failure.
    */
  def spellsFailLong(s: String, i0: Int, iN: Int): Boolean = sfLongStr(s, i0, iN)

  /** As the range `spellsFailLong`, over the whole string. */
  inline def spellsFailLong(s: String): Boolean = spellsFailLong(s, 0, s.length)

  /** As the String `spellsFailLong`, over ASCII bytes. */
  def spellsFailLong(ab: Array[Byte], i0: Int, iN: Int): Boolean = sfLongArrB(ab, i0, iN)

  /** As the String `spellsFailLong`, over a whole ASCII byte array. */
  inline def spellsFailLong(ab: Array[Byte]): Boolean = spellsFailLong(ab, 0, ab.length)

  /** As the String `spellsFailLong`, over chars. */
  def spellsFailLong(ac: Array[Char], i0: Int, iN: Int): Boolean = sfLongArrC(ac, i0, iN)

  /** As the String `spellsFailLong`, over a whole char array. */
  inline def spellsFailLong(ac: Array[Char]): Boolean = spellsFailLong(ac, 0, ac.length)

  /** As the String `spellsFailLong`, over ASCII bytes in memory. */
  @targetName("spellsFailLongMemByte")
  def spellsFailLong(mb: Mem[Byte], i0: Long, iN: Long): Boolean = sfLongMemB(mb, i0, iN)

  /** As the String `spellsFailLong`, over all the ASCII bytes in memory. */
  @targetName("spellsFailLongMemByteAll")
  inline def spellsFailLong(mb: Mem[Byte]): Boolean = spellsFailLong(mb, 0L, mb.length)

  /** As the String `spellsFailLong`, over chars in memory; positions index chars, not bytes. */
  @targetName("spellsFailLongMemChar")
  def spellsFailLong(mc: Mem[Char], i0: Long, iN: Long): Boolean = sfLongMemC(mc, i0, iN)

  /** As the String `spellsFailLong`, over all the chars in memory. */
  @targetName("spellsFailLongMemCharAll")
  inline def spellsFailLong(mc: Mem[Char]): Boolean = spellsFailLong(mc, 0L, mc.length)

  /** Whether `[i0, iN)` of `s` spells [[failULong]], i.e. whether that value answered by
    * `uLong` on this input was a true parse rather than a failure.
    */
  def spellsFailULong(s: String, i0: Int, iN: Int): Boolean = sfULongStr(s, i0, iN)

  /** As the range `spellsFailULong`, over the whole string. */
  inline def spellsFailULong(s: String): Boolean = spellsFailULong(s, 0, s.length)

  /** As the String `spellsFailULong`, over ASCII bytes. */
  def spellsFailULong(ab: Array[Byte], i0: Int, iN: Int): Boolean = sfULongArrB(ab, i0, iN)

  /** As the String `spellsFailULong`, over a whole ASCII byte array. */
  inline def spellsFailULong(ab: Array[Byte]): Boolean = spellsFailULong(ab, 0, ab.length)

  /** As the String `spellsFailULong`, over chars. */
  def spellsFailULong(ac: Array[Char], i0: Int, iN: Int): Boolean = sfULongArrC(ac, i0, iN)

  /** As the String `spellsFailULong`, over a whole char array. */
  inline def spellsFailULong(ac: Array[Char]): Boolean = spellsFailULong(ac, 0, ac.length)

  /** As the String `spellsFailULong`, over ASCII bytes in memory. */
  @targetName("spellsFailULongMemByte")
  def spellsFailULong(mb: Mem[Byte], i0: Long, iN: Long): Boolean = sfULongMemB(mb, i0, iN)

  /** As the String `spellsFailULong`, over all the ASCII bytes in memory. */
  @targetName("spellsFailULongMemByteAll")
  inline def spellsFailULong(mb: Mem[Byte]): Boolean = spellsFailULong(mb, 0L, mb.length)

  /** As the String `spellsFailULong`, over chars in memory; positions index chars, not bytes. */
  @targetName("spellsFailULongMemChar")
  def spellsFailULong(mc: Mem[Char], i0: Long, iN: Long): Boolean = sfULongMemC(mc, i0, iN)

  /** As the String `spellsFailULong`, over all the chars in memory. */
  @targetName("spellsFailULongMemCharAll")
  inline def spellsFailULong(mc: Mem[Char]): Boolean = spellsFailULong(mc, 0L, mc.length)

  /** Whether `[i0, iN)` of `s` spells [[failHex]], i.e. whether that value answered by
    * `hex` on this input was a true parse rather than a failure.
    */
  def spellsFailHex(s: String, i0: Int, iN: Int): Boolean = sfHexStr(s, i0, iN)

  /** As the range `spellsFailHex`, over the whole string. */
  inline def spellsFailHex(s: String): Boolean = spellsFailHex(s, 0, s.length)

  /** As the String `spellsFailHex`, over ASCII bytes. */
  def spellsFailHex(ab: Array[Byte], i0: Int, iN: Int): Boolean = sfHexArrB(ab, i0, iN)

  /** As the String `spellsFailHex`, over a whole ASCII byte array. */
  inline def spellsFailHex(ab: Array[Byte]): Boolean = spellsFailHex(ab, 0, ab.length)

  /** As the String `spellsFailHex`, over chars. */
  def spellsFailHex(ac: Array[Char], i0: Int, iN: Int): Boolean = sfHexArrC(ac, i0, iN)

  /** As the String `spellsFailHex`, over a whole char array. */
  inline def spellsFailHex(ac: Array[Char]): Boolean = spellsFailHex(ac, 0, ac.length)

  /** As the String `spellsFailHex`, over ASCII bytes in memory. */
  @targetName("spellsFailHexMemByte")
  def spellsFailHex(mb: Mem[Byte], i0: Long, iN: Long): Boolean = sfHexMemB(mb, i0, iN)

  /** As the String `spellsFailHex`, over all the ASCII bytes in memory. */
  @targetName("spellsFailHexMemByteAll")
  inline def spellsFailHex(mb: Mem[Byte]): Boolean = spellsFailHex(mb, 0L, mb.length)

  /** As the String `spellsFailHex`, over chars in memory; positions index chars, not bytes. */
  @targetName("spellsFailHexMemChar")
  def spellsFailHex(mc: Mem[Char], i0: Long, iN: Long): Boolean = sfHexMemC(mc, i0, iN)

  /** As the String `spellsFailHex`, over all the chars in memory. */
  @targetName("spellsFailHexMemCharAll")
  inline def spellsFailHex(mc: Mem[Char]): Boolean = spellsFailHex(mc, 0L, mc.length)
}
