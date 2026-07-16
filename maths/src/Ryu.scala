// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.maths


/** Shortest correctly-rounding decimal rendering of `Double`, after the Ryu algorithm
  * (Ulf Adams, "Ryu: Fast Float-to-String Conversion", PLDI 2018), reimplemented from the
  * algorithm's principles rather than ported; the correctness conditions are derived in the
  * comments, and the table precision is verified exactly (not sampled) in the test suite.
  *
  * The output is character-identical to `java.lang.Double.toString` on JDK 19+ (which renders
  * the same shortest decimal via a different route) except that the exponent letter is a
  * lowercase 'e' -- deliberately, since the x-height break reads as a separator where Java's
  * capital E does not; compare against the JDK case-insensitively.  The point of this version
  * is that it writes ASCII bytes straight into a caller-supplied buffer with no allocation at
  * all, which is what a serializer wants.  The longest possible rendering is 24 bytes.
  *
  * The shape of the algorithm: a finite double `v = m2 * 2^e2` is reproduced by parsing a
  * decimal `d` iff `d` lies within v's rounding interval, which stretches halfway to each
  * neighboring double (inclusive iff `m2` is even, by round-half-even).  Scaling by 4 makes
  * the interval ends integers times `2^(e2-2)`: `(4*m2 - 1 - mmShift, 4*m2, 4*m2 + 2)` for
  * lower / value / upper, where `mmShift` is 0 at a power-of-two boundary (the gap below is
  * half the gap above) and 1 otherwise.  One 64x128-bit multiply per endpoint converts all
  * three to a common decimal scale `10^e10` chosen so they land near 2^55; then a loop strips
  * trailing decimal digits while the shortened value still lies inside the interval, and the
  * last stripped digit rounds the result.  Divisibility flags track whether anything nonzero
  * was ever discarded, which is what decides exact-boundary hits and .5000... ties.
  */
object Ryu {

  /** Bits kept of each power-of-5 significand.  55-bit multiplicands (4*m2+2 < 2^55) leave
    * a 70-bit error margin, far more than the worst case over the reachable exponents; the
    * test suite verifies exactness per exponent with exact integer arithmetic.
    */
  private inline val B = 125

  /** Significands of 5^-q for the e2 >= 0 branch: with L = bitlength(5^q), entry q holds
    * `floor(2^(L-1+B) / 5^q) + 1` as (hi, lo) at (2q, 2q+1).  The +1 makes the error one-sided:
    * entry = (2^(L-1+B) + rho) / 5^q with 0 < rho <= 5^q, so a multiply-shift overshoots the
    * true `m * 2^s / 5^q` by at most m/2^j < 1, and floors match unless the true fraction is
    * within that sliver of 1 (impossible; proven exactly in the test suite).  q reaches 290
    * (from e2 = 969, the maximum).  Built at load; the cost is microseconds.
    */
  private[kse] val inv5: Array[Long] =
    val a = new Array[Long](2 * 291)
    val five = java.math.BigInteger.valueOf(5)
    var p5 = java.math.BigInteger.ONE
    var q = 0
    while q <= 290 do
      val c = java.math.BigInteger.ONE.shiftLeft(p5.bitLength - 1 + B).divide(p5).add(java.math.BigInteger.ONE)
      a(2*q) = c.shiftRight(64).longValue
      a(2*q + 1) = c.longValue
      p5 = p5.multiply(five)
      q += 1
    a

  /** Significands of 5^i for the e2 < 0 branch: entry i holds `ceil(5^i / 2^(L-B))` (exact
    * value shifted up when 5^i has fewer than B bits).  Rounding up rather than truncating
    * matters: it keeps the error on the same side as `inv5`, so an exactly-representable
    * quotient can never floor one too low.  i reaches 325 (from e2 = -1076, the minimum).
    */
  private[kse] val pow5: Array[Long] =
    val a = new Array[Long](2 * 326)
    val five = java.math.BigInteger.valueOf(5)
    var p5 = java.math.BigInteger.ONE
    var i = 0
    while i <= 325 do
      val l = p5.bitLength
      val c =
        if l <= B then p5.shiftLeft(B - l)
        else p5.subtract(java.math.BigInteger.ONE).shiftRight(l - B).add(java.math.BigInteger.ONE)
      a(2*i) = c.shiftRight(64).longValue
      a(2*i + 1) = c.longValue
      p5 = p5.multiply(five)
      i += 1
    a

  private val pairs: Array[Byte] =
    val a = new Array[Byte](200)
    var i = 0
    while i < 100 do
      a(2*i) = ('0' + i/10).toByte
      a(2*i + 1) = ('0' + i%10).toByte
      i += 1
    a

  // Fixed-point integer logs, exact over the ranges used here (verified exhaustively in the
  // test suite against integer powers): floor(e log10 2) for e <= 969, floor(e log10 5) for
  // e <= 1076, and bitlength(5^e) = floor(e log2 5) + 1 for e <= 325.
  private inline def log10pow2(e: Int): Int = (e * 78913) >>> 18
  private inline def log10pow5(e: Int): Int = (e * 732923) >>> 20
  private inline def pow5bits(e: Int): Int = ((e * 1217359) >>> 19) + 1

  /** floor(m * (hi:lo) / 2^j) for unsigned m < 2^56 and 64 < j < 128; the low 64 bits of the
    * 192-bit product never reach the answer (table precision absorbs them; see object docs).
    */
  private def mulShift(m: Long, hi: Long, lo: Long, j: Int): Long =
    val h0 = Math.unsignedMultiplyHigh(m, lo)
    val l1 = m * hi
    val h1 = Math.unsignedMultiplyHigh(m, hi)
    val mid = l1 + h0
    val top = h1 + (if java.lang.Long.compareUnsigned(mid, l1) < 0 then 1L else 0L)
    (top << (128 - j)) | (mid >>> (j - 64))

  private def mult5(x0: Long, q0: Int): Boolean =
    var x = x0
    var q = q0
    while q > 0 && x % 5 == 0 do
      x /= 5
      q -= 1
    q == 0

  private val pow10: Array[Long] =
    val a = new Array[Long](19)
    a(0) = 1L
    var i = 1
    while i < 19 do
      a(i) = a(i-1) * 10L
      i += 1
    a

  private def decLen(v: Long): Int =
    if      v >= 1000000000000000000L then 19
    else if v >= 100000000000000000L then 18
    else if v >= 10000000000000000L then 17
    else if v >= 1000000000000000L  then 16
    else if v >= 100000000000000L   then 15
    else if v >= 10000000000000L    then 14
    else if v >= 1000000000000L     then 13
    else if v >= 100000000000L      then 12
    else if v >= 10000000000L       then 11
    else if v >= 1000000000L        then 10
    else if v >= 100000000L         then 9
    else if v >= 10000000L          then 8
    else if v >= 1000000L           then 7
    else if v >= 100000L            then 6
    else if v >= 10000L             then 5
    else if v >= 1000L              then 4
    else if v >= 100L               then 3
    else if v >= 10L                then 2
    else 1

  /** The digit-stripping stage: shorten the interval-scaled triple while a shorter decimal
    * remains inside the interval, then round.  A multiple of 10 in (vm, vp] exists iff
    * vp/10 > vm/10; after that, if the lower end is exact, inclusive, and ends in zeros,
    * decimals exactly on it are reachable too.  The last digit stripped from vr rounds the
    * survivor: up from 5 unless it was an exact .5000... tie and vr is even (round half to
    * even), and forced up off a collision with an unattainable vm.  At most `cap` digits are
    * stripped (the redo path below needs that; a first pass never gets near a real cap).
    * Answers `(removed << 58) | result`; the result is at most 17 digits < 2^57.
    *
    * The result never ends in 0: a vr ending in 0 strictly above vm is itself the multiple of
    * 10 that keeps the strip loop going, rounding up to a multiple of 10 means that multiple
    * was in range and likewise keeps the loop going, and a vr that ends in 0 by colliding
    * with an exact attainable vm is stripped by the second loop.  (Only an artificial cap
    * can break this, which is why the redo path must special-case `10 * result`.)
    */
  private def strip(vr0: Long, vp0: Long, vm0: Long, vmTZ0: Boolean, vrTZ0: Boolean, even: Boolean, cap: Int): Long =
    var vr = vr0
    var vp = vp0
    var vm = vm0
    var vmTZ = vmTZ0
    var vrTZ = vrTZ0
    var removed = 0
    var lastRemoved = 0
    var out = 0L
    if vmTZ || vrTZ then
      var go = true
      while go && removed < cap do
        val vp1 = vp / 10
        val vm1 = vm / 10
        if vp1 > vm1 then
          val vr1 = vr / 10
          vmTZ = vmTZ && vm == vm1 * 10
          vrTZ = vrTZ && lastRemoved == 0
          lastRemoved = (vr - vr1 * 10).toInt
          vr = vr1; vp = vp1; vm = vm1
          removed += 1
        else go = false
      if vmTZ then
        go = true
        while go && removed < cap do
          val vm1 = vm / 10
          if vm == vm1 * 10 then
            val vr1 = vr / 10
            vrTZ = vrTZ && lastRemoved == 0
            lastRemoved = (vr - vr1 * 10).toInt
            vr = vr1; vp = vp / 10; vm = vm1
            removed += 1
          else go = false
      if vrTZ && lastRemoved == 5 && (vr & 1) == 0 then lastRemoved = 4
      out = vr + (if (vr == vm && !(even && vmTZ)) || lastRemoved >= 5 then 1 else 0)
    else
      // Nothing discarded so far and the bounds are unattainable (or off-scale): plain strip.
      // Four and two digits at a time first: a multiple of 10^k in (vm, vp] is exactly the
      // license to strip k more, so chunking is not an approximation, just fewer divisions
      // (short-decimal values strip a dozen digits; one at a time costs more than the rest
      // of the algorithm put together).  The rounding digit is the top of the last chunk.
      var up = false
      var go = true
      while go && removed + 4 <= cap do
        val vp1 = vp / 10000
        val vm1 = vm / 10000
        if vp1 > vm1 then
          val vr1 = vr / 10000
          up = vr - vr1 * 10000 >= 5000
          vr = vr1; vp = vp1; vm = vm1
          removed += 4
        else go = false
      if removed + 2 <= cap then
        val vp1 = vp / 100
        val vm1 = vm / 100
        if vp1 > vm1 then
          val vr1 = vr / 100
          up = vr - vr1 * 100 >= 50
          vr = vr1; vp = vp1; vm = vm1
          removed += 2
      go = true
      while go && removed < cap do
        val vp1 = vp / 10
        val vm1 = vm / 10
        if vp1 > vm1 then
          val vr1 = vr / 10
          up = vr - vr1 * 10 >= 5
          vr = vr1; vp = vp1; vm = vm1
          removed += 1
        else go = false
      out = vr + (if vr == vm || up then 1 else 0)
    (removed.toLong << 58) | out

  /** Digits of `v` written most-significant-first into `[p, p+t)`, where `t` is v's length. */
  private def digits(buf: Array[Byte], p: Int, t: Int, v0: Long): Unit =
    var v = v0
    var q = p + t
    while v >= 100 do
      val r = ((v % 100) << 1).toInt
      v /= 100
      q -= 2
      buf(q) = pairs(r)
      buf(q + 1) = pairs(r + 1)
    if v >= 10 then
      val r = (v << 1).toInt
      buf(q - 2) = pairs(r)
      buf(q - 1) = pairs(r + 1)
    else buf(q - 1) = ('0' + v).toByte

  /** Renders `d` as `java.lang.Double.toString` would (but with a lowercase exponent letter)
    * into `buf` starting at `at`, and answers the index just past the last byte written.  The
    * caller must supply at least 24 bytes of room; nothing is checked.
    */
  def append(buf: Array[Byte], at: Int, d: Double): Int =
    val bits = java.lang.Double.doubleToRawLongBits(d)
    val ieeeE = ((bits >>> 52) & 0x7FF).toInt
    val mant = bits & 0x000FFFFFFFFFFFFFL
    var p = at
    if ieeeE == 2047 then
      if mant != 0 then
        buf(p) = 'N'; buf(p+1) = 'a'; buf(p+2) = 'N'
        return p + 3
      if bits < 0 then { buf(p) = '-'; p += 1 }
      buf(p) = 'I'; buf(p+1) = 'n'; buf(p+2) = 'f'; buf(p+3) = 'i'
      buf(p+4) = 'n'; buf(p+5) = 'i'; buf(p+6) = 't'; buf(p+7) = 'y'
      return p + 8
    if bits < 0 then { buf(p) = '-'; p += 1 }
    if (bits & 0x7FFFFFFFFFFFFFFFL) == 0 then
      buf(p) = '0'; buf(p+1) = '.'; buf(p+2) = '0'
      return p + 3

    var m2 = 0L
    var e2 = 0
    if ieeeE == 0 then { m2 = mant; e2 = -1076 }
    else { m2 = mant | 0x0010000000000000L; e2 = ieeeE - 1077 }
    val even = (m2 & 1) == 0
    val mv = m2 << 2
    val mmShift = if mant != 0 || ieeeE <= 1 then 1 else 0
    val mm = mv - 1 - mmShift

    // Convert interval ends to scale 10^e10.  e10 is one less than the largest power of ten
    // certainly below the interval width, so (i) whenever any scaling happens (q > 0 below,
    // give or take the fixed factors of 2), the scaled width is at least 30 and the strip
    // loop always removes at least one digit, making lastRemoved meaningful; and (ii) when no
    // scaling happens (q == 0), the scaled ends are exact integers and nothing was discarded.
    var vr = 0L
    var vp = 0L
    var vm = 0L
    var e10 = 0
    var vmTZ = false   // the interval's lower end is exactly vm (and every digit stripped from it was 0)
    var vrTZ = false   // everything discarded below vr (and later, below lastRemoved) was exactly 0
    var thi = 0L       // the table entry and shift used, kept for the rare single-digit redo
    var tlo = 0L
    var tj = 0
    if e2 >= 0 then
      // v = mv * 2^(e2-2): strip q decimal digits via mv * 2^(e2-q) / 5^q (exact iff 5^q | mv).
      var q = log10pow2(e2) - 1
      if q < 0 then q = 0
      e10 = q
      val h = q << 1
      thi = inv5(h)
      tlo = inv5(h + 1)
      tj = pow5bits(q) - 1 + B - (e2 - q)
      vr = mulShift(mv, thi, tlo, tj)
      vp = mulShift(mv + 2, thi, tlo, tj)
      vm = mulShift(mm, thi, tlo, tj)
      // 5^24 > 2^55 > mp, so no divisibility is possible past q = 23; and mm, mv, mp span at
      // most 4 consecutive integers, so at most one of them is divisible by 5 at all.  When
      // the exact-but-excluded upper end is hit (odd m2, open interval), pulling vp down one
      // is what keeps the strip loop from reaching it.  (For q = 0 everything is exact, but
      // only the two flags reachable here can matter: a 10^r-tie needs 5^r | mv, and a
      // vm ending in decimal zeros needs 5 | mm, each caught by its own branch.)
      if q <= 23 then
        if mv % 5 == 0 then vrTZ = mult5(mv, q)
        else if even then vmTZ = mult5(mm, q)
        else if mult5(mv + 2, q) then vp -= 1
    else
      // v = mv * 2^(e2-2): scale to mv * 5^i / 2^q with i = -e2 - q (exact iff 2^q | mv).
      var q = log10pow5(-e2) - 1
      if q < 0 then q = 0
      e10 = e2 + q
      val i = -e2 - q
      val h = i << 1
      thi = pow5(h)
      tlo = pow5(h + 1)
      tj = q - pow5bits(i) + B
      vr = mulShift(mv, thi, tlo, tj)
      vp = mulShift(mv + 2, thi, tlo, tj)
      vm = mulShift(mm, thi, tlo, tj)
      if q <= 1 then
        // Dividing by at most 2: mv and mv+2 are even, so vr is exact and the excluded-upper
        // adjustment applies; mm is even iff mmShift is 1.
        vrTZ = true
        if even then vmTZ = mmShift == 1
        else vp -= 1
      else if q <= 54 then
        // mm and mp have exactly one factor of 2, so with q >= 2 only vr can be exact.  (mv
        // has at most 54 trailing zero bits, hence the cap.)  A near-miss -- 2^(q-1) | mv
        // only, an exact trailing decimal 5 one place below scale -- cannot masquerade as a
        // .5000... tie later: the digits stripped would have to end in 5-over-zeros, forcing
        // vr = (odd * 5^i - 1)/2 == 0 (mod 5), but that quantity is 2 mod 5.
        vrTZ = (mv & ((1L << q) - 1)) == 0

    finish(buf, p, mv, thi, tlo, tj, vr, vp, vm, vmTZ, vrTZ, even, e10, redo = true, pad = true)

  /** The shared back half of `append` and `fmt`: strip, round, and lay out the digits.
    * `redo` enables the Double.toString single-digit quirk below (value parity with toString
    * needs it; `fmt` wants the genuinely shortest answer instead); `pad` writes the Java-style
    * ".0" after integer-valued output and after a lone digit in scientific notation (`fmt`
    * omits both -- a precision-limited "86" should not grow a fake tenths digit).
    */
  private def finish(buf: Array[Byte], at: Int, mv: Long, thi: Long, tlo: Long, tj: Int,
                     vr: Long, vp: Long, vm: Long, vmTZ: Boolean, vrTZ: Boolean, even: Boolean,
                     e10: Int, redo: Boolean, pad: Boolean): Int =
    var p = at
    val packed = strip(vr, vp, vm, vmTZ, vrTZ, even, 99)
    var out = packed & 0x03FFFFFFFFFFFFFFL
    var removed = (packed >>> 58).toInt
    if redo && out < 10 then
      // java.lang.Double.toString quirk: when the minimal length is 1, two-digit decimals
      // also compete on closeness to v (so the smallest double prints as 4.9E-324, not
      // 5.0E-324).  The best two-digit candidate sits where the scaled value has two integer
      // digits: normally one strip back, but two when the single digit came from a round-up
      // carry across a decade (99ish -> 9.9ish -> 1), which is recognizable as the one-strip
      // redo answering exactly 10.  A capped redo that strips nothing has no rounding digit,
      // so its nearest value is recovered from one extra half-scale multiply instead; that
      // can only happen at the very bottom of the subnormals (the initial scaled value is
      // under 1000 only for mv <= 96), where the value is never an exact half (mv has at
      // most six factors of 2 against 2^751) and both round targets sit 20-deep inside the
      // interval, so plain round-half-up is exact and unclamped.  (The two-digit scale never
      // sits below the initial one: the initial scaled value is 40 or more, since mv >= 4
      // and the scale factor exceeds 10.)  A redo answer that is a multiple of ten is really
      // a one-digit decimal, and Java renders it in that canonical form.
      val half = mulShift(mv, thi, tlo, tj - 1)
      val rB0 = (half >>> 1) + (half & 1)
      val pk = strip(vr, vp, vm, vmTZ, vrTZ, even, removed - 1)
      var o2 = if (pk >>> 58) == 0L then rB0 else pk & 0x03FFFFFFFFFFFFFFL
      var rem2 = (pk >>> 58).toInt
      if o2 == 10 then
        val pk3 = strip(vr, vp, vm, vmTZ, vrTZ, even, removed - 2)
        val o3 = if (pk3 >>> 58) == 0L then rB0 else pk3 & 0x03FFFFFFFFFFFFFFL
        if o3 < 100 && o3 % 10 != 0 then
          o2 = o3
          rem2 = (pk3 >>> 58).toInt
      if o2 % 10 == 0 then
        out = o2 / 10
        removed = rem2 + 1
      else
        out = o2
        removed = rem2

    // Lay the digits out as java.lang.Double.toString does: plain decimal iff the value is
    // in [10^-3, 10^7), else d.ddd E x, always with at least one digit on each side of the
    // point, no exponent sign when positive, and no exponent leading zeros.
    val t = decLen(out)
    val k = e10 + removed
    val x = t + k - 1
    if x >= -3 && x < 7 then
      if k >= 0 then
        digits(buf, p, t, out)
        var q = p + t
        val qN = q + k
        while q < qN do
          buf(q) = '0'
          q += 1
        if pad then
          buf(qN) = '.'
          buf(qN + 1) = '0'
          p = qN + 2
        else p = qN
      else if x >= 0 then
        digits(buf, p + 1, t, out)
        System.arraycopy(buf, p + 1, buf, p, x + 1)
        buf(p + x + 1) = '.'
        p = p + t + 1
      else
        buf(p) = '0'
        buf(p + 1) = '.'
        var q = p + 2
        val qN = q - x - 1
        while q < qN do
          buf(q) = '0'
          q += 1
        digits(buf, qN, t, out)
        p = qN + t
    else
      digits(buf, p + 1, t, out)
      buf(p) = buf(p + 1)
      if t == 1 then
        if pad then
          buf(p + 1) = '.'
          buf(p + 2) = '0'
          p += 3
        else p += 1
      else
        buf(p + 1) = '.'
        p += t + 1
      buf(p) = 'e'   // always lowercase: the x-height break is a natural separator; capital E is not
      p += 1
      var xm = x
      if xm < 0 then
        buf(p) = '-'
        p += 1
        xm = -xm
      if xm >= 100 then
        buf(p) = ('0' + xm / 100).toByte
        val r = (xm % 100) << 1
        buf(p + 1) = pairs(r)
        buf(p + 2) = pairs(r + 1)
        p += 3
      else if xm >= 10 then
        val r = xm << 1
        buf(p) = pairs(r)
        buf(p + 1) = pairs(r + 1)
        p += 2
      else
        buf(p) = ('0' + xm).toByte
        p += 1
    p

  /** The string `java.lang.Double.toString` would produce (lowercase exponent), via `append`. */
  def string(d: Double): String =
    val b = new Array[Byte](24)
    new String(b, 0, append(b, 0, d), java.nio.charset.StandardCharsets.ISO_8859_1)

  /** Renders `d` with limited precision: the shortest decimal within the widest of the
    * round-trip interval and the don't-care interval `d +- 10^p / 2`, where `p` is the last
    * decimal place that matters.  This is deliberately NOT rounding: any equally short decimal
    * inside the tolerance is fair game (86.421 to the tens gives "90"; "86" would have been
    * just as acceptable), and the exact toString-style tie rules are not promised.
    *
    * `mag` names the last place kept by absolute position: 1 is the ones digit, 2 the tens,
    * 3 the hundreds; -1 the tenths, -6 the millionths (so the place is `10^(mag-1)` above the
    * decimal point and `10^mag` below); 0 means no absolute limit.  `sig` counts significant
    * figures: positive is an upper bound (combined with `mag`, the coarser cutoff wins),
    * negative is a floor correcting `mag` to keep at least `-sig` leading digits (so
    * `fmt(86.421, 2, -3)` answers "86.4" where `mag` alone would say "90"), and 0 means no
    * constraint (a negative `sig` with `mag` 0 has nothing to correct and is also a no-op).
    *
    * Layout is Java's except that no cosmetic ".0" is added: "86", "90", "1200", "9e8".
    * A value entirely inside the don't-care region prints as an unsigned "0"; NaN and the
    * infinities print as in `append`; the zeros print as "0" and "-0".  With `mag` and `sig`
    * both 0 the output parses back bit-identically to `d` (it is `append`'s rendering minus
    * the ".0").  The caller must supply at least 24 bytes of room; nothing is checked.
    */
  def fmt(buf: Array[Byte], at: Int, d: Double, mag: Int, sig: Int): Int =
    val bits = java.lang.Double.doubleToRawLongBits(d)
    val ieeeE = ((bits >>> 52) & 0x7FF).toInt
    val mant = bits & 0x000FFFFFFFFFFFFFL
    var p = at
    if ieeeE == 2047 then
      if mant != 0 then
        buf(p) = 'N'; buf(p+1) = 'a'; buf(p+2) = 'N'
        return p + 3
      if bits < 0 then { buf(p) = '-'; p += 1 }
      buf(p) = 'I'; buf(p+1) = 'n'; buf(p+2) = 'f'; buf(p+3) = 'i'
      buf(p+4) = 'n'; buf(p+5) = 'i'; buf(p+6) = 't'; buf(p+7) = 'y'
      return p + 8
    if (bits & 0x7FFFFFFFFFFFFFFFL) == 0 then
      if bits < 0 then { buf(p) = '-'; p += 1 }
      buf(p) = '0'
      return p + 1

    var m2 = 0L
    var e2 = 0
    if ieeeE == 0 then { m2 = mant; e2 = -1076 }
    else { m2 = mant | 0x0010000000000000L; e2 = ieeeE - 1077 }
    val even = (m2 & 1) == 0
    val mv = m2 << 2
    val mmShift = if mant != 0 || ieeeE <= 1 then 1 else 0
    val mm = mv - 1 - mmShift

    var vr = 0L
    var vp = 0L
    var vm = 0L
    var e10 = 0
    var vmTZ = false
    var vrTZ = false
    var thi = 0L
    var tlo = 0L
    var tj = 0
    if e2 >= 0 then
      var q = log10pow2(e2) - 1
      if q < 0 then q = 0
      e10 = q
      val h = q << 1
      thi = inv5(h)
      tlo = inv5(h + 1)
      tj = pow5bits(q) - 1 + B - (e2 - q)
      vr = mulShift(mv, thi, tlo, tj)
      vp = mulShift(mv + 2, thi, tlo, tj)
      vm = mulShift(mm, thi, tlo, tj)
      if q <= 23 then
        if mv % 5 == 0 then vrTZ = mult5(mv, q)
        else if even then vmTZ = mult5(mm, q)
        else if mult5(mv + 2, q) then vp -= 1
    else
      var q = log10pow5(-e2) - 1
      if q < 0 then q = 0
      e10 = e2 + q
      val i = -e2 - q
      val h = i << 1
      thi = pow5(h)
      tlo = pow5(h + 1)
      tj = q - pow5bits(i) + B
      vr = mulShift(mv, thi, tlo, tj)
      vp = mulShift(mv + 2, thi, tlo, tj)
      vm = mulShift(mm, thi, tlo, tj)
      if q <= 1 then
        vrTZ = true
        if even then vmTZ = mmShift == 1
        else vp -= 1
      else if q <= 54 then
        vrTZ = (mv & ((1L << q) - 1)) == 0

    // The last place that matters, from mag and sig; Int.MinValue marks no limit at all.
    var cut = Int.MinValue
    if mag != 0 then cut = if mag > 0 then mag - 1 else mag
    if sig > 0 then
      val ps = e10 + decLen(vr) - 1 - sig + 1   // lead digit place - (sig - 1)
      if ps > cut then cut = ps
    else if sig < 0 && cut != Int.MinValue then
      val pf = e10 + decLen(vr) - 1 + sig + 1
      if pf < cut then cut = pf
    if cut != Int.MinValue then
      // Widen the interval to d +- 10^cut / 2 (in scaled units, 5*10^(cut-e10-1)) wherever
      // that is wider than round-trip; if it swallows zero, zero is the shortest answer.
      val k = cut - e10 - 1
      if k >= 0 then
        val hu = if k > 18 then Long.MaxValue else 5L * pow10(k)
        if hu >= vr then
          buf(p) = '0'
          return p + 1
        if vr - hu < vm then
          vm = vr - hu
          vmTZ = false
        if vr + hu > vp then vp = vr + hu

    if bits < 0 then { buf(p) = '-'; p += 1 }
    finish(buf, p, mv, thi, tlo, tj, vr, vp, vm, vmTZ, vrTZ, even, e10, redo = false, pad = false)

  /** As the byte-buffer `fmt`, but into a char array (at least 24 chars of room). */
  def fmt(buf: Array[Char], at: Int, d: Double, mag: Int, sig: Int): Int =
    val b = new Array[Byte](24)
    val n = fmt(b, 0, d, mag, sig)
    var i = 0
    while i < n do
      buf(at + i) = (b(i) & 0xFF).toChar
      i += 1
    at + n

  /** As the byte-buffer `fmt`, but answering a String. */
  def fmt(d: Double, mag: Int, sig: Int): String =
    val b = new Array[Byte](24)
    new String(b, 0, fmt(b, 0, d, mag, sig), java.nio.charset.StandardCharsets.ISO_8859_1)
}
