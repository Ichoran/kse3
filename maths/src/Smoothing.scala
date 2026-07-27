// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.maths


import java.lang.{Math => jm}


/** Data-in, data-out smoothing kernels: LOESS (Cleveland-style, with optional robustness
  * iterations), kernel regression, rolling mean/median, and global polynomial fits.
  *
  * Everything here is dependency-free by design: local fits of degree at most two use
  * closed-form solves, and the global polynomial fit uses a small pivoted elimination on a
  * centered basis.  Methods that would need real linear-algebra machinery (GAMs, Gaussian
  * processes, sparse solvers) do not belong in maths; they go in a dependency-permitted
  * analytics module when they are wanted.
  *
  * Where sortedness is required it is checked (cheap, and the failure mode otherwise is
  * silently wrong output).  Evaluation points are the caller's choice: smoothing for
  * display usually evaluates on an even grid; rolling methods evaluate at the data.
  */
object Smoothing {

  enum Shape {
    case Gaussian, Epanechnikov, Tricube
  }

  private def shapeWeight(shape: Shape, u: Double): Double = shape match
    case Shape.Gaussian     => jm.exp(-0.5 * u * u)
    case Shape.Epanechnikov => if u <= -1 || u >= 1 then 0.0 else 0.75 * (1 - u * u)
    case Shape.Tricube      => tricube(u)

  private def tricube(u: Double): Double =
    val a = jm.abs(u)
    if a >= 1 then 0.0
    else
      val t = 1 - a * a * a
      t * t * t

  private def checkAscending(x: Array[Double], name: String): Unit =
    var i = 1
    while i < x.length do
      if x(i) < x(i - 1) then throw new IllegalArgumentException(s"$name must be ascending (violated at index $i)")
      i += 1

  private def checkPaired(x: Array[Double], y: Array[Double]): Unit =
    if x.length != y.length then throw new IllegalArgumentException(s"x and y differ in length: ${x.length} vs ${y.length}")
    if x.length == 0 then throw new IllegalArgumentException("no data")

  /** Weighted polynomial fit over `[lo, hi)` on a basis centered at `x0`, returning the
    * fitted value at `x0` (the constant term).  Degenerate systems fall back a degree.
    */
  private def localFit(x: Array[Double], y: Array[Double], w: Array[Double], lo: Int, hi: Int, x0: Double, degree: Int): Double =
    var s0 = 0.0; var s1 = 0.0; var s2 = 0.0; var s3 = 0.0; var s4 = 0.0
    var t0 = 0.0; var t1 = 0.0; var t2 = 0.0
    var i = lo
    while i < hi do
      val wi = w(i)
      if wi > 0 then
        val t = x(i) - x0
        val wt = wi * t
        val wtt = wt * t
        s0 += wi; s1 += wt; s2 += wtt; s3 += wtt * t; s4 += wtt * t * t
        t0 += wi * y(i); t1 += wt * y(i); t2 += wtt * y(i)
      i += 1
    if s0 <= 0 then
      // all weights vanished (e.g. two points both exactly at the window edge): plain mean
      var s = 0.0
      i = lo
      while i < hi do
        s += y(i)
        i += 1
      return s / jm.max(1, hi - lo)
    if degree >= 2 then
      // symmetric 3x3 via Cramer; tiny determinant means x-values don't support a quadratic
      val det = s0*(s2*s4 - s3*s3) - s1*(s1*s4 - s2*s3) + s2*(s1*s3 - s2*s2)
      val scale = jm.max(s0, jm.max(s2, s4))
      if jm.abs(det) > 1e-12 * scale * scale * scale then
        return (t0*(s2*s4 - s3*s3) - s1*(t1*s4 - s3*t2) + s2*(t1*s3 - s2*t2)) / det
    if degree >= 1 then
      val det = s0 * s2 - s1 * s1
      if jm.abs(det) > 1e-12 * jm.max(s0, s2) * jm.max(s0, s2) then
        return (s2 * t0 - s1 * t1) / det
    t0 / s0

  private def medianOfAbs(r: Array[Double]): Double =
    val a = new Array[Double](r.length)
    var i = 0
    while i < a.length do
      a(i) = jm.abs(r(i))
      i += 1
    java.util.Arrays.sort(a)
    if a.length % 2 == 1 then a(a.length / 2)
    else (a(a.length / 2 - 1) + a(a.length / 2)) / 2

  private def loessPass(x: Array[Double], y: Array[Double], evalX: Array[Double], q: Int, degree: Int, rw: Array[Double]): Array[Double] =
    val n = x.length
    val out = new Array[Double](evalX.length)
    val w = new Array[Double](n)
    var lo = 0
    var e = 0
    while e < evalX.length do
      val x0 = evalX(e)
      while lo + q < n && x0 - x(lo) > x(lo + q) - x0 do lo += 1
      val hi = lo + q
      val h = jm.max(x(hi - 1) - x0, x0 - x(lo))
      var i = lo
      while i < hi do
        w(i) = (if h > 0 then tricube((x(i) - x0) / h) else 1.0) * rw(i)
        i += 1
      out(e) = localFit(x, y, w, lo, hi, x0, degree)
      e += 1
    out

  /** Cleveland-style LOESS evaluated at `evalX` (both `x` and `evalX` ascending).
    * `span` is the fraction of points in each local window; `degree` 0, 1, or 2;
    * `robustIters` > 0 downweights outliers by bisquare of scaled residuals.
    */
  def loessAt(x: Array[Double], y: Array[Double], evalX: Array[Double], span: Double = 0.75, degree: Int = 1, robustIters: Int = 0): Array[Double] =
    checkPaired(x, y)
    if !(span > 0) then throw new IllegalArgumentException(s"span must be positive, got $span")
    if degree < 0 || degree > 2 then throw new IllegalArgumentException(s"degree must be 0 to 2, got $degree")
    checkAscending(x, "x")
    checkAscending(evalX, "evalX")
    val n = x.length
    if n == 1 then return Array.fill(evalX.length)(y(0))
    val q = jm.max(degree + 1, jm.min(n, jm.ceil(span * n).toInt))
    val rw = Array.fill(n)(1.0)
    var iter = 0
    var live = true
    while live && iter < robustIters do
      val fitted = loessPass(x, y, x, q, degree, rw)
      val r = new Array[Double](n)
      var i = 0
      while i < n do
        r(i) = y(i) - fitted(i)
        i += 1
      val s = medianOfAbs(r)
      if s <= 1e-300 then live = false
      else
        i = 0
        while i < n do
          val u = r(i) / (6 * s)
          rw(i) = if u <= -1 || u >= 1 then 0.0 else { val t = 1 - u * u; t * t }
          i += 1
      iter += 1
    loessPass(x, y, evalX, q, degree, rw)

  /** Kernel regression evaluated at `evalX`: every point contributes with weight
    * `shape((xi - x0)/bandwidth)`.  `degree` 0 is Nadaraya–Watson; 1 is local-linear
    * (reproduces straight-line data exactly, and behaves better at the edges).
    * Neither `x` nor `evalX` needs to be sorted.
    */
  def kernelAt(x: Array[Double], y: Array[Double], evalX: Array[Double], bandwidth: Double, shape: Shape = Shape.Gaussian, degree: Int = 1): Array[Double] =
    checkPaired(x, y)
    if !(bandwidth > 0) then throw new IllegalArgumentException(s"bandwidth must be positive, got $bandwidth")
    if degree < 0 || degree > 2 then throw new IllegalArgumentException(s"degree must be 0 to 2, got $degree")
    val n = x.length
    val w = new Array[Double](n)
    val out = new Array[Double](evalX.length)
    var e = 0
    while e < evalX.length do
      val x0 = evalX(e)
      var wsum = 0.0
      var i = 0
      while i < n do
        w(i) = shapeWeight(shape, (x(i) - x0) / bandwidth)
        wsum += w(i)
        i += 1
      if wsum <= 1e-300 then
        // compact kernels can strand a far-away eval point: use the nearest datum
        var best = 0
        i = 1
        while i < n do
          if jm.abs(x(i) - x0) < jm.abs(x(best) - x0) then best = i
          i += 1
        out(e) = y(best)
      else out(e) = localFit(x, y, w, 0, n, x0, degree)
      e += 1
    out

  /** Centered rolling mean over a count window (order is the caller's, typically by x);
    * windows clip at the edges.
    */
  def rollingMean(y: Array[Double], window: Int): Array[Double] =
    if window < 1 then throw new IllegalArgumentException(s"window must be at least 1, got $window")
    val n = y.length
    val left = (window - 1) / 2
    val right = window / 2
    val cum = new Array[Double](n + 1)
    var i = 0
    while i < n do
      cum(i + 1) = cum(i) + y(i)
      i += 1
    val out = new Array[Double](n)
    i = 0
    while i < n do
      val a = jm.max(0, i - left)
      val b = jm.min(n, i + right + 1)
      out(i) = (cum(b) - cum(a)) / (b - a)
      i += 1
    out

  /** Centered rolling median over a count window; robust to spikes. */
  def rollingMedian(y: Array[Double], window: Int): Array[Double] =
    if window < 1 then throw new IllegalArgumentException(s"window must be at least 1, got $window")
    val n = y.length
    val left = (window - 1) / 2
    val right = window / 2
    val out = new Array[Double](n)
    val buf = new Array[Double](window)
    var i = 0
    while i < n do
      val a = jm.max(0, i - left)
      val b = jm.min(n, i + right + 1)
      var j = a
      while j < b do
        buf(j - a) = y(j)
        j += 1
      java.util.Arrays.sort(buf, 0, b - a)
      val k = b - a
      out(i) = if k % 2 == 1 then buf(k / 2) else (buf(k / 2 - 1) + buf(k / 2)) / 2
      i += 1
    out

  /** Global least-squares polynomial fit evaluated at `evalX`.  Solved on a centered,
    * scaled basis with pivoted elimination; a rank-deficient system (e.g. duplicate x)
    * falls back to a lower degree.  Neither array needs to be sorted.
    */
  def polyFitAt(x: Array[Double], y: Array[Double], evalX: Array[Double], degree: Int = 1): Array[Double] =
    checkPaired(x, y)
    if degree < 0 || degree > 8 then throw new IllegalArgumentException(s"degree must be 0 to 8, got $degree")
    val n = x.length
    var mx = 0.0
    var i = 0
    while i < n do
      mx += x(i)
      i += 1
    mx /= n
    var sx = 0.0
    i = 0
    while i < n do
      if jm.abs(x(i) - mx) > sx then sx = jm.abs(x(i) - mx)
      i += 1
    if sx <= 0 then
      var my = 0.0
      i = 0
      while i < n do
        my += y(i)
        i += 1
      return Array.fill(evalX.length)(my / n)
    var d = jm.min(degree, n - 1)
    var coef: Array[Double] | Null = null
    while coef == null do
      coef = solveNormal(x, y, mx, sx, d)
      if coef == null then d -= 1  // d = 0 always solves, so this terminates
    val cs = coef.asInstanceOf[Array[Double]]
    val out = new Array[Double](evalX.length)
    var e = 0
    while e < evalX.length do
      val t = (evalX(e) - mx) / sx
      var v = 0.0
      var k = cs.length - 1
      while k >= 0 do
        v = v * t + cs(k)
        k -= 1
      out(e) = v
      e += 1
    out

  /** Builds and solves the (d+1)-square normal equations on the centered basis; null if
    * the pivoting finds the system rank-deficient at this degree.
    */
  private def solveNormal(x: Array[Double], y: Array[Double], mx: Double, sx: Double, d: Int): Array[Double] | Null =
    val m = d + 1
    val pow = new Array[Double](2 * d + 1)
    val rhs = new Array[Double](m)
    var i = 0
    while i < x.length do
      val t = (x(i) - mx) / sx
      var p = 1.0
      var k = 0
      while k < pow.length do
        pow(k) += p
        if k < m then rhs(k) += p * y(i)
        p *= t
        k += 1
      i += 1
    val a = Array.ofDim[Double](m, m + 1)
    var r = 0
    while r < m do
      var c = 0
      while c < m do
        a(r)(c) = pow(r + c)
        c += 1
      a(r)(m) = rhs(r)
      r += 1
    // Gaussian elimination with partial pivoting
    var col = 0
    while col < m do
      var piv = col
      r = col + 1
      while r < m do
        if jm.abs(a(r)(col)) > jm.abs(a(piv)(col)) then piv = r
        r += 1
      if jm.abs(a(piv)(col)) <= 1e-12 * x.length then return null
      if piv != col then
        val tmp = a(piv)
        a(piv) = a(col)
        a(col) = tmp
      r = col + 1
      while r < m do
        val f = a(r)(col) / a(col)(col)
        var c = col
        while c <= m do
          a(r)(c) -= f * a(col)(c)
          c += 1
        r += 1
      col += 1
    val cs = new Array[Double](m)
    r = m - 1
    while r >= 0 do
      var v = a(r)(m)
      var c = r + 1
      while c < m do
        v -= a(r)(c) * cs(c)
        c += 1
      cs(r) = v / a(r)(r)
      r -= 1
    cs
}
