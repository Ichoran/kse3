// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.maths


import java.lang.{Math => jm}


/** Analytic real-root finding for polynomials up to quartic order.
  *
  * Coefficients are given in ascending order of power: `a0 + a1*x + a2*x^2 + ...`.  Real
  * roots are written into a caller-supplied array starting at `offset`, sorted ascending,
  * with multiple roots reported once; the return value is how many were written.  Degenerate
  * polynomials (leading zeros) fall through to the lower-order solvers; constant polynomials
  * and non-finite coefficients yield zero roots.
  *
  * The solutions are the classic closed forms (quadratic formula, trigonometric/Cardano
  * cubic, Ferrari quartic via a resolvent cubic), arranged for numerical stability:
  * quadratics avoid subtractive cancellation, discriminants that are zero to within
  * rounding error are treated as exact double roots, the cubic's three-real-root branch
  * uses a fast series for `cos(acos(t)/3)`, and cubic and quartic roots get a guarded
  * Newton polish against the original coefficients.
  */
object Roots {
  // Chebyshev-like series for cos(acos(t)/3) about various centers t0; each row is
  // t0, f(t0), f'(t0), f''(t0)/2, ... with intervals refined toward the square-root
  // singularity at t = -1.  Coefficients computed with Mathematica (see Choreography's
  // Fitter.java); the empty final row marks the reflection regime handled in code.
  private val trisectionCoeff: Array[Array[Double]] = Array(
    Array(1.0, 1.00000000000000000, 0.111111111111111111, -0.0164609053497942387, 0.00426764212772443225, -0.00135480702467442294, 0.000478363961798623407, -0.000180393143843251925, 0.0000711440603923936162, -0.0000289846171969011029, 0.0000121032352129398215),
    Array(0.8, 0.977082867122276646, 0.118255152546255299, -0.0193897791341417425, 0.00557627375386415221, -0.00196514157092676306, 0.000770514808800875661, -0.000322720643940775764, 0.000141376154987797655, -0.0000639831968963048284, 0.0000296811817396416175),
    Array(0.6, 0.952608221822056414, 0.126749955975378451, -0.0232776440585948592, 0.00751747628196047947, -0.00297745895850725166, 0.00131255475720956516, -0.000618205006160952861, 0.000304580032321341748, -0.000155039648770090648, 0.0000808968022240828250),
    Array(0.4, 0.926261757195517814, 0.137070231272876853, -0.0286249288765318218, 0.0105437254508361435, -0.00476754118533357597, 0.00240032992450573429, -0.00129146799705807940, 0.000726949820446853157, -0.000422797151056714268, 0.000252075161192156866),
    Array(0.2, 0.897609874622561880, 0.149960069137274354, -0.0363241753573766228, 0.0155744494451876410, -0.00820636576473876825, 0.00481686991361213750, -0.00302215988177106760, 0.00198397890954231693, -0.00134586205445813352, 0.000935964050340249824, -0.000663716797646247392),
    Array(0.0, 0.866025403784438647, 0.166666666666666667, -0.0481125224324688137, 0.0246913580246913580, -0.0155920211586704489, 0.0109739368998628258, -0.00825799639144397848, 0.00650307371843723010, -0.00529232705245318462, 0.00441566733967960068, -0.00375689883353158168, 0.00324707658917853464),
    Array(-0.2, 0.830540951770293644, 0.189480810933704582, -0.0678012969589677020, 0.0433661360846158919, -0.0341814339105757972, 0.0300464997445734463, -0.0282473438600416210, 0.0277948643996841836, -0.0282669701873795322, 0.0294743536142216752, -0.0313409784504885293, 0.0338551599970753962, -0.0370480106216445160),
    Array(-0.4, 0.789519248286085428, 0.223209915136065471, -0.105362099389941873, 0.0895392563472501813, -0.0939461344951441286, 0.110006024238694065, -0.137809846382160710, 0.180728675138160920, -0.244990774652067807, 0.340528791355239743, -0.482705693273669958, 0.695136669439399693, -1.01413747851414350, 1.49568892874247036, -2.22633879971546758),
    Array(-11.0/20, 0.753307047010601533, 0.262490747912781887, -0.163491537170758413, 0.184670772262678347, -0.257984981302009979, 0.402472163994208291, -0.671936953967329478, 1.17454843487068024, -2.12240706212364264, 3.93272070942197228, -7.43188652595570640, 14.2684333718844903),
    Array(-13.0/20, 0.725207747386557441, 0.302013038695660697, -0.239729102814446611, 0.347301453209952272, -0.623155701742487466, 1.24922578357109792, -2.68058331168361222, 6.02304126141674620, -13.9909105517385130, 33.3273405011592230, -80.9671523335197637, 199.846593560804813, -499.733502640004448),
    Array(-15.0/20, 0.692183576319070784, 0.363713439155063518, -0.399650703570253908, 0.808277714617387997, -2.02806123238030522, 5.68845447732404731, -17.0825888145402160, 53.7233990615162681, -174.681515014207970, 582.470274245416627, -1980.91865075167155, 6844.59804730662526, -23960.1450164282985, 84793.9321792031395, -302871.203927300239, 1.09044614208059904e6),
    Array(-33.0/40, 0.662234381771498811, 0.441959157481586422, -0.686024114124336247, 1.97712821883550750, -7.08019132759748977, 28.3564607004019782, -121.615653157854629, 546.285200939307338, -2537.15471757620309, 12084.6094164127090, -58707.5736266785127, 289768.305834027679, -1.44901450947640108e6),
    Array(-35.0/40, 0.638118652632768511, 0.530125725481802257, -1.14082577559750193, 4.59417471216485013, -23.0169200995707848, 129.013685544972943, -774.489195286730414, 4869.86751110944138, -31661.6195998003585, 211114.220422712711, -1.43576850753159471e6, 9.92090411038118118e6, -6.94522849043971084e7, 4.91543353957992297e8, -3.51123348600714573e9),
    Array(-73.0/80, 0.616334469439248000, 0.641676309594736291, -1.95409520887594806, 11.2234549248256990, -80.2839954829407743, 642.695089707672148, -5510.87021074998638, 49497.2211276811750, -459693.691670216836, 4.37857233172808068e6, -4.25387031088488545e7, 4.19893754476790005e8, -4.19919072224060702e9),
    Array(-75.0/80, 0.598856508681471799, 0.767136241876516416, -3.24430385067335527, 26.0557185950357168, -260.834601248862153, 2922.74154128270869, -35082.3310508001947, 441111.009177794112, -5.73512978455682138e6, 7.64751286455055172e7, -1.04013188447046704e9, 1.43734976266564108e10, -2.01237658526557345e11, 2.84838856847141309e12, -4.06924865812291993e13),
    Array(-153.0/160, 0.583119362323381580, 0.925635988796204458, -5.54957088916928685, 63.6075105255179129, -909.370473069236436, 14554.8411559953788, -249558.869761503194, 4.48241695028402534e6, -8.32518923994283756e7, 1.58584837560858283e9, -3.08122269641655707e10, 6.08264115139567357e11, -1.21656509300474784e13),
    Array(-155.0/160, 0.570529684398212177, 1.10369251999730579, -9.20457602765407001, 147.593232737563537, -2953.49466975979287, 66174.3605953345032, -1.58840077861633486e6, 3.99404044735883737e7, -1.03851311627887494e9, 2.76949088990827805e10, -7.53327042141283449e11, 2.08197772527163537e13, -5.82966399020313920e14, 1.65027290571124047e16, -4.71513641437779439e17),
    new Array[Double](0)
  )

  /** Computes `cos(acos(t)/3)` for `t` in [-1, 1] by piecewise series expansion,
    * considerably faster than the transcendental route.  Used by the three-real-root
    * branch of the cubic solver, where the argument is the normalized half-discriminant.
    */
  def trisectCosine(t: Double): Double =
    val series =
      if t > -0.1 then
        if t > 0.5 then
          if t > 0.9 then trisectionCoeff(0)
          else if t > 0.7 then trisectionCoeff(1)
          else trisectionCoeff(2)
        else
          if t > 0.3 then trisectionCoeff(3)
          else if t > 0.1 then trisectionCoeff(4)
          else trisectionCoeff(5)
      else
        if t > -0.5 then
          if t > -0.3 then trisectionCoeff(6)
          else trisectionCoeff(7)
        else if t > -0.8 then
          if t > -0.6 then trisectionCoeff(8)
          else if t > -0.7 then trisectionCoeff(9)
          else trisectionCoeff(10)
        else if t > -0.9 then
          if t > -0.85 then trisectionCoeff(11)
          else trisectionCoeff(12)
        else if t > -0.95 then
          if t > -0.925 then trisectionCoeff(13)
          else trisectionCoeff(14)
        else if t > -0.975 then
          if t > -0.9625 then trisectionCoeff(15)
          else trisectionCoeff(16)
        else trisectionCoeff(17)
    if series.length == 0 then
      // Too close to the singularity at -1; use cos((pi - acos(-t))/3) = cos(pi/3 - acos(-t)/3)
      val s = trisectCosine(-t)
      0.5*s + 0.866025403784438647*jm.sqrt(1 - s*s)
    else
      var sum = series(1)
      val dt = t - series(0)
      var dtn = dt
      var i = 2
      while i < series.length do
        sum += series(i)*dtn
        dtn *= dt
        i += 1
      sum

  // A difference that is zero to within accumulated rounding error is treated as exactly
  // zero, so that analytically-degenerate cases (e.g. double roots) are recognized.
  private def snapDiff(a: Double, b: Double): Double =
    val d = a - b
    if jm.abs(d) < 100.0 * jm.ulp(jm.max(jm.abs(a), jm.abs(b))) then 0.0 else d

  // Insert an exact root at zero into an ascending run of n roots (no-op if already there).
  private def insertZero(result: Array[Double], offset: Int, n: Int): Int =
    var i = 0
    while i < n && result(offset + i) < 0 do i += 1
    if i < n && result(offset + i) == 0 then n
    else
      var j = n
      while j > i do
        result(offset + j) = result(offset + j - 1)
        j -= 1
      result(offset + i) = 0.0
      n + 1

  // Ascending insertion sort followed by removal of exactly-equal neighbors; returns new count.
  private def sortDistinct(result: Array[Double], offset: Int, n: Int): Int =
    var i = 1
    while i < n do
      val x = result(offset + i)
      var j = i - 1
      while j >= 0 && result(offset + j) > x do
        result(offset + j + 1) = result(offset + j)
        j -= 1
      result(offset + j + 1) = x
      i += 1
    var k = 0
    i = 1
    while i < n do
      if result(offset + i) != result(offset + k) then
        k += 1
        result(offset + k) = result(offset + i)
      i += 1
    if n > 0 then k + 1 else 0

  // Guarded Newton polish against the original quartic (a4 may be zero for a cubic):
  // steps are only taken while they reduce |f|, so multiple roots and flat spots are safe.
  private def polish(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, x0: Double): Double =
    var x = x0
    var f = (((a4*x + a3)*x + a2)*x + a1)*x + a0
    var k = 0
    while k < 2 && f != 0 do
      val fp = ((4*a4*x + 3*a3)*x + 2*a2)*x + a1
      var y = Double.NaN
      if fp != 0 then y = x - f/fp
      if !y.finite then k = 2
      else
        val g = (((a4*y + a3)*y + a2)*y + a1)*y + a0
        if jm.abs(g) < jm.abs(f) then
          x = y
          f = g
          k += 1
        else k = 2
    x

  /** Real root of `a0 + a1*x`, written at `result(offset)`.  Returns the number of roots
    * written: 1, or 0 if the polynomial is constant or the root is not finite.
    */
  def linear(a0: Double, a1: Double, result: Array[Double], offset: Int = 0): Int =
    if a1 == 0 then 0
    else
      val x = -a0/a1
      if x.finite then
        result(offset) = x + 0.0   // normalizes -0.0
        1
      else 0

  /** Distinct real roots of `a0 + a1*x + a2*x^2`, written ascending starting at
    * `result(offset)`; returns how many were written (0, 1, or 2).
    */
  def quadratic(a0: Double, a1: Double, a2: Double, result: Array[Double], offset: Int = 0): Int =
    if a2 == 0 then linear(a0, a1, result, offset)
    else if !(a0.finite && a1.finite && a2.finite) then 0
    else if a0 == 0 then insertZero(result, offset, linear(a1, a2, result, offset))
    else
      val disc = snapDiff(a1*a1, 4.0*a2*a0)
      if disc < 0 then 0
      else if disc == 0 then
        result(offset) = a1/(-2.0*a2)
        1
      else
        // Compute the root pair without subtractive cancellation
        val h = jm.sqrt(disc)
        val q = -0.5*(a1 + (if a1 < 0 then -h else h))
        val x0 = q/a2
        val x1 = a0/q
        if x0 < x1 then
          result(offset) = x0
          result(offset + 1) = x1
        else
          result(offset) = x1
          result(offset + 1) = x0
        2

  // Roots of t^3 + p*t + q, ascending.  The three-root branch computes the largest root
  // by angle trisection and the other two from it; the one-root branch pairs the dominant
  // cube root with its exact partner to avoid cancellation.
  private def depressedCubic(p: Double, q: Double, result: Array[Double], offset: Int): Int =
    val u = -0.5*q
    val v = p/(-3.0)
    val dd = snapDiff(u*u, v*v*v)
    if dd > 0 then
      val s = jm.sqrt(dd)
      val j = if u >= 0 then jm.cbrt(u + s) else jm.cbrt(u - s)
      result(offset) = j + (if j == 0 then 0.0 else v/j)
      1
    else if dd == 0 then
      val w = jm.cbrt(u)
      if w == 0 then
        result(offset) = 0.0
        1
      else
        if w > 0 then
          result(offset) = -w
          result(offset + 1) = 2*w
        else
          result(offset) = 2*w
          result(offset + 1) = -w
        2
    else
      val l = jm.sqrt(v)
      val h = trisectCosine(u/(l*v))
      val t0 = 2*l*h
      val m = NumericConstants.SqrtThree * l * jm.sqrt(1.0 - h*h)
      result(offset) = -0.5*t0 - m
      result(offset + 1) = -0.5*t0 + m
      result(offset + 2) = t0
      3

  /** Distinct real roots of `a0 + a1*x + a2*x^2 + a3*x^3`, written ascending starting at
    * `result(offset)`; returns how many were written (0 through 3; at least 1 for a true cubic).
    */
  def cubic(a0: Double, a1: Double, a2: Double, a3: Double, result: Array[Double], offset: Int = 0): Int =
    if a3 == 0 then quadratic(a0, a1, a2, result, offset)
    else if !(a0.finite && a1.finite && a2.finite && a3.finite) then 0
    else if a0 == 0 then insertZero(result, offset, cubic(a1, a2, a3, 0.0, result, offset))
    else
      val b3 = a2/(3.0*a3)
      val c = a1/a3
      val p = c - 3.0*b3*b3
      val q = (2.0*b3*b3 - c)*b3 + a0/a3
      val n = depressedCubic(p, q, result, offset)
      var i = 0
      while i < n do
        result(offset + i) = polish(a0, a1, a2, a3, 0.0, result(offset + i) - b3)
        i += 1
      sortDistinct(result, offset, n)

  // Roots of t^4 + p*t^2 + q*t + r (r != 0, q != 0), ascending: factor into two quadratics
  // using the largest root w of the resolvent cubic (largest => least cancellation, and
  // guaranteed positive since the resolvent is -q^2 < 0 at w = 0).  The smaller of the two
  // quadratic constant terms is recomputed from s*u = r to sidestep cancellation.
  private def ferrari(p: Double, q: Double, r: Double, result: Array[Double], offset: Int): Int =
    val nw = cubic(-q*q, p*p - 4.0*r, 2.0*p, 1.0, result, offset)
    val w = if nw > 0 then result(offset + nw - 1) else Double.NaN
    if !(w > 0) then 0
    else
      val m = jm.sqrt(w)
      val half = 0.5*(p + w)
      val dq = 0.5*q/m
      var s = half - dq
      var u = half + dq
      if jm.abs(s) >= jm.abs(u) then u = r/s
      else s = r/u
      val n1 = quadratic(s, m, 1.0, result, offset)
      n1 + quadratic(u, -m, 1.0, result, offset + n1)

  // Roots of t^4 + p*t^2 + q*t + r, ascending.
  private def depressedQuartic(p: Double, q: Double, r: Double, result: Array[Double], offset: Int): Int =
    if r == 0 then insertZero(result, offset, depressedCubic(p, q, result, offset))
    else if q == 0 then
      // Biquadratic: real roots come in +- pairs from nonnegative roots in u = t^2
      val nu = quadratic(r, p, 1.0, result, offset)
      var u0 = Double.NaN
      var u1 = Double.NaN
      if nu > 0 then u0 = result(offset)
      if nu > 1 then u1 = result(offset + 1)
      var n = 0
      if u1 > 0 then
        val x = jm.sqrt(u1)
        result(offset) = -x
        result(offset + 3) = x  // provisional; shifted down below if u0 yields nothing
        n = 2
      if u0 > 0 then
        val x = jm.sqrt(u0)
        if n == 2 then
          result(offset + 1) = -x
          result(offset + 2) = x
          n = 4
        else
          result(offset) = -x
          result(offset + 1) = x
          n = 2
      else if u0 == 0 || u1 == 0 then
        if n == 2 then
          result(offset + 1) = 0.0
          result(offset + 2) = result(offset + 3)
          n = 3
        else
          result(offset) = 0.0
          n = 1
      else if n == 2 then result(offset + 1) = result(offset + 3)
      n
    else ferrari(p, q, r, result, offset)

  /** Distinct real roots of `a0 + a1*x + a2*x^2 + a3*x^3 + a4*x^4`, written ascending
    * starting at `result(offset)`; returns how many were written (0 through 4).
    */
  def quartic(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, result: Array[Double], offset: Int = 0): Int =
    if a4 == 0 then cubic(a0, a1, a2, a3, result, offset)
    else if !(a0.finite && a1.finite && a2.finite && a3.finite && a4.finite) then 0
    else if a0 == 0 then insertZero(result, offset, quartic(a1, a2, a3, a4, 0.0, result, offset))
    else
      val b4 = a3/(4.0*a4)
      val c = a2/a4
      val d = a1/a4
      val e = a0/a4
      val bb = b4*b4
      val p = c - 6.0*bb
      val q = d - 2.0*c*b4 + 8.0*bb*b4
      val r = e - d*b4 + c*bb - 3.0*bb*bb
      val n = depressedQuartic(p, q, r, result, offset)
      var i = 0
      while i < n do
        result(offset + i) = polish(a0, a1, a2, a3, a4, result(offset + i) - b4)
        i += 1
      sortDistinct(result, offset, n)
}
