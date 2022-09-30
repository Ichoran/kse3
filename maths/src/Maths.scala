// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-22 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.maths

import java.lang.{Math => jm}

import scala.annotation.targetName

object NumericConstants {
  // Common constants involving Pi or roots
  inline val OneOverPi = 0.31830988618379067154
  inline val OverSqrtTwoPi = 0.39894228040143267794
  inline val TwoOverPi = 0.63661977236758134308
  inline val OverSqrtTwo = 0.7071067811865475244
  inline val SqrtTwo = 1.4142135623730950488
  inline val PiOverTwo = 1.5707963267948966192
  inline val SqrtTwoPi = 2.5066282746310005024
  inline val TwoPi = 6.2831853071795864769
  inline val OverTwoPi = 0.15915494309189533576

  // Less common constants involving Pi
  inline val LnTwoPi = 1.8378770664093454836
  inline val HalfLnTwoPi = 0.91893853320467274178
  inline val OverSqrtEight = 0.3535533905932737622
  inline val QuarterSqrtPi = 0.44311346272637900682

  inline val DegreesPerRadian = 57.295779513082320877
  inline val RadiansPerDegree = 0.017453292519943295769

  // Logarithmic and exponential constants
  inline val OverLnTwo = 1.4426950408889634079
  inline val LnTwo = 0.69314718055994530942
  inline val LnHalf = -0.69314718055994530942
  inline val NegOverLnTwo = -1.4426950408889634079
  inline val OverE = 0.36787944117144233

  // Constants for statistical distributions; bidirectional conversions chosen so they multiply to 1.
  inline val GaussianFWHMToSigma = 0.7413011092528010        // Conversion constant from Mathematica 8: 1/(2*InverseErf[1/2]*Sqrt[2])
  inline val GaussianSigmaToFWHM = 1.3489795003921635        // Conversion constant from Mathematica 8: 2*InverseErf[1/2]*Sqrt[2]
  inline val Gaussian90PctToSigma = 0.30397841595588447291   // One sigma is this fraction of the central 90% of a Gaussian distribution.    

  // Constants for numeric approximation
  inline val GammaTwentyTwo = 5.109094217170944e19
  inline val LanczosDoubleG = 6.0246800407767295837         // Magic value not from Mathematica

  // Constants involving machine precision
  inline val TiniestDouble = 2.2250738585072014e-308       // NOT subnormal, that's Double.MinPositiveValue
  inline val SqrtTiniestDouble = 1.4916681462400413e-154
  inline val OverSqrtTiniestDouble = 6.7039039649712985e153
  inline val EpsDouble10x = 2.220446049250313e-15
  inline val EpsDouble100x = 2.220446049250313e-14
}

object NumericFunctions {
  import NumericConstants._

  inline def log2(d: Double) = jm.log(d) * OverLnTwo

  def entropy(d: Double) = if (d == 0) 0 else d * jm.log(d) * NegOverLnTwo

  // Functions useful in computing statistical distributions
  // Gamma functions and their ilk (including complete beta)
  // Common operation when using Lanczos rational function approximation for gamma
  private final def lanczosLogGTerm(x: Double) = (x-0.5)*log(x*0.36787944117144232160 + 2.0324162060519644573)

  // "Exact" (double precision) Lanczos rational function for gamma
  // Values taken from Boost 1.53, at 20 digits of precision for g value 6.024680040776729583740234375
  // Takes ~20 ns on 3.33 GHz Intel Xeon X5680
  private final def lanczosApproximationRatio(x: Double) = {
    (56906521.9134715639 + x*(
      103794043.1163445452 + x*(
        86363131.28813859146 + x*(
          43338889.32467613834 + x*(
            14605578.08768506808 + x*(
              3481712.154980645909 + x*(
                601859.6171681098787 + x*(
                  75999.29304014542650 + x*(
                    6955.999602515376140 + x*(
                      449.9445569063168119 + x*(
                        19.51992788247617483 + x*(
                          0.5098416655656676188 + x*0.006061842346248906526))))))))))))/(
      x*(
        39916800 + x*(
          120543840 + x*(
            150917976 + x*(
              105258076 + x*(
                45995730 + x*(
                  13339535 + x*(
                    2637558 + x*(
                      357423 + x*(
                        32670 + x*(
                          1925 + x*(
                            66 + x)))))))))))   // Coefficients from Mathematica 8: CoefficientList[Product[(x + i), {i, 1, 11}], x]
    )
  }

  // Takes ~60 ns on 3.33GHz Intel Xeon X5680
  final def lnGamma(z: Double): Double = lanczosLogGTerm(z) + jm.log(lanczosApproximationRatio(z))

  // Takes ~15-30 ns for integer z <= 21, 20-70 ns for integer 22 <= z <= 60, 110 ns for real z > 0, 210 ns for z < 0  (3.33GHz Intel Xeon X5680)
  final def gamma(z: Double): Double =
    if z > 0 then
      if z <= 60.5 && jm.abs(z-jm.rint(z)) < 100*jm.ulp(z) then
        val n = jm.round(z).toInt
        if n <= 21 then
          var p = 1L
          var i = 2
          while i < n do
            p *= i
            i += 1
          p.toDouble
        else
          var q = GammaTwentyTwo
          var i = 23
          while i < n do
            q *= i
            i += 1
          q
      else jm.exp(lanczosLogGTerm(z))*lanczosApproximationRatio(z)
    else
      // Reflection formula, Gamma(z)*Gamma(1-z) = pi / sin(pi * z)
      val d = jm.sin(jm.PI * z)
      if d == 0 then Double.NaN
      else jm.PI / (d * jm.exp(lanczosLogGTerm(1-z)) * lanczosApproximationRatio(1-z))

  final def lnGammaRat(z: Double, w: Double): Double = 
    lanczosLogGTerm(z) - lanczosLogGTerm(w) + jm.log(lanczosApproximationRatio(z)/lanczosApproximationRatio(w))

  final def gammaRat(z: Double, w: Double): Double = 
    jm.exp(lanczosLogGTerm(z) - lanczosLogGTerm(w))*lanczosApproximationRatio(z)/lanczosApproximationRatio(w)

  // lnBeta is lnGamma(a) + lnGamma(b) - lnGamma(a+b) but we'll take it apart to calculate more efficiently
  // Takes ~150 ns on a 3.33GHz Intel Xeon X5680
  final def lnBeta(a: Double, b: Double): Double =
    if a < b then lnBeta(b, a)
    else if a <= 0 || b <= 0 then Double.NaN
    else
      val c = a+b
      lanczosLogGTerm(a) + lanczosLogGTerm(b) - lanczosLogGTerm(c) 
      + log(lanczosApproximationRatio(a)*lanczosApproximationRatio(b)/lanczosApproximationRatio(c))

  // beta is gamma(a)gamma(b)/gamma(a+b) but we'll take it apart to calculate more efficiently
  // Takes 40-110 ns for small integer a,b, 200 ns for general case (large integer or real) (3.33GHz Intel Xeon X5680)
  final def beta(a: Double, b: Double): Double =
    if a < b then beta(b, a)
    else if a <= 0 || b <= 0 then Double.NaN
    else
      val c = a+b
      if b < 40.5 && c < 1024.5 && jm.abs(a-jm.rint(a)) + jm.abs(b-jm.rint(b)) < 100*jm.ulp(c) then
        var n = jm.round(c).toInt - 1
        var m = jm.round(b).toInt - 1
        var p = 1.0
        var q = 1.0
        while m >= 1 do
          p *= n
          q *= m
          m -= 1
          n -= 1
        q/(p * n)
      else
        jm.exp(lanczosLogGTerm(a) + lanczosLogGTerm(b) - lanczosLogGTerm(c))
        * lanczosApproximationRatio(a)*lanczosApproximationRatio(b)/lanczosApproximationRatio(c)

  // Reasonably high-quality error/inverse error functions for general use
  // Based on Applied Statistics 37:477-484 (1988), alg. AS241
  // Takes ~45 ns on a 3.33 GHz Intel Xeon X5680
  def icdfNormal(p: Double): Double =
    val h = p-0.5
    val x = 0.180625 - h*h
    if x >= 0 then
      h*
      (((((((2.5090809287301226727e3*x + 3.3430575583588128105e4
            )*x + 6.7265770927008700853e4
           )*x + 4.5921953931549871457e4
          )*x + 1.3731693765509461125e4
         )*x + 1.9715909503065514427e3
        )*x + 1.3314166789178437745e2
       )*x + 3.3871328727963666080e0
      ) /
      (((((((5.2264952788528545610e3*x + 2.8729085735721942674e4
            )*x + 3.9307895800092710610e4
           )*x + 2.1213794301586595867e4
          )*x + 5.3941960214247511077e3
         )*x + 6.8718700749205790830e2
        )*x + 4.2313330701600911252e1
       )*x + 1.0e0
      )
    else
      val hh = if h <= 0 then -1.0 else 1.0
      val y = 
        if h <= 0 then { if p == 0 then return Double.NegativeInfinity; p }
        else           { if p == 1 then return Double.PositiveInfinity; 1.0-p }
      val z = jm.sqrt(-jm.log(y))
      if z <= 5.0 then
        val x = z - 1.6
        hh*
        (((((((7.74545014278341407640e-4*x + 2.27238449892691845833e-2
              )*x + 2.41780725177450611770e-1
             )*x + 1.27045825245236838258e0
            )*x + 3.64784832476320460504e0
           )*x + 5.76949722146069140550e0
          )*x + 4.63033784615654529590e0
         )*x + 1.42343711074968357734e0
        ) /
        (((((((1.05075007164441684324e-9*x + 5.47593808499534494600e-4
              )*x + 1.51986665636164571966e-2
             )*x + 1.48103976427480074590e-1
            )*x + 6.89767334985100004550e-1
           )*x + 1.67638483018380384940e0
          )*x + 2.05319162663775882187e0
         )*x + 1.0
        )
      else
        val x = z - 5.0
        hh*
        (((((((2.01033439929228813265e-7*x + 2.71155556874348757815e-5
              )*x + 1.24266094738807843860e-3
             )*x + 2.65321895265761230930e-2
            )*x + 2.96560571828504891230e-1
           )*x + 1.78482653991729133580e0
          )*x + 5.46378491116411436990e0
         )*x + 6.65790464350110377720e0
        ) /
        (((((((2.04426310338993978564e-15*x + 1.42151175831644588870e-7
              )*x + 1.84631831751005468180e-5
             )*x + 7.86869131145613259100e-4
            )*x + 1.48753612908506148525e-2
           )*x + 1.36929880922735805310e-1
          )*x + 5.99832206555887937690e-1
         )*x + 1.0
        )

  inline def erfInv(x: Double) = OverSqrtTwo * icdfNormal(0.5+0.5*x)

  inline def erfcInv(x: Double) = OverSqrtTwo * icdfNormal(1.0-0.5*x)

  // Piecewise rational function approximation of CDF for Normal distribution (courtesy of Mathematica 7)
  // Should be full double precision
  // Takes ~100ns on a 3.33 GHz Intel Xeon X5680 for moderate values
  def cdfNormal(y: Double) =
    if y > 8.3 then 1.0
    else if y < -38.5 then 0.0
    else
      val x = if y < 0 then -y else y
      val f =
        if x < 3 then
          jm.exp(
            -0.5*x*x -
            (((((((-3.6271830621274548308e-6*x - 6.2054577195631746255e-5
                  )*x + 0.0020555154846807655013
                 )*x + 0.032099345474574417685
                )*x + 0.21504119632351847003
               )*x + 0.73055326515392090713
              )*x + 1.3812898842892215850
             )*x + 0.69314718055994526146
            ) /
            (((((((-5.8186829446354815108e-7*x - 2.2135273033157240657e-5
                  )*x + 3.6576165145176352643e-4
                 )*x + 0.0094667294072793799548
                )*x + 0.078740088812851505927
               )*x + 0.34723234319509102797
              )*x + 0.84167596702197143827
             )*x + 1.0
            )
          )
        else if x < 16 then
          (jm.exp( -0.5*x*x )
          * ((((((0.00118089255719362346624*x + 0.0136334301130162766315
                   )*x + 0.086474160844062169269
                  )*x + 0.33993667920309143168
                 )*x + 0.86339167691367313008
                )*x + 1.3345326346191572297
               )*x + 1
              ) /
              (((((((0.0029600586715196076372*x + 0.034173941597530707646
                    )*x + 0.21971862448906668587
                   )*x + 0.88626919617829879773
                  )*x + 2.3750320592403537542
                 )*x + 4.1290652702771203918
                )*x + 4.2651316245967753927
               )*x + 1.9999244808870340017
              )
            )
        else
          val f0 = jm.exp(-0.5*x*x) * OverSqrtTwoPi
          val z = 1/(x*x)
          var g, sum = 1/x
          var i = -1
          while i >= -20 do
            g *= i * z
            sum += g
            i -= 2
          f0 * sum
      if y > 0 then 1.0 - f else f


  inline def erf(x: Double) = 2.0*cdfNormal(SqrtTwo * x) - 1.0

  inline def erfc(x: Double) = 2.0 - 2.0*cdfNormal(SqrtTwo * x)


  @annotation.tailrec
  private def nestCosS(n: Int, ib: Double, x: Double): Double =
    if n < 4 then x
    else nestCosS(n-2, ib, 1 + (x*ib*(n-3))/(n-2))

  // Student's T test distribution functions (special case of incomplete regularized beta)
  // Approximations from Hill, Comm. ACM, Algorithm 395 & 396, v13 pp 617-620 (1970)
  // Takes no more than about 180 ns on a 3.33 GHz Intel Xeon X5680 (df = 18)
  def cdfStudentT(df: Long, t0: Double): Double =
    val t = jm.abs(t0)
    val p =
      if df == 1 then
        0.5*(1 - TwoOverPi * jm.atan(t))
      else
        val y = t*t/df
        if df >= 20 then
          val dg = df - 0.5
          val b = 48*dg*dg
          val z = if y > 1e-6 then dg * jm.log(1+y) else dg * y
          cdfNormal( -jm.sqrt(z)*(((((-0.4*z - 3.3)*z - 24.0)*z - 85.5)/(0.8*z*z+100+b) + z + 3)/b + 1) )
        else
          val iy1 = 1/(1+y)
          val cs = if df < 4 then 1.0 else nestCosS(df.toInt, iy1, 1.0)
          val q2 =
            if (df & 1) == 0 then jm.sqrt(y/(1+y))*cs
            else
              var yrt = jm.sqrt(y)
              TwoOverPi*(jm.atan(yrt) + yrt*iy1*cs)
          0.5*jm.max(0 , 1 - q2)
    if t0 < 0 then p else 1-p

  // Takes about 350 ns on a 3.33 GHz Intel Xeon X5680 (df = 12)
  def icdfStudentT(df: Long, p0: Double): Double =
    val p = if p0 > 0.5 then 2*(1 - p0) else 2*p0
    val t =
      if df < 2 then
        1.0/jm.tan(p * PiOverTwo)
      else if df == 2 then
        jm.sqrt(2/(p*(2-p)) - 2)
      else
        val dg = df - 0.5
        val idg = 1/dg
        val b = 48*dg*dg
        val ib = 1/b
        val c = ((20700*idg*ib - 98)*idg-16)*idg + 96.36
        val d = ((94.5/(b+c)-3)*ib+1)*jm.sqrt(idg * PiOverTwo)*df
        val y = jm.pow(d*p, 2.0/df)
        val z =
          if y > 0.05 + idg then
            val in = icdfNormal(p*0.5)
            val insq = in*in
            val e = if df < 5 then c + 0.3*(df - 4.5)*(in+0.6) else c
            val f = (((0.05*d*in-5)*in-7)*in-2)*in + b + e
            val g = (((((0.4*insq + 6.3)*insq + 36)*insq + 94.5)/f - insq - 3)*ib + 1)*in
            val h = idg*g*g
            if h > 0.002 then exp(h) - 1 else 0.5*h*h + h
          else ((1/(((df + 6)/(df*y) - 0.089*d - 0.822)*(df+2)*3) + 0.5/(df+4))*y-1)*(df+1)/(df+2.0) + 1/y
        jm.sqrt(df*z)
    if p0 > 0.5 then t else -t

  // Regularized incomplete gamma functions and chi squared distributions
  // $\gamma (s,x) = \frac{1}{\Gamma (s)} \cdot \int_{0}^{x} t^{s-1} e^{-t} dt$
  // Using standard form found in Cuyt & Peterson's "Handbook of Continued Fractions for Special Functions"
  // unless x is small so the series form should do better.  Assumes s>0,x>0.
  // A better split could be found for s,x >> 1000
  private final def igammaLowerTaylorTerm(s: Double, x: Double): Double =
    var taylor = 1.0/s;
    var sum = taylor;
    var denom = 1.0+s
    while taylor > 100*ulp(sum) do
      taylor *= x/denom
      sum += taylor
      denom += 1.0
    sum

  private final def igammaUpperContFracTerm(s: Double, x: Double): Double =
    var cont = x + 1.0 - s
    var lentzC = OverSqrtTiniestDouble
    var lentzD = if jm.abs(cont) < SqrtTiniestDouble then OverSqrtTiniestDouble else 1.0/cont
    var factor = 2.0
    var prod = lentzD
    var i = 1
    while jm.abs(factor-1) > EpsDouble100x do
      val a = i*(s-i)
      cont += 2.0
      lentzC = cont + a/lentzC
      if jm.abs(lentzC) < SqrtTiniestDouble then lentzC = SqrtTiniestDouble * jm.signum(lentzC)
      lentzD = cont + a*lentzD
      if jm.abs(lentzD) < SqrtTiniestDouble then lentzD = OverSqrtTiniestDouble * jm.signum(lentzD) else lentzD = 1.0/lentzD
      factor = lentzC*lentzD
      prod *= factor
      i += 1
    prod

  private final def igammaRegShapeApprox(s: Double, x: Double) = exp(-x + s*log(x) - lnGamma(s))

  // Takes about 300 ns on a 3.33 GHz Intel Xeon X5680
  def regularizedLowerIncompleteGamma(s: Double, x: Double) =
    if x < s+1 then igammaLowerTaylorTerm(s, x) * igammaRegShapeApprox(s, x)
    else 1.0 - igammaUpperContFracTerm(s, x) * igammaRegShapeApprox(s, x)

  // Takes about 300 ns on a 3.33 GHz Intel Xeon X5680
  def regularizedUpperIncompleteGamma(s: Double, x: Double) =
    if x < s+1 then 1.0 - igammaLowerTaylorTerm(s, x) * igammaRegShapeApprox(s, x)
    else igammaUpperContFracTerm(s, x) * igammaRegShapeApprox(s, x)

  // Runtime equal to regularizedLowerIncompleteGamma
  inline def cdfChiSq(df: Double, chisq: Double) = regularizedLowerIncompleteGamma(0.5*df, 0.5*chisq)


  // Incomplete beta functions and F distribution based on DiDonato & Morris, ACM Trans Math Soft v18 pp360-373 (1992)
  // Additional inspiration taken from bratio.f90 by DD & M, and Boost 1.53 implementation and documentation also based on DD & M
  private def ddmMethodBPSER(a: Double, b: Double, x: Double) =
    var nu = a
    var de = 1.0
    var term = EpsDouble100x
    var sum = 1.0
    var j = 0
    while jm.abs(term) >= sum * EpsDouble100x do
      j += 1
      nu *= (j-b)*x
      de *= j
      term = nu/(de*(a+j))
      sum += term
    exp(a * log(x) - lnBeta(a, b)) * sum / a
  
  private def ddmMethodBUP(a: Double, b: Double, x: Double, n: Int) =
    var term = jm.exp(a*log(x) + b*log(1-x) - lnBeta(a,b))/a
    var sum = term
    var j = 1
    val ab1 = a+b-1
    val earliable = if b <= 1 then 1 else jm.ceil((b-1)*x/(1-x) - a).toInt
    while j < n && (j <= earliable || sum * EpsDouble100x < term) do
      term *= (ab1+j)*x/(a+j)
      sum += term
      j += 1
    sum

  // Only gives about 9 digits accuracy
  private def ddmMethodBGRAT(a: Double, b: Double, x: Double, w: Double = 0.0) =
    val t = a + 0.5*(b-1)
    val lx = jm.log(x)
    val u = -t*lx
    val lh = -u + b*jm.log(u) - lnGamma(b)
    val m = jm.exp(lh - b*jm.log(t) + lnGammaRat(a+b,a))
    val ew = jm.abs(w/m)*EpsDouble100x
    var p = new Array[Double](8); p(0) = 1
    val i4tsq = 1/(4*t*t)
    val lx2sq = 0.25*lx*lx
    var j = regularizedUpperIncompleteGamma(b, u)*exp(-lh)
    var term = j
    var sum = term
    var n = 0
    val ub = u + b
    var q = b-1
    var g = 1.0
    while jm.max(ew, sum*EpsDouble100x) < jm.abs(term) do
      j = i4tsq*(g*(ub+(2*n+1)) + (b+2*n)*(b+2*n+1)*j)
      g *= lx2sq
      n += 1
      q /= 2*n*(2*n+1)
      var m = 1
      var s = 0.0
      var r = 1.0
      while m < n do
        r *= (m*b-n)/(2*m*(2*m+1))
        s += r*p(n-m)
        m += 1
      if n >= p.length then p = java.util.Arrays.copyOf(p, p.length*2)
      p(n) = q + s/n
      term = p(n)*j
      sum += term
    m * sum

  private def ddmMethodBFRAC(a: Double, b: Double, x: Double) =
    val lam1 = 1 + a - (a+b)*x
    val ia = 1/a
    var p = 1.0
    var an = 0.0
    var bn = 1.0
    var an1 = 1.0
    var bn1 = lam1/(1 + ia)
    var r = (1 + ia)/lam1
    var n = 1
    while n != 0 do
      val w = n*(b - n)*x
      val ia2n1 = 1/(a + (2*n - 1))
      val e = a*ia2n1
      val alph = (p*(p + b*ia)*e*e)*(w*x)
      if alph <= 0 then n = 0
      else
        p = 1 + n*ia
        val bet = n + w*ia2n1 + (p/(1+ia*(2*n+1)))*(lam1 + n*(2 - x))
        val aa = alph*an + bet*an1; an = an1; an1 = aa
        val bb = alph*bn + bet*bn1; bn = bn1; bn1 = bb
        val r0 = r
        val ibn1 = 1/bn1
        r = an1*ibn1
        if jm.abs(r - r0) <= EpsDouble100x * r then n = 0
        else
          an *= ibn1
          an1 = r
          bn *= ibn1
          bn1 = 1
          n += 1
    r * jm.exp(a*jm.log(x) + b*jm.log(1-x) - lnBeta(a,b))

  // Incomplete regularized beta.  At least 9 digits of accuracy almost everywhere.
  // ~1000 ns for most values, except for large a,b with x near a/(a+b), which takes ~2000*log10((a+b)/1000) ns (all on a 3.33 GHz Intel Xeon X5680)
  def regularizedIncompleteBeta(a: Double, b: Double)(x: Double): Double =
    if a <= 0 || b <= 0 then return Double.NaN
    val y = 1-x
    if x <= 0 || y <= 0 then
      return if jm.min(x,y) > -EpsDouble100x then (if x < 0.5 then 0 else 1) else Double.NaN
    val abm = jm.min(a, b)
    val abM = jm.max(a, b)
    if abm < 1 then
      if x > 0.5 then 1 - regularizedIncompleteBeta(b, a)(1-x)
      else if abM <= 1 then
        if a >= jm.min(0.2,b) || (a*jm.log(x) <= -0.1053605156578263 /* log(0.9) */) then ddmMethodBPSER(a, b, x)
        else if x >= 0.3 then 1 - ddmMethodBPSER(b, a, 1-x)
        else
          val w = ddmMethodBUP(b, a, 1-x, 20)
          1 - (w + ddmMethodBGRAT(b + 20, a, 1-x, w))
      else if b <= 1 then ddmMethodBPSER(a, b, x)
      else
        if x >= 0.3 then 1 - ddmMethodBPSER(b, a, 1-x)
        else if x < 0.1 && a*jm.log(x*b) <= -0.35667494393873238 /* log(0.7) */ then ddmMethodBPSER(a, b, x)
        else
          val n = if b <= 15 then 20                          else 0
          val w = if b <= 15 then ddmMethodBUP(b, a, 1-x, 20) else 0.0
          1 - (w + ddmMethodBGRAT(b + n, a, 1-x, w))
    else if x*(a+b) > a then 1 - regularizedIncompleteBeta(b, a)(1-x)
    else if b >= 40 then ddmMethodBFRAC(a, b, x)
    else
      val m = jm.ceil(b).toInt - 1
      if b*x < 0.7 then ddmMethodBPSER(a, b, x)
      else if x <= 0.7 then ddmMethodBUP(b-m, a, 1-x, m) + ddmMethodBPSER(a, b-m, x)
      else
        val w = ddmMethodBUP(b-m, a, 1-x, m)
        val n = if a <= 15 then 20                         else 0
        val v = if a <= 15 then ddmMethodBUP(a, b-m, x,20) else 0.0
        w + v + ddmMethodBGRAT(a+n, b-m, x, w+v)

  // F distribution from  incomplete regularized beta
  def cdfFDist(n: Int, m: Int)(f: Double) = regularizedIncompleteBeta(0.5*n, 0.5*m)(n*f/(n*f+m))
}


extension (b: Byte) {
  def +!(c: Byte): Byte =
    val x = b + c
    if x < -128 then (-128: Byte) else if x > 127 then (127: Byte) else x.toByte
  def -!(c: Byte): Byte =
    val x = b - c
    if x < -128 then (-128: Byte) else if x > 127 then (127: Byte) else x.toByte
  def *!(c: Byte): Byte =
    val x = b * c
    if x < -128 then (-128: Byte) else if x > 127 then (127: Byte) else x.toByte
  def /!(c: Byte): Byte =
    if c == -1 && b == Byte.MinValue then Byte.MaxValue
    else (b / c).toByte
  inline def %!(c: Byte): Byte = (b % c).toByte
  inline def clamp(lo: Byte, hi: Byte) =
    if lo <= b then
      if b <= hi then b
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Byte, hi: Byte) = lo <= b && b <= hi

  @targetName("byte_unsigned") inline def unsigned: kse.maths.UByte = UByte.wrap(b)
  @targetName("byte_toUByte")  inline def toUByte: kse.maths.UByte  = UByte.wrap(b)
  @targetName("byte_toUInt")   inline def toUInt: kse.maths.UInt    = UInt.wrap(b & 0xFF)
  @targetName("byte_toULong")  inline def toULong: kse.maths.ULong  = ULong.wrap(b & 0xFF)

  @targetName("byte_clampUByte") inline def clampToUByte: kse.maths.UByte = UByte.clamp(b)
  @targetName("byte_clampChar")  inline def clampToChar: Char             = if b < 0 then '\u0000' else b.toChar
  @targetName("byte_clampUInt")  inline def clampToUInt: kse.maths.UInt   = if b < 0 then UInt(0) else UInt(b.toInt)
  @targetName("byte_clampULong") inline def clampToULong: kse.maths.ULong = if b < 0 then ULong(0L) else ULong(b.toLong)
}

extension (s: Short) {
  def +!(t: Short): Short =
    val x = s + t
    if x < -32768 then (-32768: Short) else if x > 32767 then (32767: Short) else x.toShort
  def -!(t: Short): Short =
    val x = s - t
    if x < -32768 then (-32768: Short) else if x > 32767 then (32767: Short) else x.toShort
  def *!(t: Short): Short =
    val x = s * t
    if x < -32768 then (-32768: Short) else if x > 32767 then (32767: Short) else x.toShort
  def /!(t: Short): Short = 
    if t == -1 && s == Short.MinValue then Short.MaxValue
    else (s / t).toShort
  inline def %!(t: Short): Short = (s % t).toShort
  inline def clamp(lo: Short, hi: Short) =
    if lo <= s then
      if s <= hi then s
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Short, hi: Short) = lo <= s && s <= hi

  @targetName("short_toUByte")  inline def toUByte: kse.maths.UByte = UByte.wrap((s & 0xFF).toByte)
  @targetName("short_toUInt")   inline def toUInt: kse.maths.UInt   = UInt.wrap(s & 0xFFFF)
  @targetName("short_toULong")  inline def toULong: kse.maths.ULong = ULong.wrap(s & 0xFFFFL)

  @targetName("short_clampByte")  def clampToByte: Byte             = if s < -128 then -128 else if s > 127 then 127 else s.toByte
  @targetName("short_clampUByte") def clampToUByte: kse.maths.UByte = if s < 0 then UByte(0) else if s > 255 then UByte(255) else UByte(s.toByte)
  @targetName("short_clampChar")  def clampToChar: Char             = if s < 0 then '\u0000' else s.toChar
  @targetName("short_clampUInt")  def clampToUInt: kse.maths.UInt   = if s < 0 then UInt(0) else UInt(s.toInt)
  @targetName("short_clampULong") def clampToULong: kse.maths.ULong = if s < 0 then ULong(0L) else ULong(s.toLong)
}

extension (c: Char) {
  inline def clamp(lo: Char, hi: Char) =
    if lo <= c then
      if c <= hi then c
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Char, hi: Char) = lo <= c && c <= hi

  @targetName("char_toUByte") inline def toUByte: kse.maths.UByte = UByte.wrap((c & 0xFF).toByte)
  @targetName("char_toUInt")  inline def toUInt: kse.maths.UInt   = UInt.wrap(c)
  @targetName("char_toULong") inline def toULong: kse.maths.ULong = ULong.wrap(c)

  @targetName("char_clampByte")  def clampToByte: Byte             = if c > '\u007F' then 127 else c.toByte
  @targetName("char_clampUByte") def clampToUByte: kse.maths.UByte = if c > '\u00FF' then UByte.MaxValue else UByte(c.toByte)
  @targetName("char_clampShort") def clampToShort: Short           = if c > '\u7FFF' then Short.MaxValue else c.toShort
}

extension (i: Int) {
  def +!(j: Int): Int =
    val x = i.toLong + j.toLong
    if x < Int.MinValue then Int.MinValue else if x > Int.MaxValue then Int.MaxValue else x.toInt
  def -!(j: Int): Int =
    val x = i.toLong - j.toLong
    if x < Int.MinValue then Int.MinValue else if x > Int.MaxValue then Int.MaxValue else x.toInt
  def *!(j: Int): Int =
    val x = i.toLong * j.toLong
    if x < Int.MinValue then Int.MinValue else if x > Int.MaxValue then Int.MaxValue else x.toInt
  def /!(j: Int): Int =
    if j == -1 && i == Int.MinValue then Int.MaxValue
    else i / j
  inline def %!(j: Int): Int = i % j
  inline def clamp(lo: Int, hi: Int) =
    if lo <= i then
      if i <= hi then i
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Int, hi: Int) = lo <= i && i <= hi

  @targetName("int_unsigned") inline def unsigned: kse.maths.UInt = UInt.wrap(i)
  @targetName("int_toUByte")  inline def toUByte: kse.maths.UByte = UByte.wrap((i & 0xFF).toByte)
  @targetName("int_toUInt")   inline def toUInt: kse.maths.UInt   = UInt.wrap(i)
  @targetName("int_toULong")  inline def toULong: kse.maths.ULong = ULong.wrap(i & 0xFFFFFFFFL)

  @targetName("int_clampByte")  def clampToByte: Byte             = if i < -128 then -128 else if i > 127 then 127 else i.toByte
  @targetName("int_clampUByte") def clampToUByte: kse.maths.UByte = if i < 0 then UByte(0) else if i > 255 then UByte(255) else UByte(i.toByte)
  @targetName("int_clampShort") def clampToShort: Short           = if i < Short.MinValue then Short.MinValue else if i > Short.MaxValue then Short.MaxValue else i.toShort
  @targetName("int_clampChar")  def clampToChar: Char             = if i < 0 then '\u0000' else if i > 0xFFFF then '\uFFFF' else i.toChar
  @targetName("int_clampUInt")  def clampToUInt: kse.maths.UInt   = if i < 0 then UInt(0) else UInt(i)
  @targetName("int_clampULong") def clampToULong: kse.maths.ULong = if i < 0 then ULong(0L) else ULong(i.toLong)
}

extension (l: Long) {
  def +!(k: Long): Long =
    if (l < 0) != (k < 0) then l + k
    else
      val x = l + k
      if x >= 0 then
        if l >= 0 && k >= 0 then x
        else Long.MinValue
      else
        if l <= 0 && k <= 0 then x
        else Long.MaxValue
  def -!(k: Long): Long =
    if (l < 0) == (k < 0) then l - k
    else
      val x = l - k
      if x >= 0 then
        if l >= 0 && k <= 0 then x
        else Long.MinValue
      else
        if l <= 0 && k >= 0 then x
        else Long.MaxValue
  def *!(k: Long): Long =
    if l == Long.MinValue then
      if k == 0 then 0L
      else if k > 0 then l
      else Long.MaxValue
    else if k == Long.MinValue then
      if l == 0 then 0L
      else if l > 0 then k
      else Long.MaxValue
    else
      val a = if l < 0 then -l else l
      val b = if k < 0 then -k else k
      val za = java.lang.Long.numberOfLeadingZeros(a)
      val zb = java.lang.Long.numberOfLeadingZeros(b)
      if za + zb >= 65 then l * k
      else if za + zb == 64 then
        val a1 = a >>> 1
        val b1 = b >>> 1
        val qab = (a1 * b1) + (((a & 1) * b1 + (b & 1) * a1) >>> 1)
        if qab < (1L << 61) then l * k
        else if (l < 0) == (k < 0) then Long.MaxValue
        else Long.MinValue
      else if (l < 0) == (k < 0) then Long.MaxValue
      else Long.MinValue
  def /!(k: Long): Long =
    if k == -1L && l == Long.MinValue then Long.MaxValue
    else l / k
  inline def %!(k: Long): Long = l % k
  inline def clamp(lo: Long, hi: Long) =
    if lo <= l then
      if l <= hi then l
      else if lo <= hi then hi
      else lo
    else lo
  inline def in(lo: Long, hi: Long) = lo <= l && l <= hi

  @targetName("long_unsigned") inline def unsigned: kse.maths.ULong = ULong.wrap(l)
  @targetName("long_toUByte")  inline def toUByte: kse.maths.UByte  = UByte.wrap((l & 0xFFL).toByte)
  @targetName("long_toUInt")   inline def toUInt: kse.maths.UInt    = UInt.wrap((l & 0xFFFFFFFFL).toInt)
  @targetName("long_toULong")  inline def toULong: kse.maths.ULong  = ULong.wrap(l)

  @targetName("long_clampByte")  def clampToByte: Byte             = if l < -128 then -128 else if l > 127 then 127 else l.toByte
  @targetName("long_clampUByte") def clampToUByte: kse.maths.UByte = if l < 0 then UByte(0) else if l > 255 then UByte(255) else UByte(l.toByte)
  @targetName("long_clampShort") def clampToShort: Short           = if l < Short.MinValue then Short.MinValue else if l > Short.MaxValue then Short.MaxValue else l.toShort
  @targetName("long_clampChar")  def clampToChar: Char             = if l < 0 then '\u0000' else if l > 0xFFFF then '\uFFFF' else l.toChar
  @targetName("long_clampInt")   def clampToInt: Int               = if l < Int.MinValue then Int.MinValue else if l > Int.MaxValue then Int.MaxValue else l.toInt
  @targetName("long_clampUInt")  def clampToUInt: kse.maths.UInt   = if l < 0 then UInt(0) else UInt(l.toInt)
  @targetName("long_clampULong") def clampToULong: kse.maths.ULong = if l < 0 then ULong(0L) else ULong(l)
}

extension (inline x: Byte | Short | Int | Long) {
  transparent inline def bitsF = inline x match
    case i: Int => java.lang.Float.intBitsToFloat(i)

  transparent inline def bitsD = inline x match
    case l: Long => java.lang.Double.longBitsToDouble(l)

  transparent inline def leadingZeros: Int = inline x match
    case i: Int  => java.lang.Integer.numberOfLeadingZeros(i)
    case l: Long => java.lang.Long.   numberOfLeadingZeros(l)

  transparent inline def trailingZeros: Int = inline x match
    case i: Int  => java.lang.Integer.numberOfTrailingZeros(i)
    case l: Long => java.lang.Long.   numberOfTrailingZeros(l)

  transparent inline def u: UByte | UInt | ULong = inline x match
    case b: Byte => UByte(b)
    case i: Int  => UInt(i)
    case l: Long => ULong(l)
}

extension (f: Float) {
  inline def clamp(lo: Float, hi: Float) =
    if lo <= f && f <= hi then f
    else if f < lo then lo
    else if f > hi then
      if lo <= hi then hi
      else lo
    else Float.NaN

  inline def in(lo: Float, hi: Float) = lo <= f && f <= hi

  final def closeTo(that: Float, abstol: Float, fractol: Float): Boolean = 
    jm.abs(f - that) match
      case x if x <= abstol =>
        val big = jm.max(jm.abs(f), jm.abs(that))
        big <= 1 || x <= big*fractol
      case _ => false
  inline final def closeTo(that: Float): Boolean = closeTo(that, 1e-6f, 1e-6f)
}

extension (d: Double) {
  inline def cube = d * d * d
  inline def sqrt = jm.sqrt(d)
  inline def cbrt = jm.cbrt(d)
  inline def hypot(e: Double) = jm.hypot(d, e)
  inline def pow(e: Double) = jm.pow(d, e)
  inline def log = jm.log(d)
  inline def log2 = jm.log(d) * NumericConstants.OverLnTwo
  inline def log10 = jm.log10(d)
  inline def exp = jm.exp(d)
  inline def exp2 = jm.pow(2, d)
  inline def exp10 = jm.pow(10, d)
  inline def entropy = if (d == 0) 0 else d * jm.log(d) * NumericConstants.NegOverLnTwo

  inline def sin = jm.sin(d)
  inline def cos = jm.cos(d)
  inline def tan = jm.tan(d)
  inline def asin = jm.asin(d)
  inline def acos = jm.acos(d)
  inline def atan = jm.atan(d)
  inline def atan2(e: Double) = jm.atan2(d, e)
  inline def cosh = jm.cosh(d)
  inline def sinh = jm.sinh(d)
  inline def tanh = jm.tanh(d)

  inline def rad2deg = d * NumericConstants.DegreesPerRadian
  inline def rad2rev = d * NumericConstants.OverTwoPi
  inline def deg2rad = d * NumericConstants.RadiansPerDegree
  inline def rev2rad = d * NumericConstants.TwoPi

  inline def gamma = NumericFunctions.gamma(d)
  inline def lnGamma = NumericFunctions.lnGamma(d)
  inline def erf = NumericFunctions.erf(d)
  inline def erfc = NumericFunctions.erfc(d)
  inline def erfInv = NumericFunctions.erfInv(d)
  inline def erfcInv = NumericFunctions.erfcInv(d)
  inline def cdfNormal = NumericFunctions.cdfNormal(d)
  inline def icdfNormal = NumericFunctions.icdfNormal(d)

  inline def rint = jm.rint(d)

  inline def clamp(lo: Double, hi: Double) =
    if lo <= d && d <= hi then d
    else if d < lo then lo
    else if d > hi then
      if lo <= hi then hi
      else lo
    else Double.NaN

  inline def in(lo: Double, hi: Double) = lo <= d && d <= hi

  final def closeTo(that: Double, abstol: Double = 1e-12, fractol: Double = 1e-12) = 
    jm.abs(d - that) match
      case x if x <= abstol =>
        val big = jm.max(jm.abs(d), jm.abs(that))
        big <= 1 || x <= big*fractol
      case _ => false
}

extension (inline x: Byte | Short | Int | Long | Float | Double) {
  transparent inline def bitsI = inline x match
    case f: Float => java.lang.Float.floatToRawIntBits(f)

  transparent inline def bitsL = inline x match
    case d: Double => java.lang.Double.doubleToRawLongBits(d)

  transparent inline def f64: Double = inline x match
    case f: Float => f.toDouble

  transparent inline def f32: Float = inline x match
    case d: Double => d.toFloat

  transparent inline def sq = inline x match
    case b: Byte   => val d = b.toDouble; d * d
    case s: Short  => val d = s.toDouble; d * d
    case i: Int    => val d = i.toDouble; d * d
    case l: Long   => val d = l.toDouble; d * d
    case f: Float  => f * f
    case d: Double => d * d

  transparent inline def sign = inline x match
    case b: Byte   => if b > 0 then 1 else -(b >>> 31)
    case s: Short  => if s > 0 then 1 else -(s >>> 31)
    case i: Int    => if i > 0 then 1 else -(i >>> 31)
    case l: Long   => if l > 0 then 1L else -(l >>> 63)
    case f: Float  => jm.signum(f)
    case d: Double => jm.signum(d)

  transparent inline def ulp = inline x match
    case f: Float  => jm.ulp(f)
    case d: Double => jm.ulp(d)

  transparent inline def next = inline x match
    case f: Float  => jm.nextUp(f)
    case d: Double => jm.nextUp(d)

  transparent inline def prev = inline x match
    case f: Float  => jm.nextDown(f)
    case d: Double => jm.nextDown(d)

  transparent inline def nan = inline x match
    case f: Float  => java.lang.Float.isNaN(f)
    case d: Double => java.lang.Double.isNaN(d)

  transparent inline def inf = inline x match
    case f: Float  => java.lang.Float.isInfinite(f)
    case d: Double => java.lang.Double.isInfinite(d)

  transparent inline def finite = inline x match
    case f: Float  => (java.lang.Float.floatToRawIntBits(f) & 0x7F800000) != 0x7F800000
    case d: Double => (java.lang.Double.doubleToRawLongBits(d) & 0x7FF0000000000000L) != 0x7FF0000000000000L
}

opaque type UByte = Byte
object UByte {
  // The compiler is sometimes grumpy if we give it a 256-way type union all in one go.
  // So split it up 16 at a time.  Note that there is also a scala.compiletime.ops.int
  // solution (courtesy of Aly#7777 on Discord) but it overflows the stack:
  //   type ValidUpTo[Max] = Max match
  //     case 0 => Max
  //     case S[n] => Max | ValidUpTo[Max-1]
  type ValidIntValues0 = 0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x08 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x0E | 0x0F
  type ValidIntValues1 = 0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1E | 0x1F
  type ValidIntValues2 = 0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 | 0x26 | 0x27 | 0x28 | 0x29 | 0x2A | 0x2B | 0x2C | 0x2D | 0x2E | 0x2F
  type ValidIntValues3 = 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37 | 0x38 | 0x39 | 0x3A | 0x3B | 0x3C | 0x3D | 0x3E | 0x3F
  type ValidIntValues4 = 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4A | 0x4B | 0x4C | 0x4D | 0x4E | 0x4F
  type ValidIntValues5 = 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 | 0x58 | 0x59 | 0x5A | 0x5B | 0x5C | 0x5D | 0x5E | 0x5F
  type ValidIntValues6 = 0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69 | 0x6A | 0x6B | 0x6C | 0x6D | 0x6E | 0x6F
  type ValidIntValues7 = 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79 | 0x7A | 0x7B | 0x7C | 0x7D | 0x7E | 0x7F
  type ValidIntValues8 = 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0x8E | 0x8F
  type ValidIntValues9 = 0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99 | 0x9A | 0x9B | 0x9C | 0x9D | 0x9E | 0x9F
  type ValidIntValuesA = 0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA6 | 0xA7 | 0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC | 0xAD | 0xAE | 0xAF
  type ValidIntValuesB = 0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBE | 0xBF
  type ValidIntValuesC = 0xC0 | 0xC1 | 0xC2 | 0xC3 | 0xC4 | 0xC5 | 0xC6 | 0xC7 | 0xC8 | 0xC9 | 0xCA | 0xCB | 0xCC | 0xCD | 0xCE | 0xCF
  type ValidIntValuesD = 0xD0 | 0xD1 | 0xD2 | 0xD3 | 0xD4 | 0xD5 | 0xD6 | 0xD7 | 0xD8 | 0xD9 | 0xDA | 0xDB | 0xDC | 0xDD | 0xDE | 0xDF
  type ValidIntValuesE = 0xE0 | 0xE1 | 0xE2 | 0xE3 | 0xE4 | 0xE5 | 0xE6 | 0xE7 | 0xE8 | 0xE9 | 0xEA | 0xEB | 0xEC | 0xED | 0xEE | 0xEF
  type ValidIntValuesF = 0xF0 | 0xF1 | 0xF2 | 0xF3 | 0xF4 | 0xF5 | 0xF6 | 0xF7 | 0xF8 | 0xF9 | 0xFA | 0xFB | 0xFC | 0xFD | 0xFE | 0xFF
  type ValidIntValues =
    ValidIntValues0 | ValidIntValues1 | ValidIntValues2 | ValidIntValues3 |
    ValidIntValues4 | ValidIntValues5 | ValidIntValues6 | ValidIntValues7 |
    ValidIntValues8 | ValidIntValues9 | ValidIntValuesA | ValidIntValuesB |
    ValidIntValuesC | ValidIntValuesD | ValidIntValuesE | ValidIntValuesF

  inline def MaxValue: UByte = (-1: Byte)

  inline def wrap(b: Byte): UByte = b

  inline def apply(b: Byte): UByte = b
  inline def apply(i: ValidIntValues): UByte = i.toByte

  inline def clamp(b: Byte): UByte = if b < 0 then 0: Byte else b

  extension (b: UByte) {
    inline def unwrap: Byte = b
    inline def signed: Byte = b
    inline def toByte: Byte = b
  }

  extension (b: kse.maths.UByte) {
    inline def +(c: kse.maths.UByte): kse.maths.UInt  = UInt.wrap( (b.signed & 0xFF) + (c.signed & 0xFF) )
    inline def +(j: kse.maths.UInt ): kse.maths.UInt  = UInt.wrap( (b.signed & 0xFF) + j.signed )
    inline def +(k: kse.maths.ULong): kse.maths.ULong = ULong.wrap((b.signed & 0xFF) + k.signed)

    def +!(c: kse.maths.UByte): kse.maths.UByte =
      val ans = (b.signed & 0xFF) + (c.signed & 0xFF)
      UByte.wrap(if ans > 0xFF then (-1: Byte) else ans.toByte)

    inline def -(c: kse.maths.UByte): kse.maths.UInt  = UInt.wrap( (b.signed & 0xFF) - (c.signed & 0xFF) )
    inline def -(j: kse.maths.UInt ): kse.maths.UInt  = UInt.wrap( (b.signed & 0xFF) - j.signed )
    inline def -(k: kse.maths.ULong): kse.maths.ULong = ULong.wrap((b.signed & 0xFF) - k.signed)

    def -!(c: kse.maths.UByte): kse.maths.UByte =
      val ans = (b.signed & 0xFF) - (c.signed & 0xFF)
      UByte.wrap(if ans < 0 then (0: Byte) else ans.toByte)

    inline def *(c: kse.maths.UByte): kse.maths.UInt  = UInt.wrap( (b.signed & 0xFF) * (c.signed & 0xFF) )
    inline def *(j: kse.maths.UInt ): kse.maths.UInt  = UInt.wrap( (b.signed & 0xFF) * j.signed )
    inline def *(k: kse.maths.ULong): kse.maths.ULong = ULong.wrap((b.signed & 0xFF) * k.signed)

    def *!(c: kse.maths.UByte): kse.maths.UByte =
      val ans = (b.signed & 0xFF) * (c.signed & 0xFF)
      UByte.wrap(if ans > 0xFF then (-1: Byte) else ans.toByte)

    inline def /(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(b.signed & 0xFF) / UInt.wrap(c.signed & 0xFF)
    inline def /(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(b.signed & 0xFF) / j
    inline def /(k: kse.maths.ULong): kse.maths.ULong = ULong.wrap(b.signed & 0xFF) / k

    inline def /!(c: kse.maths.UByte): kse.maths.UByte = UByte.wrap(((b.signed & 0xFF) / (c.signed & 0xFF)).toByte)

    inline def %(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(b.signed & 0xFF) % UInt.wrap(c.signed & 0xFF)
    inline def %(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(b.signed & 0xFF) % j
    inline def %(k: kse.maths.ULong): kse.maths.ULong = ULong.wrap(b.signed & 0xFF) % k

    inline def %!(c: kse.maths.UByte): kse.maths.UByte = UByte.wrap(((b.signed & 0xFF) % (c.signed & 0xFF)).toByte)

    inline def |(c: kse.maths.UByte): kse.maths.UByte = UByte.wrap(((b.signed & 0xFF) | (c.signed & 0xFF)).toByte)

    inline def &(c: kse.maths.UByte): kse.maths.UByte = UByte.wrap(((b.signed & 0xFF) & (c.signed & 0xFF)).toByte)

    inline def ^(c: kse.maths.UByte): kse.maths.UByte = UByte.wrap(((b.signed & 0xFF) ^ (c.signed & 0xFF)).toByte)

    inline def <( c: kse.maths.UByte): Boolean = (b.signed & 0xFF) <  (c.signed & 0xFF)
    inline def <=(c: kse.maths.UByte): Boolean = (b.signed & 0xFF) <= (c.signed & 0xFF)
    inline def >=(c: kse.maths.UByte): Boolean = (b.signed & 0xFF) >= (c.signed & 0xFF)
    inline def >( c: kse.maths.UByte): Boolean = (b.signed & 0xFF) >  (c.signed & 0xFF)

    transparent inline def >>(inline j: Int | kse.maths.UInt): kse.maths.UInt = inline j match
      case x: Int => UInt.wrap((b.signed & 0xFF) >>> x)
      case y: kse.maths.UInt => UInt.wrap((b.signed & 0xFF) >>> y.signed)

    transparent inline def <<(inline j: Int | kse.maths.UInt): kse.maths.UInt = inline j match
      case x: Int => UInt.wrap((b.signed & 0xFF) << x)
      case y: kse.maths.UInt => UInt.wrap((b.signed & 0xFF) << y.signed)

    inline def max(c: kse.maths.UByte): kse.maths.UByte = if (b.signed & 0xFF) < (c.signed & 0xFF) then c else b
    inline def min(c: kse.maths.UByte): kse.maths.UByte = if (b.signed & 0xFF) > (c.signed & 0xFF) then c else b

    inline def toShort: Short           =           (b.signed & 0xFF).toShort
    inline def toChar:  Char            =           (b.signed & 0xFF).toChar
    inline def toInt:   Int             =            b.signed & 0xFF
    inline def toUInt:  kse.maths.UInt  =  UInt.wrap(b.signed & 0xFF)
    inline def toLong:  Long            =           (b.signed & 0xFF).toLong
    inline def toULong: kse.maths.ULong = ULong.wrap(b.signed & 0xFF)

    def pr: String = (b.signed & 0xFF).toString
  }
}


opaque type UInt = Int
object UInt {
  import java.lang.Integer.{divideUnsigned, remainderUnsigned, compareUnsigned, toUnsignedString}
  import java.lang.Long.{divideUnsigned => divUnsignedL, remainderUnsigned => modUnsignedL}

  inline def MaxValue: UInt = -1

  inline def wrap(i: Int): UInt = i

  inline def apply(i: Int): UInt = i

  inline def clamp(i: Int): UInt = if i < 0 then 0 else i

  extension (i: UInt) {
    inline def unwrap: Int = i
    inline def signed: Int = i
    inline def toInt:  Int = i
  }

  extension (i: kse.maths.UInt) {
    inline def +(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(i.signed + (c.signed & 0xFF) )
    inline def +(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(i.signed +  j.signed )
    inline def +(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.toLong + l.signed)

    def +!(j: kse.maths.UInt): kse.maths.UInt =
      val ans: Long = (i & 0xFFFFFFFFL) + (j & 0xFFFFFFFFL)
      UInt.wrap(if ans > 0xFFFFFFFFL then -1 else ans.toInt)

    inline def -(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(i.signed - (c.signed & 0xFF))
    inline def -(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(i.signed -  j.signed )
    inline def -(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.toLong -  l.signed)

    def -!(j: kse.maths.UInt): kse.maths.UInt =
      val ans: Long = (i & 0xFFFFFFFFL) - (j & 0xFFFFFFFFL)
      UInt.wrap(if ans < 0L then 0 else ans.toInt)

    inline def *(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(i.signed * (c.signed & 0xFF))
    inline def *(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(i.signed *  j.signed )
    inline def *(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.toLong *  l.signed)

    def *!(j: kse.maths.UInt): kse.maths.UInt =
      val ans: Long = (i & 0xFFFFFFFFL) * (j & 0xFFFFFFFFL)
      UInt.wrap(if ans < 0L || ans > 0xFFFFFFFFL then -1 else ans.toInt)

    inline def /(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(divideUnsigned(i.signed, (c.signed & 0xFF)))
    inline def /(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(divideUnsigned(i.signed,  j.signed))
    inline def /(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(divUnsignedL(i.toLong, l.signed))

    inline def /!(j: kse.maths.UInt): kse.maths.UInt = UInt.wrap(divideUnsigned(i.signed,  j.signed))

    inline def %(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(remainderUnsigned(i.signed, (c.signed & 0xFF)))
    inline def %(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(remainderUnsigned(i.signed,  j.signed))
    inline def %(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(modUnsignedL(i.toLong, l.signed))

    inline def %!(j: kse.maths.UInt): kse.maths.UInt = UInt.wrap(remainderUnsigned(i.signed,  j.signed))

    inline def |(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(i.signed | (c.signed & 0xFF))
    inline def |(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(i.signed |  j.signed )
    inline def |(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.toLong |  l.signed)

    inline def &(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(i.signed & (c.signed & 0xFF))
    inline def &(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(i.signed &  j.signed )
    inline def &(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.toLong &  l.signed)

    inline def ^(c: kse.maths.UByte): kse.maths.UInt  =  UInt.wrap(i.signed ^ (c.signed & 0xFF))
    inline def ^(j: kse.maths.UInt ): kse.maths.UInt  =  UInt.wrap(i.signed ^  j.signed )
    inline def ^(l: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.toLong ^  l.signed)

    inline def unary_~ : kse.maths.UInt = UInt.wrap(~i.signed)

    inline def <( j: kse.maths.UInt): Boolean = compareUnsigned(i.signed, j.signed) < 0
    inline def <=(j: kse.maths.UInt): Boolean = compareUnsigned(i.signed, j.signed) <= 0
    inline def >=(j: kse.maths.UInt): Boolean = compareUnsigned(i.signed, j.signed) >= 0
    inline def >( j: kse.maths.UInt): Boolean = compareUnsigned(i.signed, j.signed) > 0

    transparent inline def >>(inline j: Int | kse.maths.UInt): kse.maths.UInt = inline j match
      case x: Int => UInt.wrap(i.signed >>> x)
      case y: kse.maths.UInt => UInt.wrap(i.signed >>> y.signed)

    transparent inline def <<(inline j: Int | kse.maths.UInt): kse.maths.UInt = inline j match
      case x: Int => UInt.wrap(i.signed << x)
      case y: kse.maths.UInt => UInt.wrap(i.signed << y.signed)

    inline def max(j: kse.maths.UInt): kse.maths.UInt = if compareUnsigned(i.signed, j.signed) < 0 then j else i
    inline def min(j: kse.maths.UInt): kse.maths.UInt = if compareUnsigned(i.signed, j.signed) > 0 then j else i

    inline def toByte:  Byte            =            i.signed.toByte
    inline def toUByte: kse.maths.UByte = UByte.wrap(i.signed.toByte)
    inline def toShort: Short           =            i.signed.toShort
    inline def toChar:  Char            =            i.signed.toChar
    inline def toLong:  Long            =           (i.signed & 0xFFFFFFFFL)
    inline def toULong: kse.maths.ULong = ULong.wrap(i.signed & 0xFFFFFFFFL)

    inline def pr: String = toUnsignedString(i.signed)
  }

  given Ordering[kse.maths.UInt] = new {
    def compare(i: kse.maths.UInt, j: kse.maths.UInt): Int = java.lang.Integer.compareUnsigned(i.signed, j.signed)
  }
}


opaque type ULong = Long
object ULong {
  import java.lang.Long.{divideUnsigned, remainderUnsigned, compareUnsigned, toUnsignedString}

  inline def MaxValue: ULong = -1L

  inline def wrap(i: Long): ULong = i

  inline def apply(i: Long): ULong = i
  inline def apply(i: Int): ULong = (i & 0xFFFFFFFFL)

  inline def clamp(l: Long): ULong = if l < 0 then 0L else l

  extension (i: ULong) {
    inline def unwrap: Long = i
    inline def signed: Long = i
    inline def toLong: Long = i
  }

  extension (i: kse.maths.ULong) {
    inline def +(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(i.signed + (c.signed & 0xFF))
    inline def +(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(i.signed + (j.signed & 0xFFFFFFFFL))
    inline def +(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.signed + j.signed)

    def +!(j: kse.maths.ULong): kse.maths.ULong =
      if      ((i.signed | j.signed) & 0x8000000000000000L) == 0 then ULong.wrap(i.signed + j.signed)
      else if ((i.signed & j.signed) & 0x8000000000000000L) != 0 then ULong.wrap(-1L)
      else
        val ans = i.signed + j.signed
        ULong.wrap(if ans >= 0 then -1L else ans)

    inline def -(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(i.signed - (c.signed & 0xFF))
    inline def -(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(i.signed - (j.signed & 0xFFFFFFFFL))
    inline def -(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.signed - j.signed)
    
    def -!(j: kse.maths.ULong): kse.maths.ULong =
      if ((i.signed | j.signed) & 0x8000000000000000L) == 0 then
        val ans = i.signed - j.signed
        ULong.wrap(if ans < 0 then 0L else ans)
      else if ((i.signed & j.signed) & 0x8000000000000000L) != 0 then
        val ans = (i.signed & 0x7FFFFFFFFFFFFFFFL) - (i.signed & 0x7FFFFFFFFFFFFFFFL)
        ULong.wrap(if ans < 0 then 0L else ans)
      else
        ULong.wrap(if j.signed < 0 then 0L else i.signed - j.signed)

    inline def *(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(i.signed * (c.signed & 0xFF))
    inline def *(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(i.signed * (j.signed & 0xFFFFFFFFL))
    inline def *(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.signed * j.signed)

    def *!(j: kse.maths.ULong): kse.maths.ULong =
      if j.signed < 0 then
        if i.signed == 1 then j else ULong.wrap(if i.signed == 0 then 0L else -1L)
      else if i.signed < 0 then
        if j.signed == 1 then i else ULong.wrap(if j.signed == 0 then 0L else -1L)
      else
        val zi = java.lang.Long.numberOfLeadingZeros(i.signed)
        val zj = java.lang.Long.numberOfLeadingZeros(j.signed)
        if zi+zj >= 64 then ULong.wrap(i.signed * j.signed)
        else if zi+zj < 62 then ULong.wrap(-1L)
        else
          val a = if i.signed < j.signed then j.signed else i.signed
          val b = if i.signed < j.signed then i.signed else j.signed
          val hi = (a >>> 32) * b
          val lo = (a & 0xFFFFFFFFL) * b
          val hip = hi + (lo >>> 32)
          ULong.wrap(if hip > 0xFFFFFFFFL then -1L else lo + (hi << 32))
    
    inline def /(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(divideUnsigned(i.signed, c.signed & 0xFF))
    inline def /(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(divideUnsigned(i.signed, j.signed & 0xFFFFFFFFL))
    inline def /(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(divideUnsigned(i.signed, j.signed))

    inline def /!(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(divideUnsigned(i.signed, j.signed))
    
    inline def %(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(remainderUnsigned(i.signed, c.signed & 0xFF))
    inline def %(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(remainderUnsigned(i.signed, j.signed & 0xFFFFFFFFL))
    inline def %(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(remainderUnsigned(i.signed, j.signed))

    inline def %!(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(remainderUnsigned(i.signed, j.signed))    

    inline def |(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(i.signed | (c.signed & 0xFF))
    inline def |(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(i.signed | (j.signed & 0xFFFFFFFFL))
    inline def |(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.signed | j.signed)
    
    inline def &(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(i.signed & (c.signed & 0xFF))
    inline def &(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(i.signed & (j.signed & 0xFFFFFFFFL))
    inline def &(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.signed & j.signed)
    
    inline def ^(c: kse.maths.UByte): kse.maths.ULong = ULong.wrap(i.signed ^ (c.signed & 0xFF))
    inline def ^(j: kse.maths.UInt):  kse.maths.ULong = ULong.wrap(i.signed ^ (j.signed & 0xFFFFFFFFL))
    inline def ^(j: kse.maths.ULong): kse.maths.ULong = ULong.wrap(i.signed ^ j.signed)
    
    inline def unary_~ : kse.maths.ULong = ULong.wrap(~i.signed)

    inline def <( j: kse.maths.ULong): Boolean = compareUnsigned(i.signed, j.signed) < 0
    inline def <=(j: kse.maths.ULong): Boolean = compareUnsigned(i.signed, j.signed) <= 0
    inline def >=(j: kse.maths.ULong): Boolean = compareUnsigned(i.signed, j.signed) >= 0
    inline def >( j: kse.maths.ULong): Boolean = compareUnsigned(i.signed, j.signed) > 0

    transparent inline def >>(inline j: Int | kse.maths.UInt | kse.maths.ULong): kse.maths.ULong = inline j match
      case w: Int             => ULong.wrap(i.signed >>> w)
      case x: kse.maths.UInt  => ULong.wrap(i.signed >>> x.signed)
      case y: kse.maths.ULong => ULong.wrap(i.signed >>> y.signed)

    transparent inline def <<(inline j: Int | kse.maths.UInt | kse.maths.ULong): kse.maths.ULong = inline j match
      case w: Int             => ULong.wrap(i.signed << w)
      case x: kse.maths.UInt  => ULong.wrap(i.signed << x.signed)
      case y: kse.maths.ULong => ULong.wrap(i.signed << y.signed)

    inline def max(j: kse.maths.ULong): kse.maths.ULong = if compareUnsigned(i.signed, j.signed) < 0 then j else i
    inline def min(j: kse.maths.ULong): kse.maths.ULong = if compareUnsigned(i.signed, j.signed) > 0 then j else i

    inline def toByte:  Byte            =            i.signed.toByte
    inline def toUByte: kse.maths.UByte = UByte.wrap(i.signed.toByte)
    inline def toShort: Short           =            i.signed.toShort
    inline def toChar:  Char            =            i.signed.toChar
    inline def toInt:   Int             =            i.signed.toInt
    inline def toUInt:  kse.maths.UInt  =  UInt.wrap(i.signed.toInt)

    inline def pr: String = toUnsignedString(i.signed)
  }

  given Ordering[kse.maths.ULong] = new {
    def compare(i: kse.maths.ULong, j: kse.maths.ULong): Int = java.lang.Long.compareUnsigned(i.signed, j.signed)
  }
}


opaque type Vc = Long
object Vc {
  inline def wrap(l: Long): kse.maths.Vc = l
  inline def apply(x: Float, y: Float): kse.maths.Vc = 
    (java.lang.Float.floatToRawIntBits(x) & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(y).toLong << 32)
  inline def F(x: Float, y: Float): kse.maths.Vc =
    apply(x, y)
  inline def D(x: Double, y: Double): kse.maths.Vc = 
    apply(x.toFloat, y.toFloat)

  inline def zero: kse.maths.Vc = 0L
  final val NaN: kse.maths.Vc = apply(Float.NaN, Float.NaN)


  extension (v: Vc) {
    inline def x: Float = java.lang.Float.intBitsToFloat((v & 0xFFFFFFFFL).toInt)
    inline def y: Float = java.lang.Float.intBitsToFloat((v >>> 32).toInt)

    inline def xTo(f: Float): kse.maths.Vc =
      (v & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL)
    inline def xFn(inline f: Float => Float): kse.maths.Vc =
      (v & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f(v.x)) & 0xFFFFFFFFL)

    inline def yTo(f: Float): kse.maths.Vc =
      (v & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f).toLong << 32)
    inline def yFn(inline f: Float => Float): kse.maths.Vc =
      (v & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f(v.y)).toLong << 32)

    inline def isZero = (v & 0x7FFFFFFF7FFFFFFFL) == 0
    inline def isFinite = { val a = v & 0x7F8000007F800000L; (a.toInt != 0x7F800000) && ((a >> 32) != 0x7F800000) }
    inline def isNaN = java.lang.Float.isNaN(v.x) || java.lang.Float.isNaN(v.y)

    inline def swapped: kse.maths.Vc = (v >>> 32) | (v << 32)
    inline def cw: kse.maths.Vc = ((v >>> 32) | (v << 32)) ^ 0x8000000000000000L
    inline def ccw: kse.maths.Vc = ((v >>> 32) | (v << 32)) ^ 0x80000000L

    inline def unary_- : kse.maths.Vc = v ^ 0x8000000080000000L

    inline def unwrap: Long = v
  }

  extension (v: kse.maths.Vc) {
    final def rotate(angle: Float): kse.maths.Vc =
      val x = v.x
      val y = v.y
      val ca = jm.cos(angle)
      val sa = jm.sin(angle)
      Vc.D(x*ca - y*sa, y*ca + x*sa)
    inline def theta: Double = jm.atan2(v.y, v.x)

    final def lenSq: Double = { val a = v.x.toDouble; val b = v.y.toDouble; a*a + b*b }
    inline def len: Float = jm.sqrt(v.lenSq).toFloat

    inline def +(f: Float): kse.maths.Vc = Vc(v.x + f, v.y + f)
    inline def +(f: Float, g: Float): kse.maths.Vc = Vc(v.x + f, v.y + g)
    final def +(u: kse.maths.Vc): kse.maths.Vc = Vc(v.x + u.x, v.y + u.y)

    inline def -(f: Float): kse.maths.Vc = Vc(v.x - f, v.y - f)
    inline def -(f: Float, g: Float): kse.maths.Vc = Vc(v.x - f, v.y - g)
    final def -(u: kse.maths.Vc): kse.maths.Vc = Vc(v.x - u.x, v.y - u.y)

    inline def *(f: Float): kse.maths.Vc = Vc(v.x*f, v.y*f)
    inline def *(f: Float, g: Float): Double = v.x*f + v.y*g
    inline def *(u: kse.maths.Vc): Double = v.x*u.x + v.y*u.y
    inline def X(f: Float, g: Float): Double = v.x*g - v.y*f
    inline def X(u: kse.maths.Vc): Double = v.x*u.y - v.y*u.x

    def proj(f: Float, g: Float): kse.maths.Vc =
      val a = v.x
      val b = v.y
      val e = (a*f + b*g)/(f*f + g*g)
      Vc(f*e, g*e)
    def proj(u: kse.maths.Vc): kse.maths.Vc =
      val a = v.x
      val b = v.y
      val c = u.x
      val d = u.y
      val e = (a*c + b*d)/(c*c + d*d)
      Vc(c*e, d*e)

    def orth(f: Float, g: Float): kse.maths.Vc =
      val a = v.x
      val b = v.y
      val e = (a*f + b*g)/(f*f + g*g)
      Vc(a - f*e, b - g*e)
    def orth(u: kse.maths.Vc): kse.maths.Vc =
      val a = v.x
      val b = v.y
      val c = u.x
      val d = u.y
      val e = (a*c + b*d)/(c*c + d*d)
      Vc(a - c*e, b - d*e)

    def hat: kse.maths.Vc =
      val a = v.x.toDouble
      val b = v.y.toDouble
      val l2 = a*a + b*b
      if jm.abs(l2-1) < 3e-7f then v
      else if l2 == 0 then 0L
      else 
        val il = 1.0/jm.sqrt(l2)
        Vc.D(a*il, b*il)

    def normDot(f: Float, g: Float): Double =
      val a = v.x
      val b = v.y
      (a*f + b*g) / jm.sqrt((a*a + b*b)*(f*f + g*g)) match
        case w if w < -1 => -1
        case w if w > 1  =>  1
        case w           =>  w
    def normDot(u: kse.maths.Vc): Double =
      val a = v.x
      val b = v.y
      val c = u.x
      val d = u.y
      (a*c + b*d) / jm.sqrt((a*a + b*b)*(c*c + d*d)) match
        case w if w < -1 => -1
        case w if w > 1  =>  1
        case w           =>  w

    def distSq(f: Float, g: Float): Double =
      val a = (v.x - f).toDouble
      val b = (v.y - g).toDouble
      a*a + b*b
    def distSq(u: kse.maths.Vc): Double =
      val a = (v.x - u.x).toDouble
      val b = (v.y - u.y).toDouble
      a*a + b*b
    inline def dist(f: Float, g: Float): Float = jm.sqrt(v.distSq(f, g)).toFloat
    inline def dist(u: kse.maths.Vc): Float = jm.sqrt(v.distSq(u)).toFloat

    def angle(f: Float, g: Float): Double =
      val a = v.x.toDouble
      val b = v.y.toDouble
      val p = f.toDouble
      val q = g.toDouble
      val d = (a*p + b*q)/jm.sqrt((a*a + b*b)*(p*p + q*q)) match
        case c if c < -1 => -1
        case c if c > 1  =>  1
        case c           =>  c
      jm.acos(d) * jm.signum(a*q - b*p)
    def angle(u: kse.maths.Vc): Double =
      val a = v.x.toDouble
      val b = v.y.toDouble
      val p = u.x.toDouble
      val q = u.y.toDouble
      val d = (a*p + b*q)/jm.sqrt((a*a + b*b)*(p*p + q*q)) match
        case c if c < -1 => -1
        case c if c > 1  =>  1
        case c           =>  c
      jm.acos(d) * jm.signum(a*q - b*p)

    final def ===(u: kse.maths.Vc): Boolean =
      v.x == u.x && v.y == u.y

    def pr: String =
      val sb = new java.lang.StringBuilder
      sb append '['
      sb append v.x
      sb append ' '
      sb append v.y
      sb append ']'
      sb.toString

    def prf(fmt: String): String =
      val sb = new java.lang.StringBuilder
      sb append '['
      sb append fmt.format(v.x)
      sb append ' '
      sb append fmt.format(v.y)
      sb append ']'
      sb.toString
  }
}
extension (value: Float) {
  @targetName("Vc_prefix_add")
  inline def +(v: kse.maths.Vc): kse.maths.Vc = Vc(value + v.x, value + v.y)

  @targetName("Vc_prefix_sub")
  inline def -(v: kse.maths.Vc): kse.maths.Vc = Vc(value - v.x, value - v.y)

  @targetName("Vc_prefix_times")
  inline def *(v: kse.maths.Vc): kse.maths.Vc = Vc(value * v.x, value * v.y)

  inline def ~>(y: Float): kse.maths.Vc = Vc(value, y)
}


opaque type PlusMinus = Long
object PlusMinus {
  inline def wrap(l: Long): kse.maths.PlusMinus = l
  inline def apply(value: Float, error: Float): kse.maths.PlusMinus =
    (java.lang.Float.floatToRawIntBits(error) & 0x7FFFFFFFL) | (java.lang.Float.floatToRawIntBits(value).toLong << 32)
  inline def D(value: Double, error: Double): kse.maths.PlusMinus =
    apply(value.toFloat, error.toFloat)
  inline def exact(value: Float): kse.maths.PlusMinus =
    java.lang.Float.floatToRawIntBits(value).toLong << 32

  extension (pm: PlusMinus) {
    inline def value: Float = java.lang.Float.intBitsToFloat((pm >>> 32).toInt)
    inline def valueTo(value: Float): kse.maths.PlusMinus =
      (pm & 0xFFFFFFFFL) | ((java.lang.Float.floatToRawIntBits(value) & 0xFFFFFFFFL) << 32)
    inline def valueFn(f: Float => Float): kse.maths.PlusMinus =
      (pm & 0xFFFFFFFFL) | ((java.lang.Float.floatToRawIntBits(f(java.lang.Float.intBitsToFloat((pm >>> 32).toInt))) & 0xFFFFFFFFL) << 32)

    inline def error: Float = java.lang.Float.intBitsToFloat((pm & 0xFFFFFFFFL).toInt)
    inline def errorTo(error: Float): kse.maths.PlusMinus =
      (pm & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(error) & 0x7FFFFFFFL)
    inline def errorFn(f: Float => Float): kse.maths.PlusMinus =
      (pm & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f(java.lang.Float.intBitsToFloat((pm & 0xFFFFFFFFL).toInt))) & 0x7FFFFFFFL)

    inline def unary_- : kse.maths.PlusMinus = pm ^ 0x8000000000000000L

    inline def unwrap: Long = pm
  }

  extension (pm: kse.maths.PlusMinus) {
    def +(f: Float): kse.maths.PlusMinus = PlusMinus(pm.value + f, pm.error)

    def +(qm: kse.maths.PlusMinus): kse.maths.PlusMinus =
      val v = pm.value
      val u = qm.value
      val e = pm.error.toDouble
      val f = qm.error.toDouble
      PlusMinus(v + u, jm.sqrt((e*e + f*f).toDouble).toFloat)

    def -(f: Float): kse.maths.PlusMinus = PlusMinus(pm.value - f, pm.error)

    def -(qm: kse.maths.PlusMinus): kse.maths.PlusMinus =
      val v = pm.value
      val u = qm.value
      val e = pm.error.toDouble
      val f = qm.error.toDouble
      PlusMinus(v - u, jm.sqrt(e*e + f*f).toFloat)

    def *(f: Float): kse.maths.PlusMinus = PlusMinus(pm.value * f, pm.error * f)

    def *(qm: kse.maths.PlusMinus): kse.maths.PlusMinus =
      val v = pm.value.toDouble
      val u = qm.value.toDouble
      val e = pm.error.toDouble
      val f = qm.error.toDouble
      val a = v * u
      val vf = v * f
      val ue = u * e
      PlusMinus.D(a, jm.sqrt(vf*vf + ue*ue))

    def reciprocal: kse.maths.PlusMinus =
      val r = 1.0 / pm.value.toDouble
      PlusMinus.D(r, pm.error*r*r)

    def /(f: Float): kse.maths.PlusMinus = PlusMinus(pm.value / f, pm.error / f)

    def /(qm: kse.maths.PlusMinus): kse.maths.PlusMinus =
      val iu = 1.0 / qm.value.toDouble
      val r = pm.value.toDouble * iu
      val e = pm.error.toDouble
      val f = qm.error.toDouble
      PlusMinus.D(r, jm.sqrt(e*e + f*f*r*r) * iu)

    def sq: kse.maths.PlusMinus =
      val v = pm.value.toDouble
      val e = pm.error.toDouble
      if e == 0 then PlusMinus((v*v).toFloat, 0f)
      else           PlusMinus.D(v * v, v * e * kse.maths.NumericConstants.SqrtTwo)

    def sqrt: kse.maths.PlusMinus =
      val r = jm.sqrt(pm.value.toDouble)
      val e = pm.error.toDouble
      if e == 0 then PlusMinus(r.toFloat, 0f)
      else           PlusMinus.D(r, 0.5*e/r)

    def pow(exponent: Float): kse.maths.PlusMinus =
      val v = pm.value.toDouble
      val e = pm.error.toDouble
      val p = jm.pow(v, exponent)
      if e == 0 then      PlusMinus(p.toFloat, 0f)
      else if v != 0 then PlusMinus.D(p, e * jm.abs(exponent) * p / v)
      else                PlusMinus.D(p, e * jm.abs(exponent) * jm.pow(v, exponent - 1))

    def ===(qm: kse.maths.PlusMinus): Boolean =
      pm.value == qm.value && pm.error == qm.error

    def pr: String =
      val sb = new java.lang.StringBuilder
      sb append pm.value
      sb append " +- "
      sb append pm.error
      sb.toString

    def prf(fmt: String): String =
      val sb = new java.lang.StringBuilder
      sb append fmt.format(pm.value)
      sb append " +- "
      sb append fmt.format(pm.error)
      sb.toString
  }
}
extension (value: Float) {
  @targetName("PlusMinus_prefix_add")
  inline def +(pm: kse.maths.PlusMinus): kse.maths.PlusMinus = pm.valueTo(value + pm.value)

  @targetName("PlusMinus_prefix_sub")
  inline def -(pm: kse.maths.PlusMinus): kse.maths.PlusMinus = pm.valueTo(value - pm.value)

  @targetName("PlusMinus_prefix_times")
  inline def *(pm: kse.maths.PlusMinus): kse.maths.PlusMinus = PlusMinus(value * pm.value, value * pm.error)

  @targetName("PlusMinus_prefix_div")
  inline def /(pm: kse.maths.PlusMinus): kse.maths.PlusMinus =
    val v = pm.value.toDouble
    val u = value.toDouble
    val r = 1.0/v
    PlusMinus.D(u*r, pm.error.toDouble*u*r*r)

  inline def +-(error: Float): kse.maths.PlusMinus = PlusMinus(value, error)
}


opaque type Frac = Long
object Frac {
  inline def wrap(f: Long): Frac = f

  def apply(num: Int, denom: Int): Frac =
    if denom == 0 then
      if num == Int.MinValue then 0x7FFFFFFF80000000L
      else (num.toLong << 32) | 0x80000000L
    else if num == 0 then
      0x1L
    else
      val sh = java.lang.Integer.numberOfTrailingZeros(num | denom)
      if sh == 0 && (num == Int.MinValue || denom == Int.MinValue) then
        overflowApprox(num.toLong, denom.toLong)
      else
        val n = if denom < 0 then -(num   >> sh) else num   >> sh
        val d = if denom < 0 then -(denom >> sh) else denom >> sh
        gcdReduce(n, d)

  private[this] def gcdReduce(num: Int, den: Int): Long =
    val n = if num < 0 then -num else num
    var a = if n < den then den else n
    var b = if n < den then n else den
    var r = a % b
    while r > 1 do
      a = b
      b = r
      r = a % b
    if r == 0 && b > 1 then
      ((num / b).toLong << 32) | (den / b)
    else
      (num.toLong << 32) | den

  private[this] def gcdReduceBig(num: Long, den: Long): Long =
    val n = if num < 0 then -num else num
    var a = if n < den then den else n
    var b = if n < den then n else den
    var r = a % b
    while r > 1 do
      a = b
      b = r
      r = a % b
    if r == 0 && b > 1 then
      a = num / b
      b = den / b
      if a > Int.MinValue && a <= Int.MaxValue && b <= Int.MaxValue then
        (a << 32) | b
      else overflowApprox(a, b)
    else overflowApprox(num, den)

  private[this] def gcdReduceAny(num: Long, den: Long): Long =
    if num == 0 then return 0x1L
    var n = num
    var d = den
    val sh = java.lang.Long.numberOfTrailingZeros(n | d)
    if sh > 0 then
      n = n >> sh
      d = d >> sh
    if n > Int.MinValue && n <= Int.MaxValue && d <= Int.MaxValue then
      gcdReduce(n.toInt, d.toInt)
    else
      gcdReduceBig(n, d)

  private[this] def overflowApprox(num: Long, den: Long): Long =
    val n = if num < 0 then -num else num
    var a = if n < den then den else n
    var b = if n < den then n else den
    val f = a/b
    if f > 0x60000000L then
      if n < den then
        if f > 0xC0000000L then 0x80000001L
        else
          a = if num < 0 then -1 else 1
          b = if f > Int.MaxValue then Int.MaxValue else f
          (a << 32) | b | 0x80000000L
      else
        if num < 0 then 0x8000000180000001L
        else            0x7FFFFFFF80000001L
    else
      val x = a.toDouble / b.toDouble
      val sh = (64 - java.lang.Long.numberOfLeadingZeros(a)) - 31
      val yb = java.lang.Long.numberOfLeadingZeros(b)
      a = a >> sh
      b = b >> sh
      val g = a.toDouble / b.toDouble
      val better = jm.abs(x - f) - jm.abs(x - g)
      if !(better >= 0.0) then
        a = f
        b = 1
      if num < 0 then
        if n < den then b = -b
        else            a = -a
      if n < den then (b << 32) | (a & 0x7FFFFFFFL) | 0x80000000L
      else            (a << 32) | (b & 0x7FFFFFFFL) | 0x80000000L

  extension (f: Frac) {
    inline def unwrap: Long = f

    inline def numerator: Int = ((f: Long) >>> 32).toInt
    inline def denominator: Int = ((f: Long) & 0x7FFFFFFFL).toInt
    inline def numer: Int = ((f: Long) >>> 32).toInt
    inline def denom: Int = ((f: Long) & 0x7FFFFFFFL).toInt
    inline def numerL: Long = (f: Long) >> 32
    inline def denomL: Long = (f: Long) & 0x7FFFFFFFL

    inline def isExact: Boolean = ((f: Long) & 0x80000000L) == 0
    inline def inexact: Boolean = ((f: Long) & 0x80000000L) != 0
    inline def overflowBit: Long = (f: Long) & 0x80000000L

    inline def overflowed: Frac = (f: Long) | 0x80000000L
    inline def noOverflow: Frac = (f: Long) & 0xFFFFFFFF7FFFFFFFL

    inline def toDouble: Double =  ((f: Long) >> 32).toDouble / ((f: Long) & 0x7FFFFFFFL).toDouble
    inline def toFloat:  Float  = (((f: Long) >> 32).toDouble / ((f: Long) & 0x7FFFFFFFL).toDouble).toFloat
    inline def f64: Double      =  ((f: Long) >> 32).toDouble / ((f: Long) & 0x7FFFFFFFL).toDouble
    inline def f32: Float       = (((f: Long) >> 32).toDouble / ((f: Long) & 0x7FFFFFFFL).toDouble).toFloat
  }

  extension (f: kse.maths.Frac) {
    def abs: kse.maths.Frac =
      val n = f.numerL
      if n < 0 then Frac.wrap(((-f.numerL) << 32) | (f.unwrap & 0xFFFFFFFFL))
      else f

    def unary_- : kse.maths.Frac =
      Frac.wrap(((-f.numerL) << 32) | (f.unwrap & 0xFFFFFFFFL))

    def toInt: Int = f.numerator / f.denominator

    def floor: Int =
      val n = f.numerator
      val d = f.denominator
      if      d == 1 then n
      else if n >= 0 then n / d
      else               (n / d) - 1

    def ceil: Int =
      val n = f.numerator
      val d = f.denominator
      if      d == 1 then n
      else if n < 0  then n / d
      else               (n / d) + 1

    def round: Int =
      val n = f.numerator
      val d = f.denominator
      val v = n/d
      val e = 2*(n - v*d)
      if      e >  d then v+1
      else if e < -d then v-1
      else                v

    def +(i: Int): kse.maths.Frac =
      val n = f.numerator
      val d = f.denominator
      val ans = n + i.toLong * d
      if ans > Int.MaxValue || ans <= Int.MinValue then overflowApprox(ans, d)
      else Frac.wrap((ans << 32) | f.overflowBit | d.toLong)

    def +(g: Frac): kse.maths.Frac =
      val nf = f.numerL
      val df = f.denomL
      val ng = g.numerL
      val dg = g.denomL
      val ans =
        if df == dg then gcdReduceAny(nf + ng, df)
        else gcdReduceAny(nf*dg + ng*df, df * dg)
      ans | f.overflowBit | g.overflowBit

    def -(i: Int): kse.maths.Frac =
      val n = f.numerator
      val d = f.denominator
      val ans = n - i.toLong * d
      if ans > Int.MaxValue || ans <= Int.MinValue then overflowApprox(ans, d)
      else Frac.wrap((ans << 32) | f.overflowBit | d.toLong)

    def -(g: Frac): kse.maths.Frac =
      f + Frac.wrap(((-g.numerator).toLong << 32) | (g.unwrap & 0xFFFFFFFFL))

    def *(i: Int): kse.maths.Frac =
      gcdReduceAny(f.numerL * i, f.denomL) | f.overflowBit

    def *(g: Frac): kse.maths.Frac =
      gcdReduceAny(f.numerL * g.numerL, f.denomL * g.denomL) | f.overflowBit | g.overflowBit

    def /(i: Int): kse.maths.Frac =
      val ans =
        if i == 0 then apply(f.numer, 0)
        else if i < 0 then gcdReduceAny(-f.numerL, -f.denomL * i.toLong)
        else               gcdReduceAny( f.numerL,  f.denomL * i)
      ans | f.overflowBit

    def /(g: Frac): kse.maths.Frac =
      val ng = g.numerL
      val ans =
        if ng == 0 then apply(f.numer, 0)
        else if ng < 0 then gcdReduceAny(-f.numerL * g.denomL, -ng * f.denomL) 
        else                gcdReduceAny( f.numerL * g.denomL,  ng * f.denomL)
      ans | f.overflowBit | g.overflowBit

    def reciprocal: kse.maths.Frac =
      val n = f.numerL
      val d = f.denomL
      val ob = if n == 0 then 0x80000000L else f.overflowBit
      if n < 0 then ((-d) << 32) | (-n) | ob
      else          (  d  << 32) |   n  | ob

    def =~=(g: kse.maths.Frac): Boolean =
      f.numerL * g.denomL == g.numerL * f.denomL

    def <(g: kse.maths.Frac): Boolean =
      f.numerL * g.denomL < g.numerL * f.denomL

    def <=(g: kse.maths.Frac): Boolean =
      f.numerL * g.denomL <= g.numerL * f.denomL

    def >=(g: kse.maths.Frac): Boolean =
      f.numerL * g.denomL >= g.numerL * f.denomL

    def >(g: kse.maths.Frac): Boolean =
      f.numerL * g.denomL > g.numerL * f.denomL

    def max(g: kse.maths.Frac): kse.maths.Frac =
      Frac.wrap(if f < g then g.unwrap | f.overflowBit else f.unwrap | g.overflowBit)

    def min(g: kse.maths.Frac): kse.maths.Frac =
      Frac.wrap(if f > g then g.unwrap | f.overflowBit else f.unwrap | g.overflowBit)

    def pr: String =
      if f.isExact then s"${f.numerator}/${f.denominator}"
      else            s"~(${f.numerator}/${f.denominator})"
  }

  def divide(value: Int, f: kse.maths.Frac): kse.maths.Frac =
    val n = f.numerL
    val ans =
      if n == 0 then apply(value, 0)
      else if n < 0 then gcdReduceAny(-(value.toLong) * f.denomL, -n)
      else               gcdReduceAny(  value.toLong  * f.denomL,  n)
    ans | f.overflowBit

  private[this] def uncheckedApply(a: Int, b: Int, negative: Boolean, flip: Boolean): kse.maths.Frac =
    if flip then
      if negative then Frac.wrap(((-b).toLong << 32) | a.toLong)
      else             Frac.wrap((  b .toLong << 32) | a.toLong)
    else
      if negative then Frac.wrap(((-a).toLong << 32) | b.toLong)
      else             Frac.wrap((  a .toLong << 32) | b.toLong)

  def approx(d: Double, maxSteps: Int = 40, tolerance: Double = 1e-12, markInexact: Boolean = false): kse.maths.Frac =
    if d.nan then return Frac.wrap(0x80000001L)
    val e = d.abs
    if e < 2.3283064365386963E-10 then
      if markInexact && e > tolerance then return Frac.wrap(0x80000001L)
      else return Frac.wrap(0x1L)
    if e > Int.MaxValue then
      if e.inf then
        if d > 0 then return Frac.wrap(0x0000000180000000L)
        else          return Frac.wrap(0xFFFFFFFF80000000L)
      else
        val n = if d > 0 then Int.MaxValue else -Int.MaxValue
        if markInexact then
          val err = (1 - Int.MaxValue/e).abs
          if err > tolerance then return Frac.wrap((n.toLong << 32) | 0x80000001L)
          else                    return Frac.wrap((n.toLong << 32) | 0x1L)
    val f = if e < 1.0 then 1.0/e else e
    val fi = if e < 1.0 then e else 1.0/e
    var u = f.floor.toInt
    var v = 1
    var x = f.ceil.toInt
    var y = 1
    if x == u then return uncheckedApply(u, 1, d < 0, f != e)
    if (1 - x*fi).abs <= tolerance then return uncheckedApply(x, 1, d < 0, f != e)
    if (1 - u*fi).abs <= tolerance then return uncheckedApply(u, 1, d < 0, f != e)
    // At this point we're sure that the approximation is nontrivial: not just a simple integer or reciprocal thereof.
    // We traverse the >= 1 half of the Stern-Brocot tree (see Wikipedia and Bhavsar & Thaker, Int. J. Res. Rev 8:130 (2019))
    // with the observation that the tree is willow-shaped: when traversing mediants between a/b and c/d, you
    // only approach one or the other at 1/x-like speed because your steps are, for example (a+c)/(b+d), (2a+c)/(2b+d),
    // (3a+c)/(3b+d) and so on.  To avoid this, we solve (a+k*c)/(b+k*d) = f, where f is the value we want to approximate.
    // This gives k = (f*b-a)/(c-f*d).  If k > 1, we will keep traversing rightwards until we get to k+ = ceil(k) which
    // will then be too big; so our bounding values will then be (a + c*k-)/(b + d*k-) and (a + c*k+)/(b + d*k+)
    // where k- = k+ - 1; and in the leftward direction we can let j = 1/k and do the same with (a*j + c)/(b*j + d).
    // This reduces the 1/x-like part of the tree to constant time, allowing us to accumulate digits at a roughly
    // constant rate.  (A similar scheme appears to be defined in the Wikipedia entry on Farey sequences, but I
    // have not checked to see if it is equivalent.)
    var i = maxSteps
    var inrange = true
    while i > 0 && inrange do
      i -= 1
      val k = (f*v - u)/(x - f*y)
      if (k - 1).abs <= tolerance then
        if u.toLong + x.toLong <= Int.MaxValue then return uncheckedApply(u + x, v + y, d < 0, f != e)
        else inrange = false
      else if k > 1 then
        val kp = k.ceil
        if u + kp*x > Int.MaxValue then
          inrange = false
          if u.toLong + x.toLong <= Int.MaxValue then
            val m = ((Int.MaxValue.toLong - u)/x).toInt
            u += x*m
            v += y*m
          else
            {}  // Do nothing: keep existing values
        else
          val n = kp.toInt
          val m = n-1
          val tu = u
          val tv = v
          u += m*x
          v += m*y
          x = tu + n*x
          y = tv + n*y
          val erxy = (1 - x*fi/y).abs
          val eruv = (1 - u*fi/v).abs
          if erxy < eruv then
            if erxy <= tolerance then return uncheckedApply(x, y, d < 0, f != e)
          else
            if eruv <= tolerance then return uncheckedApply(u, v, d < 0, f != e)
      else
        val j = 1.0/k
        val jp = j.ceil
        if jp*u + x > Int.MaxValue then
          inrange = false
          if u.toLong + x.toLong <= Int.MaxValue then
            val m = ((Int.MaxValue.toLong - x)/u).toInt
            x += u*m
            y += v*m
          else
            {}  // Do nothing: keep existing values
        else
          val n = jp.toInt
          val m = n-1
          val tx = x
          val ty = y
          x += m*u
          y += m*v
          u = tx + n*u
          v = ty + n*v
          val erxy = (1 - x*fi/y).abs
          val eruv = (1 - u*fi/v).abs
          if erxy < eruv then
            if erxy <= tolerance then return uncheckedApply(x, y, d < 0, f != e)
          else
            if eruv <= tolerance then return uncheckedApply(u, v, d < 0, f != e)
    val erxy = (1 - x*fi/y).abs
    val eruv = (1 - u*fi/v).abs
    if erxy < eruv then
      val ans = uncheckedApply(x, y, d < 0, f != e)
      if markInexact && erxy > tolerance then ans.overflowed else ans
    else
      val ans = uncheckedApply(u, v, d < 0, f != e)
      if markInexact && eruv > tolerance then ans.overflowed else ans

  given Ordering[kse.maths.Frac] = new {
    def compare(f: kse.maths.Frac, g: kse.maths.Frac) =
      if f == g then 0
      else
        val a = f.numerL * g.denomL
        val b = g.numerL * f.denomL
        if      a == b then  0
        else if a < b  then -1
        else                 1
  }
}
extension (value: Int) {
  @targetName("Frac_over")
  inline def over(denom: Int): kse.maths.Frac = Frac(value, denom)

  @targetName("Frac_plus")
  inline def +(f: kse.maths.Frac): kse.maths.Frac = f + value

  @targetName("Frac_minus")
  inline def -(f: kse.maths.Frac): kse.maths.Frac =
    (-f) + value

  @targetName("Frac_times")
  inline def *(f: kse.maths.Frac): kse.maths.Frac =
    f * value

  @targetName("Frac_divide")
  inline def /(f: kse.maths.Frac): kse.maths.Frac =
    Frac.divide(value, f)
}
