# kse3 maths

The maths module is the numeric toolbox: the functions and types that ought to be a method call away and usually
aren't.  Most of it is extension methods on the primitives, so `x.sqrt`, `x.clamp(lo, hi)`, and `a +# b` read as
arithmetic, and the rest is a set of small, fast types (unsigned integers, durations, vectors, colours) and a few
larger tools (random numbers, hashing, statistics, sketches, fits).  Nothing here depends on anything outside the
JDK, and the numerics are written to be right at the edges: saturating where overflow is possible, NaN-aware where
NaN can arrive.

`import kse.maths.{given, *}` brings in the extensions and types below.  Four sub-packages hold the rest:
`kse.maths.packed` for the bit, byte, and hex accessors, `kse.maths.fitting` for the line and circle fits,
`kse.maths.colours` for colour, and `kse.maths.stringmaths` for numbers as text.  `java.lang.Math` is imported as `jm` throughout kse3 for the few things that stay plain
function calls; `scala.math` is never used.

<!-- guide examples: maths/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `maths/test/src/GuideExamples.scala`; `mill guides.run` verifies
that the two copies are identical.

## What's here

- **Numbers**: `sq`, `sqrt`, `clamp`, `in`, `between`, `finite`, and the special functions, as methods on the number.
- **Arithmetic that can't overflow quietly**: `+#` saturates, `+!` throws, and division never divides by zero by accident.
- **Unsigned and packed**: `UByte` through `ULong` via `.u`, and bits, bytes, and hex digits addressed by index.
- **Time**: `NanoDuration`, `DoubleDuration`, and instants with saturating arithmetic; literals like `1500.ms`; `tic`/`toc`; `Now`.
- **Random numbers**: `Prng` with `D`, `Z`, `%`, `gaussian`, `sample`, `shuffle`, and dice via `AutoPrng`.
- **Hashing**: `MurmurHash`, `XxHash`, `Crc32` over bytes, strings, buffers, or `Mem`, and incremental hashers for several things at once.
- **Statistics**: `Est` for mean and variance in one pass, `median` and `quantile` on arrays, `Hist`, `Dist`, `Ranks`, `Bootstrap`, `Changepoint`.
- **Streams**: `UDDSketch` for quantiles of a stream, `Adwin` for noticing when its mean shifts, `Radwin` for locating where.
- **Fitting**: `FitLine` and `FitCirc` you add points to, `TheilSen` for a robust line, `Roots` for polynomials, `Smoothing` for LOESS and rolling windows.
- **Vectors and matrices**: `Vec2D`, `Vec3D`, the packed `Vc`, small matrices and affine transforms, `PlusMinus` for a value with an error.
- **Colour**: `Rgb` with the CSS names, alpha via `Argb`, `Oklab` for perceptual mixing, `Ehsv` for hue and saturation.
- **Numbers as text**: `say` makes them and `Grok` reads them; underneath are `Ryu`, `Parse`, and `SemanticOrder`.

## Numbers

The everyday functions are methods on the number itself: `x.sq`, `x.sqrt`, `x.cbrt`, `x.log`, `x.log2`, `x.exp`,
`x.sin`, `x.atan2(y)`, `x.abs`, `x.sign`.  Predicates `finite`, `nan`, and `inf` do what their names say.  A few
verbs replace little idioms: `x.clamp(lo, hi)` for the min-max sandwich, `x.in(lo, hi)` for a range test,
`x.checkIn(lo, hi)` to throw if outside, `f.between(lo, hi)` to interpolate a fraction into a range and
`x.wherein(lo, hi)` to invert that, `x.closeTo(y)` for approximate equality (an absolute tolerance, and a fractional
one once the values exceed 1), and `x === y` for equality that treats two NaNs as equal.  The statistical special functions are there too: `erf`, `erfc`, `gamma`, `lnGamma`,
`cdfNormal`, `icdfNormal`, `logistic`, `logit`, `entropy`, and the chi-squared, Student-t, F, and Kolmogorov
distributions in `NumericFunctions`.

**Reach for these when** you'd write `jm.sqrt(x * x + 1)`, nest `jm.max` in `jm.min`, or open `scala.math` (which
kse3 never uses; `jm` is `java.lang.Math` where a plain call is wanted).

Plain Scala:

<!-- guide: numbers.plain -->
```scala
val d = jm.sqrt(x * x + 1)
val c = jm.max(lo, jm.min(hi, x))
val t = lo + 0.25 * (hi - lo)
val finite = !x.isNaN && !x.isInfinite
```

kse3:

<!-- guide: numbers.kse3 -->
```scala
val d = (x.sq + 1).sqrt
val c = x.clamp(lo, hi)
val t = 0.25.between(lo, hi)
val finite = x.finite
```

`zsqrt` is `sqrt` that gives 0 for negative input, and `entropyInv`, `logisticInv`, and `erfInv` invert their
partners.  `Int` and `Long` have `clamp`, `in`, `checkIn`, and the bit-level `bitCount`, `highBit`, `lowBit`,
`leadingZeros`, and `rotl`.  `Bf16` is a 16-bit brain float, for storage rather than arithmetic.

Full API: `maths/src/Maths.scala`, the `extension (d: Double)` block and its neighbours; `NumericFunctions` and
`NumericConstants` at the top of the same file.

## Arithmetic that can't overflow quietly

`Int` and `Long` arithmetic wraps around on overflow and says nothing.  kse3 marks the two alternatives with a
character: `a +# b`, `-#`, `*#`, `/#`, and `%#` saturate at `MinValue` or `MaxValue`, and `/#` and `%#` also give a
saturated or zero answer for a zero divisor instead of throwing; `a +! b`, `-!`, `*!`, and `/!` throw
`ArithmeticException` on overflow, as `Math.addExact` does.  The same operators appear on durations and unsigned
types, so `#` and `!` mean the same thing wherever you see them.

**Reach for these when** a sum or product could exceed the type, and the right answer is "as big as it gets"
(a saturating counter, a clamped index) or "stop now" (a size that must be exact).

Plain Scala:

<!-- guide: arithmetic.plain -->
```scala
val exact = jm.addExact(a, b)
val wide = a.toLong * b
val clamped = if wide > Int.MaxValue then Int.MaxValue else if wide < Int.MinValue then Int.MinValue else wide.toInt
```

kse3:

<!-- guide: arithmetic.kse3 -->
```scala
val exact = a +! b                        // throws ArithmeticException on overflow
val clamped = a *# b                      // saturates at Int.MinValue or Int.MaxValue
```

Full API: `maths/src/Maths.scala` for `#`, `maths/src/OverloadedExtensions.scala` for `!`.

## Unsigned and packed

`UByte`, `UShort`, `UInt`, and `ULong` are the primitives reinterpreted as unsigned: same bits, no allocation, and
arithmetic, comparison, and printing that treat the top bit as magnitude.  `.u` on a signed value gives the unsigned
view; `.signed` or `.toInt` and friends go back, widening correctly.  For bit-level work, `import kse.maths.packed.*`
gives every integer type `bit(i)`, `bits(i0, iN)`, `byte(i)`, `hex(i)`, `short(i)`, and `char(i)` accessors with
matching `bitTo(i)(v)` setters, indexed from the low end and checked at compile time when the index is a literal.
`Pack.I(b0, b1, b2, b3)` assembles, and `Bx4`, `Sx2`, `Cx2`, and the like are packed tuples of small values in one
`Int` or `Long`.

**Reach for these when** a byte from a wire or a file is a count, not a signed number; when a format packs fields
into an integer; or when you'd otherwise write `& 0xFF` and shifts by hand.

<!-- guide: unsigned.kse3 -->
```scala
val ub = b.u                              // a UByte: the same bits, read as 0 to 255
val n = ub.toInt                          // 200 where b.toInt would be -56
val text = i.u.pr                         // printed unsigned: "4294967295" for -1
val wide = i.u.toLong                     // 4294967295L for -1
val third = i.byte(2)                     // byte 2 of 4, counting from the low end
val set = i.bitTo(0)(1)                   // i with bit 0 set
val ones = i.bitCount
```

Each unsigned type has a `Sorting.Order` in its companion, so arrays of them sort correctly.  `Hex` is the type of
one hex digit, `Bit` of one bit, and `x.bitString` shows the bits.

Full API: `maths/src/Maths.scala` for the unsigned types; `maths/src/Packed.scala` for the accessors and packed
tuples.

## Time

kse3 keeps time in four small types.  `NanoDuration` is a `Long` of nanoseconds whose arithmetic saturates instead
of overflowing; `DoubleDuration` is seconds in a `Double` for when fractions matter more than exactness;
`NanoInstant` is a `System.nanoTime` reading and `DoubleInstant` an epoch time in seconds.  `java.time.Duration` and
`Instant` are still the interchange types, and they gain `+`, `-`, `*`, comparisons, `age`, and `to`.  Literals build
durations: `1500.ms` and `2.h` are `java.time.Duration`s, `1.5.s` (a `Double` receiver) is a `DoubleDuration`, and
`20.ms_nano` is a `NanoDuration`.

**Reach for these when** you'd subtract two `System.nanoTime` calls, call `toNanos` or `toMillis` (which throw or
truncate), do arithmetic on `Instant` through `plus` and `Duration.between`, or write `Duration.ofMillis(1500)`.

Plain Scala:

<!-- guide: time.plain -->
```scala
val timeout = java.time.Duration.ofMillis(1500)
val t0 = System.nanoTime
work()
val elapsedNs = System.nanoTime - t0
val deadline = java.time.Instant.now.plus(timeout)
```

kse3:

<!-- guide: time.kse3 -->
```scala
val timeout = 1500.ms                     // a java.time.Duration; 1.5.s would be a DoubleDuration
val t0 = tic()
work()
val elapsed = t0.toc()                    // a NanoDuration: a Long of nanoseconds that saturates
val deadline = Now() + timeout
```

<!-- guide: durations.kse3 -->
```scala
val n = d.nano                            // saturates where d.toNanos would throw
val secs = n.double                       // seconds as a Double
val twice = d + d                         // java.time.Duration gains +, -, *, comparisons
val soon = d < 2.s
```

Convert a `java.time.Duration` with `.nano` (saturating) or `.checkedNano` (throwing), never `toNanos`; the
saturating form is what lets "wait forever" be spelled as a very long wait, `1e9.days`, without an overflow.
`NanoDuration` and `DoubleDuration` have `into` (a count of whole units: `n.into.ms`), `round`, `floor`, and `ceil`
(to a whole unit), and the `#` and `!` arithmetic.  `Now()` is `Instant.now`, with `Now.nanos()`, `Now.utc()`, and
`Now.local()` beside it, and `Cal.utc(2026, 9, 16)` builds a date without the `java.time` ceremony.

Full API: `maths/src/Temporal.scala`; the `java.time` extensions are in `maths/src/OverloadedExtensions.scala`.

## Random numbers

`Prng()` is a PCG64 generator: fast, seedable, one `Long` of state, and serializable as that `Long`.  `rng.D` is a
`Double` in `[0, 1)`, `rng.I` and `rng.L` full-range integers, `rng.Z` a coin, `rng % n` an `Int` in `0 until n`,
`rng.gaussian` a standard normal, `rng.uniform(lo, hi)` a range.  It also does the collection work: `sample(xs)`
picks one element, `sample(k)(xs)` a subset, `shuffle(xs)` in place, `chooseIndices(n, k)` a random subset of
indices, and `stringFrom(letters, n)` a random string.  A `given AutoPrng` puts a generator in scope for the short
forms: `n.roll` is a die, `3 d 6` a dice sum, `xs.randomFill()`, `xs.shuffle()`.

**Reach for these when** you'd use `scala.util.Random` or `java.util.Random`, or write index-picking and shuffling
by hand.

Plain Scala:

<!-- guide: random.plain -->
```scala
val r = new scala.util.Random(seed)
val x = r.nextDouble()
val k = r.nextInt(6)
val coin = r.nextBoolean()
val g = r.nextGaussian()
val pick = xs(r.nextInt(xs.length))
```

kse3:

<!-- guide: random.kse3 -->
```scala
val rng = Prng(seed)                      // Pcg64: fast, one Long of state, seedable
val x = rng.D                             // uniform in [0, 1)
val k = rng % 6                           // uniform in 0 until 6
val coin = rng.Z
val g = rng.gaussian
val pick = rng.sample(xs)
given AutoPrng = rng.givable              // lets the short forms find the generator
val die = 6.roll                          // 1 to 6
```

`Pcg64` is the default; `Xo128` and `Xo256` (xoshiro) and `ShiftMix64` are there when a different generator is
wanted, and all share the `Prng` API.  A `Prng` is not thread-safe: give each thread its own, seeded from one.

Full API: `maths/src/Random.scala`.

## Hashing

Six hash functions, each an object that hashes a range of a byte array, a `ByteBuffer`, the chars of a `String`, or a
`Mem`, with an optional seed: `MurmurHash` (32 and 128 bits), `XxHash` (32 and 64), `Crc32` and `Crc32C`, and the
trivial `SumHash` and `XorHash` for when mixing doesn't matter.  To hash several things into one value, take an
incremental hasher from `MakeHasher` (`x32`, `x64`, `m32`, `m128`, `c32`, and so on), call `begin(seed)`, feed it
with `append`, `appendInt`, `appendLong`, or `appendRaw(mem)`, and take `result()`.  `PairHash` and its larger
siblings run two or more hashers over the same input in one pass; `PreseededHash` fixes a seed.

**Reach for these when** you'd use `hashCode` for anything other than a hash table, `MessageDigest` for a checksum
that needn't be cryptographic, or `java.util.zip.CRC32`.

Plain Scala:

<!-- guide: hashing.plain -->
```scala
val h = java.util.Arrays.hashCode(xs)                 // what HashMap uses: fast, weakly mixed
val crc = new java.util.zip.CRC32()
crc.update(xs)
val check = crc.getValue
```

kse3:

<!-- guide: hashing.kse3 -->
```scala
val h = MurmurHash.hash32(xs, 0, xs.length)           // a well-mixed 32-bit hash of the bytes
val big = XxHash.hash64(s, 0, s.length)               // 64 bits, straight from a String's chars
val check = Crc32.hash32(xs, 0, xs.length)
val mixed = MakeHasher.x64.begin(1234L).appendInt(xs.length).append(s, 0, s.length).result()
```

Multi-byte values append little-endian, and a `String` hashes its UTF-16 chars, so the same text gives the same
hash whether it arrives as a `String`, an `Array[Char]`, or a `Mem[Char]`.  Each object keeps a cached hasher, so
the one-shot forms allocate nothing in steady state.

Full API: `maths/src/Hash.scala`.

## Statistics

`Est` is the workhorse: `Est of xs` gives mean, variance, `sd`, `sem`, and `cv` from one pass over an array, and
`Est.M()` is the running version you `+=` values into (and `-=` out of, or `addWithWeight`), with `++` to combine
two.  `pmSD` and `pmSEM` hand the mean back as a `PlusMinus`.  Arrays of `Double` get `median`, `quantile(p)`, and
`iqr`, all R type-7 over a sorted copy of the finite values, with range variants.  `Hist(bins)` counts integers
with out-of-range values kept as `outliers`, `Hist2` does two dimensions, and `Dist` is a discrete distribution
(`fromValues`, `fromCounts`, `fromHist`) with `crossEntropy`.  `Ranks.of(xs)` gives average ranks with Wilcoxon tie
handling, `Bootstrap` resamples any statistic you can accumulate, and `Changepoint.bridge(scores, minSeg)` finds a
single change in the mean of a series, with a p-value that is distribution-free when you feed it ranks.

**Reach for these when** you'd write `xs.sum / xs.length` and then a second loop for the variance, sort an array just
to find its median, or reach for a statistics library for something that fits in a few hundred lines.

Plain Scala:

<!-- guide: stats.plain -->
```scala
val n = xs.length
val mean = xs.sum / n
val sd = jm.sqrt(xs.map(x => (x - mean) * (x - mean)).sum / (n - 1))
val sorted = xs.sorted
val median = if n % 2 == 1 then sorted(n / 2) else (sorted(n / 2 - 1) + sorted(n / 2)) / 2
```

kse3:

<!-- guide: stats.kse3 -->
```scala
val est = Est of xs                      // mean, sd, sem, and variance, in one pass
val median = xs.median                   // R type-7, over a sorted copy of the finite values
```

<!-- guide: running.kse3 -->
```scala
val est = Est.M()                        // a running mean and variance you add to
val hist = Hist(10)                      // ten bins; out-of-range values count as outliers
stream.foreach: x =>
  est += x
  hist += (x * 10).toInt
val boot = Bootstrap(200)(Prng(1))(0, xs.length)(Est.M())((e, i) => e += xs(i))
val meanPm = boot.pm(_.mean)             // the mean with its bootstrap standard error
```

Full API: `maths/src/Stats.scala`.

## Streams

Two tools for data that keeps arriving.  `UDDSketch` is a quantile sketch: feed it values with `+=` and ask for
`quantile(q)`, `median`, `iqr`, or `fractionBelow(x)` at any time, with a relative error you choose (1% by default)
and memory that stays at a few kilobytes no matter how many values pass through.  It also keeps `mean`, `sd`, `min`,
`max`, and `count`, merges with another sketch, and handles negative and zero values without losing resolution on
the positive bulk.  `Adwin` watches the mean: `add(x)` returns `true` when the recent values differ from the older
ones by more than a variance-aware bound, and drops the older ones, so `mean` and `width` describe the current
regime.  `Radwin` does the retrospective version on a buffer of recent values, locating a change in level or in
spread by rank statistics, so one wild outlier cannot fool it.

**Reach for these when** you'd store every latency to compute a percentile later, or compare a moving average
against a threshold by hand.

<!-- guide: streams.kse3 -->
```scala
val sk = UDDSketch()                     // quantiles to 1% relative error in a few kilobytes
val ad = Adwin()                         // an adaptive window that drops old data when the mean shifts
var shifted = false
latencies.foreach: x =>
  sk += x
  if ad.add(x) then shifted = true
```

`Adwin` watches the mean; pair it with a sketch per segment if you want the distribution of each regime.  The
`thyme` module's `Parsley` profiler is built from exactly these pieces.

Full API: `maths/src/Distribution.scala`.

## Fitting

`FitLine.Impl()` is a least-squares line you add points to, one at a time with `+=`, in bulk with `addRange`, or
from any array with `addWith` and two accessor functions; `x2y` is the fitted function (`slope`, `intercept`,
`apply`, `inverse`) and `y2x` the fit the other way, and `-=` removes a point, so a sliding window is cheap.
`TheilSen.fit(xs, ys)` is the robust alternative: the median of pairwise slopes, with a distribution-free confidence
interval.  `FitCirc()` fits a circle the same way and gives back `circle` with `x`, `y`, `r`.  `Roots.quadratic`,
`cubic`, and `quartic` write the real roots into an array you supply, and `Smoothing` has `loessAt`, `kdeAt`,
`rollingMean`, and `rollingMedian`.

**Reach for these when** you'd compute a slope from sums of products, sort pairs to find a robust one, or solve a
quadratic with the textbook formula and its cancellation.

Plain Scala:

<!-- guide: fitting.plain -->
```scala
val n = xs.length
val mx = xs.sum / n
val my = ys.sum / n
var sxy = 0.0
var sxx = 0.0
var i = 0
while i < n do
  sxy += (xs(i) - mx) * (ys(i) - my)
  sxx += (xs(i) - mx) * (xs(i) - mx)
  i += 1
val slope = sxy / sxx
val intercept = my - slope * mx
```

kse3:

<!-- guide: fitting.kse3 -->
```scala
val fit = FitLine.Impl()
fit.addRange(xs, 0, xs.length)(ys, 0, ys.length)
val slope = fit.x2y.slope
val intercept = fit.x2y.intercept
val at3 = fit.x2y(3.0)                   // the fitted y at x = 3
val robust = TheilSen.fit(xs, ys)        // median of pairwise slopes, with a confidence interval
```

<!-- guide: circle.kse3 -->
```scala
val fc = FitCirc()
fc.addRange(pts, 0, pts.length)
val circle = fc.circle                   // x, y, r; NaN when the points are degenerate
val roots = new Array[Double](2)
val n = Roots.quadratic(-6, 1, 1, roots) // -6 + x + x^2: writes the 2 real roots, -3 and 2
val smooth = Smoothing.rollingMedian(Array(1.0, 9.0, 2.0, 3.0, 8.0), 3)
```

`TheilSen` is O(n²) and meant for modest samples.  `Roots` takes coefficients in ascending order of power and
returns how many roots it wrote.

Full API: `maths/src/Fitting.scala` (package `kse.maths.fitting`), `maths/src/Roots.scala`, `maths/src/Smoothing.scala`.

## Vectors and matrices

`Vec2D` and `Vec3D` are case classes of `Double`s with the vector vocabulary as methods: `+`, `-`, scalar `*`, dot
product `*`, cross product `X`, `len`, `hat` (the unit vector), `theta`, `rotate`, `proj` and `orth` (the parts along
and across another vector), `dist`, and `angle`.  `Vc` is the same vocabulary for two `Float`s packed into one
`Long`, so an `Array[Vc]` is a flat array of longs, and `Vec3F` is its three-float cousin.  The `Mat22`, `Mat23`,
`Mat32`, and `Mat33` families (`F` and `D` suffixes) are small matrices over arrays in column-major order, built
from rows as you'd write them, with `*`, `T` (a zero-copy transpose), `det`, and `inv`; `Xform2D` and `Xform2F` are
affine transforms that apply to points with `x(p)` and to directions with `x.dir(d)`.  `PlusMinus` carries a value
and its error as two floats and propagates the error through `+`, `-`, `*`, and `/`.

**Reach for these when** you'd write `x*u.x + y*u.y` by hand, keep parallel arrays of x and y, or reach for a linear
algebra library for a two-by-two.

<!-- guide: vectors.kse3 -->
```scala
val v = Vec2D(1.2, 3.1)                   // Doubles; Vc(1.2f, 3.1f) packs two Floats into one Long
val u = Vec2D(-1.5, 0.7)
val dot = v * u                           // dot product; v X u is the 2D cross product
val n = v.hat                             // unit vector; also len, theta, rotate(angle), proj, orth
val m = Mat22D(0, -1)(1, 0)               // rows as written; m * v applies it, m.T transposes with no copy
val r = m * v
val x = Xform2D(m, Vec2D(10, 0))          // an affine transform: x(p) moves a point, x.dir(d) a direction
val p = x(v)
val pm = 2.5f +- 0.1f                     // a value with an error, as a PlusMinus
```

`v.T` is a row vector, so `v.T * u` is a dot product and `v * u.T` an outer product, again without copying.
`Frac` is an exact rational in a `Long`, used by the duration types for scaling.

Full API: `maths/src/Vec.scala`, `maths/src/Mat.scala`, and `Vc`, `PlusMinus`, and `Frac` in `maths/src/Maths.scala`.

## Colour

`Rgb` is an 8-bit sRGB colour in an `Int`, with every CSS colour as a constant (`Rgb.Salmon`) and `Rgb.byName` to
look one up from text.  Channels come out as `UByte`s (`r`, `g`, `b`), `Int`s (`rI`), or unit floats (`rF`), go back in with
`rTo` and `rOp`, and `pr` prints the `#RRGGBB` form.  `aTo` adds an alpha and gives an `Argb`.  For anything that
should look right to an eye, `Oklab` is the perceptual space: `Oklab.sRGB(c)` converts in, `blend` mixes with
weights, `l`, `a`, `b`, `c`, `h` read lightness, the two chroma axes, chroma, and hue, and `.rgb` converts back.
`Ehsv` is hue (as a fraction of a turn), saturation, and value, and `Ergb` an extended-range RGB for overbright
values.

**Reach for these when** you'd pack channels with shifts, average two colours channel by channel (it looks wrong),
or keep a table of colour names.

<!-- guide: colour.kse3 -->
```scala
val c = Rgb(250, 128, 114)                // 8-bit sRGB; Rgb.F(0.98f, 0.5f, 0.45f) from unit floats
val named = Rgb.Salmon == c               // the CSS names as constants; Rgb.byName("salmon") from text
val hex = c.pr                            // "#FA8072"
val red = c.rF                            // channels as UBytes (r, g, b), Ints (rI), or unit floats (rF)
val faded = c.aTo(0.5)                    // an Argb with alpha
val lab = Oklab.sRGB(c)                   // perceptual space, for mixing that looks right
val dusk = Oklab.blend(lab, 0.5f)(Oklab.sRGB(Rgb.Black), 0.5f).rgb
val hsv = Ehsv.from(c)                    // hue as a turn fraction, saturation, value
```

Full API: `maths/src/Colour.scala` (package `kse.maths.colours`).

## Numbers as text

Mostly you don't touch this layer: the `say` interpolator (basics) is how numbers become text, and `Grok` (eio) is
how text becomes numbers.  What's here is what they stand on.  `Ryu` renders a `Double` or `Float` as the shortest
digits that read back to the same value, into a byte or char buffer, a `Mem`, or a `MkStr` with no allocation, and
with a lowercase `e` on the exponent; `Ryu.fmt(x, mag, sig)` limits the precision instead, to the shortest decimal
within a don't-care tolerance set by the last place that matters (`mag`) or a count of significant figures (`sig`),
which is how jsaun prints a `Double` when asked to round.  `Parse` turns a range of a string, byte array, or `Mem` into a `Long`,
`ULong`, or hex value with no boxing on either path: failure is a sentinel that never occurs in real data,
`Parse.failLong`, and `spellsFailLong` tells you if the input really did spell it.  `SemanticOrder` is a natural
string ordering where `file2` sorts before `file10`, with presets for signed, decimal, versioned, and prose numbers.
`RomanNumber` and `SpokenNumber` go both ways between numbers and "IX" or "forty-two"; the latter is what `spoken(n)`
uses inside `say`.

<!-- guide: text.kse3 -->
```scala
val text = Ryu.string(0.1 + 0.2)          // "0.30000000000000004": the shortest digits that round-trip
val three = Ryu.fmt(0.1 + 0.2, 0, 3)      // "0.3": at most three significant figures, the shortest decimal within that
val n = Parse.long("12345")               // failure is the in-band Parse.failLong; see spellsFailLong
val order = List("file10", "file2").sorted(using SemanticOrder)
val nine = RomanNumber.text(9)            // "IX"
val words = SpokenNumber.text(42L.u)      // "forty-two"
```

Full API: `maths/src/Ryu.scala`, `maths/src/Parsing.scala`, and `maths/src/StringMaths.scala` (package
`kse.maths.stringmaths`).
