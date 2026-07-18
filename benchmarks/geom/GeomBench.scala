// JMH benchmarks for kse.maths little matrices/vectors against ojAlgo on tiny fixed-size
// operations (the use case the kse types are designed for; ojAlgo is a general-purpose
// linear algebra library, so this measures small-size overhead, not BLAS-scale skill).
//
// ojAlgo appears at two levels: MatrixR064 is the user-facing immutable API (allocates a
// result per op, like kse does), and R064Store is the mutable store with preallocated
// targets (ojAlgo's best case: zero allocation, but not how casual code is written).
// All operands hold identical values, built from the same arrays.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              taskset -c 4 scala-cli --power run benchmarks/geom --jmh -- -f 2 -wi 5 -i 5
// Quick look:                                 taskset -c 4 scala-cli --power run benchmarks/geom --jmh -- -f 1 -wi 3 -i 3

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar
//> using dep org.ojalgo:ojalgo:57.0.0

package kse.bench.geom

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import org.ojalgo.matrix.MatrixR064
import org.ojalgo.matrix.store.R064Store

import kse.maths.{_, given}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class GeomBench {
  private val rng = new java.util.Random(20260718)
  private def rnd(): Double = rng.nextDouble()*2 - 1

  var k22a: Mat22D = Mat22D(rnd(), rnd())(rnd(), rnd())
  var k22b: Mat22D = Mat22D(rnd(), rnd())(rnd(), rnd())
  var k33a: Mat33D = Mat33D(rnd(), rnd(), rnd())(rnd(), rnd(), rnd())(rnd(), rnd(), rnd())
  var k33b: Mat33D = Mat33D(rnd(), rnd(), rnd())(rnd(), rnd(), rnd())(rnd(), rnd(), rnd())
  var k23:  Mat23D = Mat23D(rnd(), rnd(), rnd())(rnd(), rnd(), rnd())
  var k32:  Mat32D = Mat32D(rnd(), rnd())(rnd(), rnd())(rnd(), rnd())
  var kv3a: Vec3D  = Vec3D(rnd(), rnd(), rnd())
  var kv3b: Vec3D  = Vec3D(rnd(), rnd(), rnd())
  var kx3:  Xform3D = Xform3D.rotate(Vec3D(rnd(), rnd(), rnd()), 0.83) * Xform3D.translate(rnd(), rnd(), rnd())

  private def store(rows: Int, cols: Int, colMajor: Array[Double]): R064Store =
    val s = R064Store.FACTORY.make(rows, cols)
    var c = 0
    while c < cols do
      var r = 0
      while r < rows do
        s.set(r.toLong, c.toLong, colMajor(r + rows*c))
        r += 1
      c += 1
    s

  var os22a: R064Store = store(2, 2, k22a.unwrap)
  var os22b: R064Store = store(2, 2, k22b.unwrap)
  var os33a: R064Store = store(3, 3, k33a.unwrap)
  var os33b: R064Store = store(3, 3, k33b.unwrap)
  var os23:  R064Store = store(2, 3, k23.unwrap)
  var os32:  R064Store = store(3, 2, k32.unwrap)
  var osv3a: R064Store = store(3, 1, Array(kv3a.x, kv3a.y, kv3a.z))
  var osv3b: R064Store = store(3, 1, Array(kv3b.x, kv3b.y, kv3b.z))
  var osT22: R064Store = R064Store.FACTORY.make(2, 2)
  var osT33: R064Store = R064Store.FACTORY.make(3, 3)
  var osT31: R064Store = R064Store.FACTORY.make(3, 1)

  var oj22a: MatrixR064 = MatrixR064.FACTORY.copy(os22a)
  var oj22b: MatrixR064 = MatrixR064.FACTORY.copy(os22b)
  var oj33a: MatrixR064 = MatrixR064.FACTORY.copy(os33a)
  var oj33b: MatrixR064 = MatrixR064.FACTORY.copy(os33b)
  var oj23:  MatrixR064 = MatrixR064.FACTORY.copy(os23)
  var oj32:  MatrixR064 = MatrixR064.FACTORY.copy(os32)
  var ojv3:  MatrixR064 = MatrixR064.FACTORY.copy(osv3a)

  // --- 2x2 * 2x2 ---

  @Benchmark def kse_mm22(bh: Blackhole): Unit = bh.consume(k22a * k22b)
  @Benchmark def oj_mm22(bh: Blackhole): Unit = bh.consume(oj22a.multiply(oj22b))
  @Benchmark def ojs_mm22(bh: Blackhole): Unit =
    osT22.fillByMultiplying(os22a, os22b)
    bh.consume(osT22)

  // --- 3x3 * 3x3 ---

  @Benchmark def kse_mm33(bh: Blackhole): Unit = bh.consume(k33a * k33b)
  @Benchmark def oj_mm33(bh: Blackhole): Unit = bh.consume(oj33a.multiply(oj33b))
  @Benchmark def ojs_mm33(bh: Blackhole): Unit =
    osT33.fillByMultiplying(os33a, os33b)
    bh.consume(osT33)

  // --- 2x3 * 3x2 ---

  @Benchmark def kse_mm23_32(bh: Blackhole): Unit = bh.consume(k23 * k32)
  @Benchmark def oj_mm23_32(bh: Blackhole): Unit = bh.consume(oj23.multiply(oj32))

  // --- A.T * A (transpose is free for kse, a copy for ojAlgo) ---

  @Benchmark def kse_gram33(bh: Blackhole): Unit = bh.consume(k33a.T * k33a)
  @Benchmark def oj_gram33(bh: Blackhole): Unit = bh.consume(oj33a.transpose().multiply(oj33a))

  // --- 3x3 * vector ---

  @Benchmark def kse_mv33(bh: Blackhole): Unit = bh.consume(k33a * kv3a)
  @Benchmark def oj_mv33(bh: Blackhole): Unit = bh.consume(oj33a.multiply(ojv3))
  @Benchmark def ojs_mv33(bh: Blackhole): Unit =
    osT31.fillByMultiplying(os33a, osv3a)
    bh.consume(osT31)

  // --- vector dot ---

  @Benchmark def kse_dot3(bh: Blackhole): Unit = bh.consume(kv3a * kv3b)
  @Benchmark def ojs_dot3(bh: Blackhole): Unit = bh.consume(osv3a.dot(osv3b))

  // --- 3x3 determinant and inverse ---

  @Benchmark def kse_det33(bh: Blackhole): Unit = bh.consume(k33a.det)
  @Benchmark def oj_det33(bh: Blackhole): Unit = bh.consume(oj33a.getDeterminant())

  @Benchmark def kse_inv33(bh: Blackhole): Unit = bh.consume(k33a.inv)
  @Benchmark def oj_inv33(bh: Blackhole): Unit = bh.consume(oj33a.invert())

  // --- affine point transform (no direct ojAlgo counterpart; for scale) ---

  @Benchmark def kse_xform3_pt(bh: Blackhole): Unit = bh.consume(kx3(kv3a))
}
