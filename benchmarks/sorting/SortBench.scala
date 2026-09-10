// JMH benchmark for kse.basics.Sorting: the stable value sort (with and without indices) and the
// index sort, against java.util.Arrays.sort and the boxed Array.range(...).sortBy that Stats used.
// Rows that sort in place first copy the pristine data, so `copyOnly` is the floor to subtract.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              taskset -c 4 scala-cli --power run benchmarks/sorting --jmh -- -f 1 -wi 3 -i 5 -r 1 -w 1
// One key type:                               taskset -c 4 scala-cli --power run benchmarks/sorting --jmh -- Double

//> using scala 3.8.4
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar

package kse.bench.sorting

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import kse.basics.{given, _}

/** The same order as Order.Doubles, but in the three-line Partial form, to price the compiled-kernels plumbing. */
object CompiledDoubles extends Sorting.Partial[Double] {
  inline def leq(a: Double, b: Double): Boolean = a <= b
  val kernels = build()
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
class DoubleSortBench {
  @Param(Array("1000", "100000", "1000000"))
  var n: Int = 0

  var pristine: Array[Double] = null
  var work: Array[Double] = null
  var tmp: Array[Double] = null
  var ix: Array[Int] = null
  var tmpIx: Array[Int] = null

  @Setup def setup(): Unit =
    val rng = new java.util.Random(8675309L + n)
    pristine = Array.fill(n)(rng.nextDouble())
    work = new Array[Double](n)
    tmp = new Array[Double](n)
    ix = new Array[Int](n)
    tmpIx = new Array[Int](n)
    val a = valueSort()
    val b = jdkSort()
    require(java.util.Arrays.equals(a, b), "sorts disagree")
    val c = indexSort()
    require(c == n && ix.indices.forall(i => pristine(ix(i)) == a(i)), "index sort disagrees")

  @Benchmark def copyOnly(): Array[Double] =
    System.arraycopy(pristine, 0, work, 0, n)
    work

  @Benchmark def jdkSort(): Array[Double] =
    System.arraycopy(pristine, 0, work, 0, n)
    java.util.Arrays.sort(work)
    work

  @Benchmark def valueSort(): Array[Double] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortInOrder(0, n, tmp) __ Unit
    work

  @Benchmark def valueSortWithIndices(): Array[Int] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortWithIndices(0, n, ix, tmp, tmpIx) __ Unit
    ix

  @Benchmark def indexSort(): Int =
    pristine.indicesInOrder(0, n, ix, tmpIx)

  @Benchmark def boxedSortBy(): Array[Int] =
    Array.range(0, n).sortBy(i => pristine(i))

  @Benchmark def valueSortCompiled(): Array[Double] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortInOrder(0, n, tmp)(using CompiledDoubles) __ Unit
    work

  @Benchmark def valueSortWithIndicesCompiled(): Array[Int] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortWithIndices(0, n, ix, tmp, tmpIx)(using CompiledDoubles) __ Unit
    ix

  @Benchmark def indexSortCompiled(): Int =
    pristine.indicesInOrder(0, n, ix, tmpIx)(using CompiledDoubles)
}


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
class IntSortBench {
  @Param(Array("1000", "100000", "1000000"))
  var n: Int = 0

  var pristine: Array[Int] = null
  var work: Array[Int] = null
  var tmp: Array[Int] = null
  var ix: Array[Int] = null
  var tmpIx: Array[Int] = null

  @Setup def setup(): Unit =
    val rng = new java.util.Random(8675309L + n)
    pristine = Array.fill(n)(rng.nextInt())
    work = new Array[Int](n)
    tmp = new Array[Int](n)
    ix = new Array[Int](n)
    tmpIx = new Array[Int](n)
    require(java.util.Arrays.equals(valueSort(), jdkSort()), "sorts disagree")

  @Benchmark def jdkSort(): Array[Int] =
    System.arraycopy(pristine, 0, work, 0, n)
    java.util.Arrays.sort(work)
    work

  @Benchmark def valueSort(): Array[Int] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortInOrder(0, n, tmp) __ Unit
    work

  @Benchmark def valueSortWithIndices(): Array[Int] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortWithIndices(0, n, ix, tmp, tmpIx) __ Unit
    ix

  @Benchmark def indexSort(): Int =
    pristine.indicesInOrder(0, n, ix, tmpIx)
}


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
class StringSortBench {
  @Param(Array("1000", "100000"))
  var n: Int = 0

  var pristine: Array[String] = null
  var work: Array[String] = null
  var tmp: Array[String] = null
  var ix: Array[Int] = null
  var tmpIx: Array[Int] = null

  @Setup def setup(): Unit =
    val rng = new java.util.Random(8675309L + n)
    pristine = Array.fill(n)("fish" + rng.nextInt(1000000))
    work = new Array[String](n)
    tmp = new Array[String](n)
    ix = new Array[Int](n)
    tmpIx = new Array[Int](n)
    require(java.util.Arrays.equals(valueSort().asInstanceOf[Array[AnyRef]], jdkSort().asInstanceOf[Array[AnyRef]]), "sorts disagree")

  /** The JDK's TimSort, which is also stable. */
  @Benchmark def jdkSort(): Array[String] =
    System.arraycopy(pristine, 0, work, 0, n)
    java.util.Arrays.sort(work.asInstanceOf[Array[AnyRef]])
    work

  @Benchmark def valueSort(): Array[String] =
    System.arraycopy(pristine, 0, work, 0, n)
    work.sortInOrder(0, n, tmp) __ Unit
    work

  @Benchmark def indexSort(): Int =
    pristine.indicesInOrder(0, n, ix, tmpIx)
}
