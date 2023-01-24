// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2020-23 Rex Kerr and Calico Life Sciences, LLC.


package kse.flow


import java.lang.ref.SoftReference
import java.time._
import java.util.concurrent.atomic._

import scala.util.control.NonFatal


/** A general way to defer a computation but cache the result. */
final class Lazy[V](gen: => V) {

  /** The cached value. */
  lazy val value = gen

  /** Create a new lazy value by (when first requested) applying a function to this value. */
  def map[W](f: V => W) = Lazy(f(value))

  /** Create a new lazy value by (when first requested) applying a function to get a new `Lazy`.
    *
    * Note that you won't even call the function to create the new `Lazy` value until you request the value.
    * If you want to generate the new `Lazy` now, just apply it instead (i.e. `f(lazy.value)`). 
    */
  def flatMap[W](f: V => Lazy[W]) = Lazy(f(value).value)
}
object Lazy {
  /** Create a new lazily computed cached value. */
  def apply[V](gen: => V) = new Lazy(gen)
}


/** An unset value that you can set but then not change (thread-safe) */
final class Worm[V] {
  private[this] val myValue: AtomicReference[AnyRef] = new AtomicReference[AnyRef](Worm.storedNullSentinel)

  /** Sets the value if it hasn't already been set.  Returns `true` if the new value was set, `false` if not.
    * 
    * The new value is only computed if the value is initially unset.  (It may be filled in by someone else while
    * the computation is occurring, though.)
    */ 
  def setIfEmpty(v: => V): Boolean =
    var old = myValue.get()
    if (old eq Worm.storedNullSentinel) {
      myValue.compareAndSet(old, v.asInstanceOf[AnyRef])
    }
    else false

  /** Sets the value, returning self, or throws an exception if it's already set. */
  def set(v: => V): this.type =
    if (!setIfEmpty(v)) throw new java.lang.IllegalStateException("Set while already set")
    this

  /** Gets the value, throwing an exception if it hasn't been set yet. */
  def get: V = myValue.get() match
    case x if x eq Worm.storedNullSentinel => throw new java.lang.IllegalStateException("Retrieved value before being set")
    case x                                 => x.asInstanceOf[V]

  /** Gets the value, or sets a new value if it hasn't been set yet. */
  def getOrSet(v: => V): V = myValue.get() match
    case x if x eq Worm.storedNullSentinel =>
      setIfEmpty(v)
      get
    case x => x.asInstanceOf[V]

  /** If the value is set, return it in a favored branch; otherwise, Unit in the disfavored branch. */
  def value: V Or Unit = myValue.get() match
    case x if x eq Worm.storedNullSentinel => Alt.unit
    case x                                 => Is(x.asInstanceOf[V])
}
object Worm {
  /** Creates a new uninitialized box for the value.  Set it with `set` or `setIfEmpty`. */
  def of[V]: Worm[V] = new Worm[V]

  private[Worm] val storedNullSentinel: AnyRef = new AnyRef {}
}


/** Caches expensive computations that are cleared when memory gets especially tight (via SoftReference); not thread-safe.
  *
  * The initial computation does not take place until it is used.
  * 
  * The source information for doing the computation is not cleared, and is given as an explicit parameter rather than
  * captured by the generating function.  For instance, this could be a filename, and the computation could load the
  * contents of the file into memory.
  */
final class Soft[S, V](source: S)(gen: S => V) {
  import Soft._

  private[this] val myCache: AtomicReference[SoftReference[AnyRef]] = new AtomicReference(storedSoftNull)

  def forget(): this.type = { myCache.set(storedSoftNull); this }

  private[this] inline def properlySetValue(v: V): Unit =
    myCache.set(new SoftReference(if (v.asInstanceOf[AnyRef] eq null) Soft.storedNullSentinel else v.asInstanceOf[AnyRef]))

  /** Retrieve or recompute the cached value. */
  def value: V = myCache.get().get() match
    case null =>
      val v = gen(source)
      properlySetValue(v)
      v
    case x if x.asInstanceOf[AnyRef] eq Soft.storedNullSentinel =>
      null.asInstanceOf[V]
    case x =>
      x.asInstanceOf[V]

  /** Retrieves the value, packed in an `Is`, or recomputes and caches the value, packing it in an `Alt` */
  def valueOrValue: V Or V = myCache.get().get() match
    case null =>
      val v = gen(source)
      properlySetValue(v)
      Alt(v)
    case x if x.asInstanceOf[AnyRef] eq Soft.storedNullSentinel =>
      Is(null.asInstanceOf[V])
    case x =>
      Is(x.asInstanceOf[V])

  /** Retrieves the value, packed in an `Is`, or recomputes and caches the value, packing it in an `Alt` */
  def valueOrUnit: V Or Unit = myCache.get().get() match
    case null =>
      Alt.unit
    case x if x.asInstanceOf[AnyRef] eq Soft.storedNullSentinel =>
      Is(null.asInstanceOf[V])
    case x =>
      Is(x.asInstanceOf[V])

  /** Create a new cached value by applying a function to this one. */
  def map[W](f: V => W) = Soft(source)(gen andThen f)
}
object Soft {
  def apply[S, V](source: S)(gen: S => V) = new Soft(source)(gen)

  private[Soft] val storedNullSentinel: AnyRef = new AnyRef {}
  private[Soft] val storedSoftNull: SoftReference[AnyRef] = new SoftReference(null)
}


/** Holds values while they are deemed good; recomputes as needed.
  *
  * `Hold` is a moderately computationally expensive bare-bones reactive computational framework.
  * It is intended to solve many common problems of caching and cache invalidation in a fairly
  * simple and fairly flexible way.
  * 
  * Holds neither print nor serialize nor compare gracefully--it is your job to access the values in
  * a reasonable way.
  * 
  * Holds are intended to be threadsafe, but if you do concurrent access from different entry points,
  * it is possible to run into a deadlock, as they internally use synchronization.  For instance,
  * if holds `c` and `d` both depend on `a` and `b`, and you operate on `c` in one thread and `d` in
  * another, one thread may synchronize on `a` and need `b`, while the other has already obtained `b`
  * and is waiting on `a`.  Thus, care should be taken to avoid this when using `Hold` in a multithreaded
  * context.
  * 
  * A `Hold` has three fundamental operations: `release()`, which tells `Hold` to stop caching any
  * values and to tell any `Hold`s it depends on to also stop caching their values, thereby causing
  * a complete recomputation; `recompute()`, which tells `Hold` to recalculate its own cached value
  * without necessarily recomputing anything it depends on, though if a recomputation is necessary it
  * will be performed, and `getOrUnit`, which gets the value if it's cached, or `Alt.unit` if it's not.
  * 
  * If a `Hold` encounters an exception during a `recompute()`, it will throw the exception normally but
  * discard any existing cached value, if possible.  To avoid exceptions. use `Try` or `safe`
  * or some other error-handling mechanism.
  * 
  * Typically, one wishes to get the cached value without worrying about any of that, which one can do
  * with `value`.  If you want both the value and the count of recomputations, use `get` instead.  If
  * you want the value and count but want to know whether it was recomputed, use `getOrCompute`--if it
  * was cached, the value and count will be in the `Is` branch; if it had to be recomputed, it will be
  * in the `Alt` branch.  You can also `force()` as a shorthand for `release` followed by `recompute`.
  * 
  * Aside from `Hold.unit`, the canonical held unit value, you can wrap stable values with `Hold.fixed`,
  * always recompute a block with `Hold.unheld`, lazily persist a block with `Hold.apply` (recomputes when relased),
  * store a mutable value with `Hold.mutate`--Holds that depend on it will update once you mutate it--and iterate
  * from an initial value, updating every access, with `Hold.iterate`.  You can also use soft instead of lazy persistence
  * with `Hold.soft`.
  * 
  * The real power comes in adding conditions for when recomputation will occur.  `trust` will produce
  * a new `Hold` that caches the value of an existing one for either a number of accesses (`trust(10)`)
  * or a `java.time.Duration` (e.g. `trust(Duration.ofSeconds(45))`); `trust()` alone will permanently cache
  * the value unless it's released.  In contrast, `expireIn` will force a release after a certain number of
  * accesses or a duration, and `expireIf` will force a release when a predicate on the value returns true.
  * To prevent a subset of dependencies from being released by `expire`, use `.protect` to create a `Hold` that
  * ignores any `releaese` requests.  Note that `Hold` has no threading itself--nothing will happen on its
  * own when a duration expires, but the next value access after the duration expires will cause a recomputation.
  * 
  * You can also `map` or `softMap` an existing `Hold` to a new value; the new value will stay cached until invalidated
  * somehow (e.g. manually with `recompute()` or because you've wrapped it with `expireIn(5)`).  There are helper
  * methods to map pairs of `Hold`s, recomputing if either dependency changes (`mapWith` and `softMapWith`); if you
  * need more, use `Hold.them` to group up to six `Hold`s into a tuple, and then map the tuple.
  * 
  * Example--cache the contents of a file, reloading it once an hour:
  * {{{
  * val filename = java.nio.file.Paths.get("my_changing_file.txt")
  * val contents = Hold.fixed(filename)
  *   .map(p => java.nio.file.Files.readAllBytes(p))
  *   .expireIn(java.time.Duration.ofHours(1))
  * println(s"On first load, the file has ${contents.value.length} bytes")
  * // Hint--while testing this code, don't run the below unless you actually want to wait an hour!
  * Thread.sleep(3_590_000)
  * println(s"We still are holding ${contents.value.length} bytes")
  * Thread.sleep(20_000)
  * println(s"Now the answer might be different--${contents.value.length}")
  * }}}
  * 
  * Example--create a string on demand from two mutable values
  * {{{
  * val capitals = Hold.mutable(false)
  * val name = Hold.mutable("Salmon")
  * val fish = name.mapWith(capitals)((s, c) => if c then s.toUpperCase else s.toLowerCase)
  * println(fish.value)  // "salmon"
  * capitals.set(true)
  * println(fish.value)  // "SALMON"
  * name.set("Cod")
  * capitals.set(false)
  * println(fish.value)  // "cod", only recomputed once
  * }}}
  * 
  * 
  */
sealed trait Hold[V] {
  import Hold._

  /** Empty our cache and the cache of any (unprotected) `Hold`s we depend on. */
  def release(): Unit

  /** Recompute our value from our dependencies, cache it if appropriate, and return the new value and its version. */
  def recompute(): (V, Long)

  /** If we have cached a value, return it along with the number of computations; otherwise return `Alt(())` */
  def getOrUnit: (V, Long) Or Unit


  /** Retrieve our value, whether from cache or recomputation. */
  final def value: V = getOrUnit.getOrElse(_ => recompute())._1

  /** Retrieve both our value and the number of recomputations. */
  final def get: (V, Long) = getOrUnit.getOrElse(_ => recompute())

  /** Retrieve our value and the number of recomputations; put in an `Is` if it was cached or `Alt` if we had to recompute. */
  final def getOrCompute: (V, Long) Or (V, Long) = getOrUnit.mapAlt(_ => recompute())

  /** Release ourselves and our dependencies, then recompute. */
  final def force(): (V, Long) = { release(); recompute() }

  /** Release ourselves and our dependencies, then recompute, returning only the value */
  final def forceValue(): V = force()._1


  /** Create a new `Hold` that will cache whatever this `Hold` has for `count` accesses at a time, without checking for changes. */
  final def trust(count: Long): Hold[V] = new Cached(0L, this)(_ + 1, _ >= count)

  /** Create a new `Hold` that will cache whatever this `Hold` has for a `Duration` `d`; discard on the first access after that much
    * time has elapsed since the first access; until that time, don't check for changes. */
  final def trust(d: Duration): Hold[V] = new Cached(Instant.now, this)(t => t, t => Duration.between(t, Instant.now).compareTo(d) >= 0)

  /** Create a new `Hold` that will cache this value permanently (unless released).  It will not check for changes. */
  final def trust(): Hold[V] = new Cached((), this)(u => u, _ => false)

  /** After `count` accesses, release ourselves and all our dependencies. */
  final def expireIn(count: Long): Hold[V] = new Fragile(0L, this)(_ + 1, (_, c) => c >= count)

  /** After a `Duration` `d` or longer from the last reload, access will release instead of using a cache */
  final def expireIn(d: Duration): Hold[V] = new Fragile(Instant.now, this)(t => t, (_, t) => Duration.between(t, Instant.now).compareTo(d) >= 0)

  /** Test the contents on access, releasing self and all dependencies if the test succeeds */
  final def expireIf(p: (V, Long) => Boolean): Hold[V] = new Fragile((), this)(u => u, (vcv, _) => p(vcv._1, vcv._2))

  /** Do not allow dependencies to release this `Hold` */
  final def protect(): Hold[V] = new Protect[V](this)


  /** Create a new `Hold` that caches the result of a function `f` applied to our value */
  final def map[U](f: V => U): Hold[U] = new Map(this)(f)

  /** Create a new `Hold` that caches the result of a function `f` applied to our value, but release it if memory pressure is bad 
    * (i.e. store it in a `SoftReference`).
    */
  final def softMap[U](f: V => U): Hold[U] = new SoftMap(this)(f)


  /** Create a new `Hold` that caches the result of a function `f` applied to both our value and that of another `Hold` */
  final def mapWith[U, A](that: Hold[U])(f: (V, U) => A): Hold[A] =
    val them = Hold.them(this, that)
    new Map(them)(vu => f(vu._1, vu._2))

  /** Create a new `Hold` that caches the result of a function `f` applied to both our value and that of another `Hold`,
    * using a `SoftReference` for the cache.
    */
  final def softMapWith[U, A](that: Hold[U])(f: (V, U) => A): Hold[A] =
    val them = Hold.them(this, that)
    new SoftMap(them)(vu => f(vu._1, vu._2))
}
object Hold {
  /** The canonical Hold of a unit value. */
  val unit: Hold[Unit] = fixed(())


  /** Hold a fixed value.  Does not recompute--but useful to provide input for mapping. */
  def fixed[V](value: V): Hold[V] = new Fixed(value)

  /** Recompute a value every time; never cache it. */
  def unheld[V](gen: => V): Hold[V] = new Unheld(gen)

  /** Compute a value once and store it--equivalent to a lazy val unless `release()` or `recompute()` is called. */
  def apply[V](gen: => V): Hold[V] = new Lazy(gen)

  /** Store a mutable value.  Don't change it, but allow users to set it; the changes may cascade. */
  def mutable[V](initial: V): Mutable[V] = new Mutable(initial)

  /** From an initial value, produce a succession of values on each access. */
  def iterate[V](initial: V)(f: V => V): Hold[V] = new Iterate(initial)(f)

  /** Compute a value once and store it in a `SoftReference`; it will be cached until memory pressure or `release()` or `recompute()`. */
  def soft[V](gen: => V): Hold[V] = new SoftMap(Hold.unit)(_ => gen)


  /** Store two `Hold`s.  Updates whenever either of the two change. */
  def them[U, V](hu: Hold[U], hv: Hold[V]): Hold[(U, V)] =
    new Tuple2(hu, hv)

  /** Store three `Hold`s.  Updates whenever either of the two change. */
  def them[T, U, V](ht: Hold[T], hu: Hold[U], hv: Hold[V]): Hold[(T, U, V)] =
    new Tuple3(ht, hu, hv)

  /** Store four `Hold`s.  Updates whenever either of the two change. */
  def them[S, T, U, V](hs: Hold[S], ht: Hold[T], hu: Hold[U], hv: Hold[V]): Hold[(S, T, U, V)] =
    new Tuple4(hs, ht, hu, hv)

  /** Store five `Hold`s.  Updates whenever either of the two change. */
  def them[R, S, T, U, V](hr: Hold[R], hs: Hold[S], ht: Hold[T], hu: Hold[U], hv: Hold[V]): Hold[(R, S, T, U, V)] =
    new Tuple5(hr, hs, ht, hu, hv)

  /** Store six `Hold`s.  Updates whenever either of the two change. */
  def them[Q, R, S, T, U, V](hq: Hold[Q], hr: Hold[R], hs: Hold[S], ht: Hold[T], hu: Hold[U], hv: Hold[V]): Hold[(Q, R, S, T, U, V)] =
    new Tuple6(hq, hr, hs, ht, hu, hv)

  /** Store a list of identical `Hold`s.  Note that a new array must be created every time a single `Hold` changes, so use with caution! */
  def array[V: scala.reflect.ClassTag](holds: Array[Hold[V]]): Hold[Array[V]] =
    new Arrayed(holds)

  private final class Fixed[V](underlying: V) extends Hold[V] {
    private[this] val answer = (underlying, 0L)

    def release(): Unit = {}
    def recompute() = answer
    def getOrUnit = Is(answer)
  }

  private final class Unheld[V](generate: => V) extends Hold[V] {
    private[this] var count = -1L

    def release(): Unit = {}
    def recompute() = this.synchronized{ count += 1; (generate, count) }
    def getOrUnit = Alt.unit
  }

  private final class Lazy[V](generate: => V) extends Hold[V] {
    private[this] var myValue: (V, Long) Or Unit = Alt.unit
    private[this] var count = -1L

    def release(): Unit = this.synchronized{ myValue = Alt.unit }
    def recompute() = this.synchronized{
      myValue = Alt.unit  // This is what we want if `generate` has an exception
      count += 1
      (generate, count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{ myValue }
  }

  /** Mutable storage for a value that acts as a `Hold`--you can't actually recompute it, but
    * if you reset it, anything that has this as a dependency has the option to recompute itself.
    */
  final class Mutable[V](initial: V) extends Hold[V] {
    private[this] var myValue = (initial, 0L)

    /** Set the stored value. */
    def set(next: V): this.type = this.synchronized{ myValue = (next, myValue._2 + 1); this }

    /** Computes a new stored value from the existing one. */
    def zap(f: V => V): this.type = this.synchronized{ myValue = (f(myValue._1), myValue._2 + 1); this }

    def release(): Unit = {}
    def recompute(): (V, Long) = this.synchronized{ myValue }
    def getOrUnit: (V, Long) Or Unit = Is(myValue)
  }

  private final class Iterate[V](initial: V)(f: V => V) extends Hold[V] {
    private[this] var myValue = initial
    private[this] var count = -1L

    def release(): Unit = {}
    def recompute() = this.synchronized{
      if count >= 0 then
        myValue = f(myValue)
      count += 1
      (myValue, count)
    }
    def getOrUnit = Alt.unit
  }

  private final class Cached[S, V](state0: => S, that: Hold[V])(advance: S => S, test: S => Boolean) extends Hold[V] {
    private[this] var myValue: (V, Long) Or Unit = Alt.unit
    private[this] var thatCount = -1L
    private[this] var count = -1L
    private[this] var state: S = state0

    def release(): Unit = this.synchronized{ myValue = Alt.unit; thatCount = -1L; that.release() }
    def recompute() = this.synchronized{
      myValue = Alt.unit  // In case of exception
      if count >= 0 then
        state = state0
      val (v, c) = that.get
      thatCount = c
      count += 1
      (v, count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      if myValue.isIs then
        state = advance(state)
        if test(state) then
          myValue = Alt.unit
      myValue
    }
  }

  private final class Fragile[S, V](state0: => S, that: Hold[V])(advance: S => S, test: ((V, Long), S) => Boolean) extends Hold[V] {
    private[this] var state: S Or Unit = Alt.unit

    def release(): Unit = this.synchronized{ state = Alt.unit; that.release() }
    def recompute() = this.synchronized{
      state = Alt.unit
      val vcv = that.recompute()
      val s = state0
      if test(vcv, s) then
        that.release()
      else
        state = Is(s)
      vcv
    }
    def getOrUnit = this.synchronized {
      var answer = that.getOrUnit
      answer.foreach{ vcv =>
        try {
          val s = state.map(advance).getOrElse(_ => state0)
          if test(vcv, s) then
            state = Alt.unit
            that.release()
            answer = Alt.unit
          else
            state = Is(s)
        }
        catch { case e if NonFatal(e) => state = Alt.unit; throw e }
      }
      answer
    }
  }

  private final class Protect[V](underlying: Hold[V]) extends Hold[V] {
    def release(): Unit = {}
    def recompute() = underlying.get
    def getOrUnit = underlying.getOrUnit
  }

  private final class Tuple2[U, V](hu: Hold[U], hv: Hold[V]) extends Hold[(U, V)] {
    private[this] var myValue: ((U, V), Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] var uCount = -1L
    private[this] var vCount = -1L

    def release(): Unit = this.synchronized{
      hu.release(); uCount = -1
      hv.release(); vCount = -1
    }
    def recompute() = this.synchronized{
      val (u, cu) = hu.get
      val (v, cv) = hv.get
      count += 1
      val answer = ((u, v), count)
      myValue = Is(answer)
      uCount = cu
      vCount = cv
      answer
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{ _ =>
        if      hu.getOrUnit.forall{ ucu => uCount != ucu._2 } then myValue = Alt.unit
        else if hv.getOrUnit.forall{ vcv => vCount != vcv._2 } then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class Tuple3[T, U, V](ht: Hold[T], hu: Hold[U], hv: Hold[V]) extends Hold[(T, U, V)] {
    private[this] var myValue: ((T, U, V), Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] var tCount = -1L
    private[this] var uCount = -1L
    private[this] var vCount = -1L

    def release(): Unit = this.synchronized{
      ht.release(); tCount = -1
      hu.release(); uCount = -1
      hv.release(); vCount = -1
    }
    def recompute() = this.synchronized{
      val (t, ct) = ht.get
      val (u, cu) = hu.get
      val (v, cv) = hv.get
      count += 1
      tCount = ct
      uCount = cu
      vCount = cv
      ((t, u, v), count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{ _ =>
        if      ht.getOrUnit.forall{ tct => tCount != tct._2 } then myValue = Alt.unit
        else if hu.getOrUnit.forall{ ucu => uCount != ucu._2 } then myValue = Alt.unit
        else if hv.getOrUnit.forall{ vcv => vCount != vcv._2 } then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class Tuple4[S, T, U, V](hs: Hold[S], ht: Hold[T], hu: Hold[U], hv: Hold[V]) extends Hold[(S, T, U, V)] {
    private[this] var myValue: ((S, T, U, V), Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] var sCount = -1L
    private[this] var tCount = -1L
    private[this] var uCount = -1L
    private[this] var vCount = -1L

    def release(): Unit = this.synchronized{
      hs.release(); sCount = -1
      ht.release(); tCount = -1
      hu.release(); uCount = -1
      hv.release(); vCount = -1
    }
    def recompute() = this.synchronized{
      val (s, cs) = hs.get
      val (t, ct) = ht.get
      val (u, cu) = hu.get
      val (v, cv) = hv.get
      count += 1
      sCount = cs
      tCount = ct
      uCount = cu
      vCount = cv
      ((s, t, u, v), count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{ _ =>
        if      hs.getOrUnit.forall{ scs => sCount != scs._2 } then myValue = Alt.unit
        else if ht.getOrUnit.forall{ tct => tCount != tct._2 } then myValue = Alt.unit
        else if hu.getOrUnit.forall{ ucu => uCount != ucu._2 } then myValue = Alt.unit
        else if hv.getOrUnit.forall{ vcv => vCount != vcv._2 } then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class Tuple5[R, S, T, U, V](hr: Hold[R], hs: Hold[S], ht: Hold[T], hu: Hold[U], hv: Hold[V])
  extends Hold[(R, S, T, U, V)] {
    private[this] var myValue: ((R, S, T, U, V), Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] var rCount = -1L
    private[this] var sCount = -1L
    private[this] var tCount = -1L
    private[this] var uCount = -1L
    private[this] var vCount = -1L

    def release(): Unit = this.synchronized{
      hr.release(); rCount = -1
      hs.release(); sCount = -1
      ht.release(); tCount = -1
      hu.release(); uCount = -1
      hv.release(); vCount = -1
    }
    def recompute() = this.synchronized{
      val (r, cr) = hr.get
      val (s, cs) = hs.get
      val (t, ct) = ht.get
      val (u, cu) = hu.get
      val (v, cv) = hv.get
      count += 1
      rCount = cr
      sCount = cs
      tCount = ct
      uCount = cu
      vCount = cv
      ((r, s, t, u, v), count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{ _ =>
        if      hr.getOrUnit.forall{ rcr => rCount != rcr._2 } then myValue = Alt.unit
        else if hs.getOrUnit.forall{ scs => sCount != scs._2 } then myValue = Alt.unit
        else if ht.getOrUnit.forall{ tct => tCount != tct._2 } then myValue = Alt.unit
        else if hu.getOrUnit.forall{ ucu => uCount != ucu._2 } then myValue = Alt.unit
        else if hv.getOrUnit.forall{ vcv => vCount != vcv._2 } then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class Tuple6[Q, R, S, T, U, V](hq: Hold[Q], hr: Hold[R], hs: Hold[S], ht: Hold[T], hu: Hold[U], hv: Hold[V])
  extends Hold[(Q, R, S, T, U, V)] {
    private[this] var myValue: ((Q, R, S, T, U, V), Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] var qCount = -1L
    private[this] var rCount = -1L
    private[this] var sCount = -1L
    private[this] var tCount = -1L
    private[this] var uCount = -1L
    private[this] var vCount = -1L

    def release(): Unit = this.synchronized{
      hq.release(); qCount = -1
      hr.release(); rCount = -1
      hs.release(); sCount = -1
      ht.release(); tCount = -1
      hu.release(); uCount = -1
      hv.release(); vCount = -1
    }
    def recompute() = this.synchronized{
      val (q, cq) = hq.get
      val (r, cr) = hr.get
      val (s, cs) = hs.get
      val (t, ct) = ht.get
      val (u, cu) = hu.get
      val (v, cv) = hv.get
      count += 1
      qCount = cq
      rCount = cr
      sCount = cs
      tCount = ct
      uCount = cu
      vCount = cv
      ((q, r, s, t, u, v), count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{ _ =>
        if      hq.getOrUnit.forall{ qcq => rCount != qcq._2 } then myValue = Alt.unit
        else if hr.getOrUnit.forall{ rcr => rCount != rcr._2 } then myValue = Alt.unit
        else if hs.getOrUnit.forall{ scs => sCount != scs._2 } then myValue = Alt.unit
        else if ht.getOrUnit.forall{ tct => tCount != tct._2 } then myValue = Alt.unit
        else if hu.getOrUnit.forall{ ucu => uCount != ucu._2 } then myValue = Alt.unit
        else if hv.getOrUnit.forall{ vcv => vCount != vcv._2 } then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class Arrayed[V: scala.reflect.ClassTag](holds: Array[Hold[V]]) extends Hold[Array[V]] {
    private[this] var myValue: (Array[V], Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] val aCounts = Array.fill(holds.length)(-1L)

    def release(): Unit = this.synchronized{
      aFor(holds){ (h, i) => h.release(); aCounts(i) = -1L }
    }
    def recompute() = this.synchronized{
      val a = new Array[V](holds.length)
      aFor(holds){ (h, i) =>
        val (v, c) = holds(i).get
        a(i) = v
        aCounts(i) = c
      }
      count += 1
      (a, count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{_ =>
        var agree = true
        cFor(0)(_ < holds.length && agree)(_ + 1){ i =>
          agree = holds(i).getOrUnit.exists(_._2 == aCounts(i))
        }
        if !agree then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class Map[A, V](source: Hold[A])(compute: A => V) extends Hold[V] {
    private[this] var myValue: (V, Long) Or Unit = Alt.unit
    private[this] var count = -1L
    private[this] var sourceCount = -1L

    def release(): Unit = { source.release(); sourceCount = -1L; myValue = Alt.unit }
    def recompute() = this.synchronized{
      myValue = Alt.unit  // In case of exception
      val (a, ca) = source.get
      count += 1
      sourceCount = ca
      (compute(a), count).tap(z => myValue = Is(z))
    }
    def getOrUnit = this.synchronized{
      myValue.foreach{ _ =>
        if source.getOrUnit.forall{ scs => sourceCount != scs._2 } then myValue = Alt.unit
      }
      myValue
    }
  }

  private final class SoftMap[A, V](source: Hold[A])(compute: A => V) extends Hold[V] {
    private var count = -1L
    private var sourceCount = -1L
    private val mySoft = Soft(source){ src =>
      val (a, ca) = src.get
      count += 1
      sourceCount = ca
      (compute(a), count)
    }

    def release(): Unit = { source.release(); mySoft.forget() }
    def recompute() = this.synchronized{ mySoft.forget(); mySoft.value }
    def getOrUnit = this.synchronized {
      if source.getOrUnit.forall{ scs => sourceCount != scs._2 } then
        sourceCount = -1L
        mySoft.forget()
      mySoft.valueOrUnit
    }
  }

}
