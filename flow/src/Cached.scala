package kse.flow

import java.util.concurrent.atomic._
import java.lang.ref.SoftReference


/** A general way to defer a computation but cache the result. */
final class Lazy[V](gen: => V) {
  lazy val value = gen
  def map[W](f: V => W) = Lazy(f(value))
  def flatMap[W](f: V => Lazy[W]) = Lazy(f(value).value)
}
object Lazy {
  def apply[V](gen: => V) = new Lazy(gen)
}


/** An unset value that you can set but then not change (thread-safe) */
final class Worm[V] {
  private[this] val myValue: AtomicReference[AnyRef] = new AtomicReference[AnyRef](Worm.storedNullSentinel)

  def setIfEmpty(v: => V): Boolean =
    var old = myValue.get()
    if (old eq Worm.storedNullSentinel) {
      myValue.compareAndSet(old, v.asInstanceOf[AnyRef])
    }
    else false

  def set(v: => V): this.type =
    if (!setIfEmpty(v)) throw new java.lang.IllegalStateException("Set while already set")
    this

  def get: V = myValue.get() match
    case x if x eq Worm.storedNullSentinel => throw new java.lang.IllegalStateException("Retrieved value before being set")
    case x                                 => x.asInstanceOf[V]

  def value: V Or Unit = myValue.get() match
    case x if x eq Worm.storedNullSentinel => Alt.unit
    case x                                 => Is(x.asInstanceOf[V])
}
object Worm {
  def of[V]: Worm[V] = new Worm[V]

  private[Worm] val storedNullSentinel: AnyRef = new AnyRef {}
}


/** Caches expensive computations that are cleared when memory gets especially tight (via SoftReference); not thread-safe */
final class Soft[S, V](source: S)(gen: S => V) {
  private[this] var myCache: SoftReference[AnyRef] = new SoftReference(null)

  def value: V = myCache.get() match
    case null =>
      val v = gen(source)
      myCache = new SoftReference(if (v.asInstanceOf[AnyRef] eq null) Soft.storedNullSentinel else v.asInstanceOf[AnyRef])
      v
    case x if x.asInstanceOf[AnyRef] eq Soft.storedNullSentinel =>
      null.asInstanceOf[V]
    case x =>
      x.asInstanceOf[V]

  def map[W](f: V => W) = Soft(source)(gen andThen f)
}
object Soft {
  def apply[S, V](source: S)(gen: S => V) = new Soft(source)(gen)

  private[Soft] val storedNullSentinel: AnyRef = new AnyRef {}
}


/** Clearable, chainable, thread-safe caching with hidden extra (captured) state.  Fairly heavyweight operation, so use for work that is significant. */
sealed trait Hold[V] {
  /** Indicate that this value should be recomputed next access. */
  def invalidate(): Unit

  /** Indicate that this value, and everything it depends on, should be released and recomputed. */
  def release(): Unit

  /** Get a count of the number of computations (which serves as a timestamp of sorts) */
  def computations(): Long

  /** Get the current value and current count of computations */
  def get(): (V, Long)

  /** Get the current value */
  def value: V

  /** Produce a new Hold that depends on this one. */
  final def map[A](f: V => A): Hold[A] = Hold.map(this)(f)
}
object Hold {
  sealed abstract class Checking[H, V, C](hidden: H) extends Hold[V] {
    // These things must only be used while synchronized
    protected var h: H = hidden
    protected var v: Any = Checking.invalidated
    protected var n: Long = 0L
    protected def reusable(context: C): Boolean    // Called EXACTLY ONCE
    protected def advance(context: C): V           // May NOT call reusable()
    // End of things that must be used synchronized

    final def invalidate(): Unit = this.synchronized { 
      if (!v.isInstanceOf[Checking.Empty]) v = Checking.invalidated
    }

    final def computations(): Long = this.synchronized { n }

    final protected def get(context: C): (V, Long) = this.synchronized {
      if (!reusable(context) || (v.isInstanceOf[Checking.Empty])) {    // Note--reusable comes first so it is called exactly once
        v = advance(context)
        n += 1
      }
      (v.asInstanceOf[V], n)
    }

    final protected def value(context: C): V = this.synchronized {
      if (!reusable(context) || (v.isInstanceOf[Checking.Empty])) {   // Note--reusable comes first so it is called exactly once
        v = advance(context)
        n += 1
      }
      v.asInstanceOf[V]
    }
  }
  object Checking {
    private[Hold] class Empty {}
    private[Hold] val invalidated = new Empty {}
    private[Hold] val released = new Empty {}
  }

  sealed abstract class Caching[H, V](hidden: H)(gen: H => V) extends Checking[H, V, Unit](hidden) {
    protected def reusable(): Boolean
    final def release(): Unit = invalidate()
    final def reusable(context: Unit): Boolean = reusable()
    final def advance(context: Unit): V = gen(h)
    def get(): (V, Long) = get(())
    def value: V = value(())
  }

  sealed abstract class Mapping[V, C] extends Checking[Long, V, C](0L) {
    protected def gatherContext(): C         // Must NOT use within synchronized!
    protected def extract(context: C): Long
    protected def forget(): Unit

    final def release(): Unit =
      val done = this.synchronized { 
        val ans = v.asInstanceOf[AnyRef] eq Checking.released
        if (!ans) v = Checking.released
        ans
      }
      if (!done) forget()

    protected def reusable(context: C): Boolean =
      val m = extract(context)
      val ans = m == h
      if (!ans) h = m
      ans

    final def get(): (V, Long) = get(gatherContext())

    final def value: V = value(gatherContext())
  }

  def apply[V](gen: => V): Hold[V] =
    new Caching[Unit, V](())(_ => gen) { protected def reusable() = true }

  def tested[V](zero: V)(test: V => Boolean)(next: V => V): Hold[V] =
    new Caching[V, V](zero)(next) {
      protected def reusable() =
        if (v.isInstanceOf[Checking.Empty]) false
        else {
          h = v.asInstanceOf[V]
          test(h)
        }
    }

  final class Counted(uses: Long) {
    def apply[V](gen: => V): Hold[V] =
      new Caching[Long, V](0L)(_ => gen) {
        protected def reusable() =
          if (h < uses) { h += 1; true }
          else          { h = 0; false }
      }
  }
  def counted(uses: Long) = new Counted(uses)

  final class Timed(timeout: java.time.Duration) {
    def apply[V](gen: => V): Hold[V] =
      new Caching[java.time.Instant, V](java.time.Instant.EPOCH)(_ => gen) {
        protected def reusable() =
          val now = java.time.Instant.now
          val ans = timeout.compareTo(java.time.Duration.between(h, now)) >= 0
          if (!ans) h = now
          ans
      }
  }
  def timed(timeout: java.time.Duration) = new Timed(timeout)

  def okay[N, Y](gen: => Ok[N, Y]): Hold[Ok[N, Y]] =
    new Caching[Unit, Ok[N, Y]](())(_ => gen) {
      protected def reusable() = (!v.isInstanceOf[Checking.Empty]) && v.asInstanceOf[Ok[N, Y]].isYes
    }

  def option[A](gen: => Option[A]): Hold[Option[A]] =
    new Caching[Unit, Option[A]](())(_ => gen) {
      protected def reusable() = (!v.isInstanceOf[Checking.Empty]) && v.asInstanceOf[Option[A]].isDefined
    }

  def preserve[V](hV: Hold[V])(p: V => Boolean): Hold[V] =
    new Checking[Long, V, (V, Long)](0L) {
      protected def advance(context: (V, Long)) = context._1

      protected def reusable(context: (V, Long)) =
        val ans = context._2 == h
        if (!ans) h = context._2
        ans

      def release(): Unit =
        hV.release()
        invalidate()

      def get(): (V, Long) =
        val failed = this.synchronized{ !v.isInstanceOf[Checking.Empty] && !p(v.asInstanceOf[V]) }
        if (failed) hV.release()
        get(hV.get())

      def value: V =
        val failed = this.synchronized{ !v.isInstanceOf[Checking.Empty] && !p(v.asInstanceOf[V]) }
        if (failed) hV.release()
        value(hV.get())
    }

  def map[V, A](hV: Hold[V])(f: V => A): Hold[A] =
    new Mapping[A, (V, Long)] {
      protected def forget(): Unit = hV.release()
      protected def gatherContext() = hV.get()
      protected def extract(context: (V, Long)) = context._2
      protected def advance(context: (V, Long)) = f(context._1)
    }

  def map[V, U, A](hV: Hold[V], hU: Hold[U])(f: (V, U) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release() }
      protected def gatherContext() = (hV.get(), hU.get())
      protected def extract(context: ((V, Long), (U, Long))) = context._1._2 + context._2._2
      protected def advance(context: ((V, Long), (U, Long))) = f(context._1._1, context._2._1)
    }

  def map[V, U, T, A](hV: Hold[V], hU: Hold[U], hT: Hold[T])(f: (V, U, T) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long))) = context._1._2 + context._2._2 + context._3._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long))) = f(context._1._1, context._2._1, context._3._1)
    }

  def map[V, U, T, S, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S])(f: (V, U, T, S) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release(); hS.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long))) =
        context._1._2 + context._2._2 + context._3._2 + context._4._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long))) =
        f(context._1._1, context._2._1, context._3._1, context._4._1)
    }

  def map[V, U, T, S, R, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R])(f: (V, U, T, S, R) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release(); hS.release(); hR.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long))) =
        context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long))) = 
        f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1)
    }

  def map[V, U, T, S, R, Q, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R], hQ: Hold[Q])(f: (V, U, T, S, R, Q) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release(); hS.release(); hR.release(); hQ.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get(), hQ.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long))) =
        context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2 + context._6._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long))) = 
        f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1, context._6._1)
    }

  def map[V, U, T, S, R, Q, P, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R], hQ: Hold[Q], hP: Hold[P])(f: (V, U, T, S, R, Q, P) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release(); hS.release(); hR.release(); hQ.release(); hP.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get(), hQ.get(), hP.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long))) =
        context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2 + context._6._2 + context._7._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long))) = 
        f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1, context._6._1, context._7._1)
    }

  def map[V, U, T, S, R, Q, P, O, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R], hQ: Hold[Q], hP: Hold[P], hO: Hold[O])(f: (V, U, T, S, R, Q, P, O) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long), (O, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release(); hS.release(); hR.release(); hQ.release(); hP.release(); hO.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get(), hQ.get(), hP.get(), hO.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long), (O, Long))) =
        context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2 + context._6._2 + context._7._2 + context._8._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long), (O, Long))) = 
        f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1, context._6._1, context._7._1, context._8._1)
    }

  def map[V, U, T, S, R, Q, P, O, N, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R], hQ: Hold[Q], hP: Hold[P], hO: Hold[O], hN: Hold[N])(f: (V, U, T, S, R, Q, P, O, N) => A): Hold[A] =
    new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long), (O, Long), (N, Long))] {
      protected def forget(): Unit = { hV.release(); hU.release(); hT.release(); hS.release(); hR.release(); hQ.release(); hP.release(); hO.release(); hN.release() }
      protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get(), hQ.get(), hP.get(), hO.get(), hN.get())
      protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long), (O, Long), (N, Long))) =
        context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2 + context._6._2 + context._7._2 + context._8._2 + context._9._2
      protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long), (P, Long), (O, Long), (N, Long))) = 
        f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1, context._6._1, context._7._1, context._8._1, context._9._1)
    }
}
