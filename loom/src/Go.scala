// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr.

package kse.loom


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.util.concurrent.{Future, ExecutorService, ArrayBlockingQueue, SynchronousQueue}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock

import scala.util.boundary
import scala.util.boundary.Label
import scala.reflect.ClassTag

import kse.basics._
import kse.flow._



final class Chan[A](bufferSize: Int) {
  private val buffer = new Array[AnyRef](if bufferSize <= 0 then 1 else if bufferSize > Int.MaxValue - 7 then Int.MaxValue - 7 else bufferSize)
  private val lock = new ReentrantLock()
  private val mightHaveSpace = lock.newCondition()
  private val mightHaveItems = lock.newCondition()
  private var i0 = 0
  private var iN = 0
  private var state: Chan.State = Chan.State.Open

  private inline def locked[A](inline f: => A): A =
    lock.lockInterruptibly()
    try f
    finally lock.unlock()

  inline def +=?(a: A)(using boundary.Label[Chan.State]): Unit = locked:
    var attempt = true
    while attempt do
      state match
        case Chan.State.Open =>
          if iN - i0 >= buffer.length then mightHaveSpace.await()
          else
            buffer(iN % buffer.length) = a.asInstanceOf[AnyRef]
            iN += 1
            attempt = false
            mightHaveItems.signal()
        case c => boundary.break(c)

  inline def take_?(using boundary.Label[Chan.State]): A = locked:
    while iN == i0 do
      state match
        case Chan.State.Open => mightHaveItems.await()
        case c => boundary.break(c)
    state match
      case c: Chan.State.Closed => boundary.break(c)
      case _ =>
        val i = i0 % buffer.length
        val ans = buffer(i).asInstanceOf[A]
        buffer(i) = null
        i0 += 1
        mightHaveSpace.signal()
        ans

  inline def retire(): Unit = locked:
    state match
      case Chan.State.Open =>
        state = Chan.State.ReadOnly
        mightHaveSpace.signalAll()
        mightHaveItems.signalAll()
      case _ =>

  inline def close(): Unit = locked:
    state match
      case c: Chan.State.Closed =>
      case _ =>
        state = Chan.completed
        mightHaveSpace.signalAll()
        mightHaveItems.signalAll()

  inline def panic(err: Err)(using boundary.Label[Chan.State]): Nothing = locked:
    state = state match
      case Chan.State.Closed(Alt(e)) => Chan.State.Closed(Alt(Err(e, err)("")))
      case _ => Chan.State.Closed(Alt(err))
    boundary.break(state)
}
object Chan {
  
  
  enum State:
    case Open
    case ReadOnly
    case Closed(why: Ask[Unit])
  val completed = State.Closed(Is.unit)
  val altOpen = Alt(State.Open)
  val altReadOnly = Alt(State.ReadOnly)
  val altClosed = Alt(completed)
}



/*
final class Concurrent private (f: => Unit, registrar: Concurrent.Registrar) {}
object Concurrent {
  enum Signal:
    case Live
    case Done
    case Halt

  def apply(f: Concurrent.Registrar ?=> Unit): Concurrent =
    val reg = new Registrar()
    new Concurrent(f(using reg), reg)

  final class Registrar() {
    def apply(f: () => Unit): Unit = {}
    def flat(f: () => Ask[Unit]): Unit = {}
    def select(fs: Array[() => (Unit | Signal | Ask[Unit])])
  }
}
object Go {
  inline def apply(f: => Unit)(using reg: Concurrent.Registrar): Unit =
    reg(() => f)

  inline def flat(f: => Ask[Unit])(using reg: Concurrent.Registrar): Unit =
    reg.flat(() => f)

  def all(fs: (() => (Unit | Signal | Ask[Unit]))*): Unit =
    reg.repeat(fs.toArray)
}
*/
