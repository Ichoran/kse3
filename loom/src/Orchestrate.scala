// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.loom

import kse.basics._
import kse.flow._


/** The outcome of a non-blocking step on a channel, a select-loop handler, or a muncher mailbox:
  * progress, a transient block, a clean finish, or an error.  Sends and select-loop steps return it
  * directly; a receive carries its value in the `Is` of an `A Or RunStatus`, so there a success *is*
  * the favored branch and `Okay` does not appear.
  *
  * (Named `RunStatus`, not a bare `Status` — far too many things have a "status" to claim that word
  * at this scope.) */
enum RunStatus:
  case Okay              // progress: a value moved, or a handler ran
  case Wait              // transient: full (send) or empty (recv), still open; or a loop is idle
  case Done              // permanent: channel closed/drained; or a handler is inactive
  case Fail(e: Err)      // permanent: errored
object RunStatus:
  // Pre-allocated alts for the empty-poll receive paths, so they don't rebox an `Alt` every call.
  // The favored side of an `Or` is phantom, so one instance serves every element type.
  private[loom] val altWait: Alt[RunStatus] = Alt(RunStatus.Wait)
  private[loom] val altDone: Alt[RunStatus] = Alt(RunStatus.Done)


/** A self-managing context you run *inside* — a [[Go]] task or a [[Munch.Context]] muncher.  Both
  * support deferred cleanup ([[Defer]]) and graceful self-stop ([[Stop]]), so those verbs dispatch
  * over this union and read identically in either world.  (`Go.session`'s richer `Stop.on`/
  * `Stop.session` stay `Go`-only — they need the select loop / the scope tree.) */
type Orchestrated = Go | Munch.Context[?]


/** Stop the current task or muncher.  `Stop()` stops it now, gracefully — it finishes (flushes /
  * drains) what it already holds, then tears down — in whichever context is in scope.  `Stop.on`
  * and `Stop.session` are `Go`-only. */
object Stop {
  /** Gracefully stop the current task/muncher (works in a `Go` task or a `Munch` muncher). */
  inline def apply()(using inline ctx: Orchestrated): Unit =
    inline ctx match
      case g: Go               => g.stopSelf()
      case c: Munch.Context[?] => c.stopSelf()

  /** (`Go` only) stop the current task once `cond` holds. */
  def on(cond: => Boolean)(using go: Go): Unit = go.addStopCond(() => cond)

  /** (`Go` only) gracefully stop the whole scope tree. */
  def session()(using go: Go): Unit = go.coord.stopAll()
}


/** Register cleanup to run when the current task/muncher finishes, in whichever context is in scope.
  * Multiple `Defer`s run last-registered-first (LIFO), like Scala's `Using` or Go's `defer`.
  *
  *  - `Defer { … }` always runs.
  *  - `Defer.ifOkay { … }` runs only if it ended without error — which **includes** a graceful
  *    `Stop`; only an actual failure (or a `Go` `cancel()` / a `Munch` decider that didn't clear it)
  *    is not-okay.
  *  - `Defer.ifFail { … }` runs only if it ended with an error.
  *  - `Defer.withErrorView { errs => … }` always runs, handed the errors bundled for this
  *    task/muncher (empty when it ended okay).
  *
  * A `Defer` that itself throws is recorded as a failure, so any later (lower-on-the-stack) `Defer`
  * sees a non-empty error list. */
object Defer {
  /** Always run `body` at the end (handed nothing). */
  inline def apply(inline body: => Unit)(using inline ctx: Orchestrated): Unit =
    register(ctx)(_ => body)

  /** Run `body` only if it ended okay (no error; a graceful `Stop` counts as okay). */
  inline def ifOkay(inline body: => Unit)(using inline ctx: Orchestrated): Unit =
    register(ctx)(es => if es.isEmpty then body)

  /** Run `body` only if it ended with an error. */
  inline def ifFail(inline body: => Unit)(using inline ctx: Orchestrated): Unit =
    register(ctx)(es => if es.nonEmpty then body)

  /** Always run `body`, handed the errors bundled for this task/muncher (empty means it ended okay). */
  inline def withErrorView(inline body: List[Err] => Unit)(using inline ctx: Orchestrated): Unit =
    register(ctx)(body)

  private inline def register(inline ctx: Orchestrated)(body: List[Err] => Unit): Unit =
    inline ctx match
      case g: Go               => g.addDefer(body)
      case c: Munch.Context[?] => c.addDefer(body)
}
