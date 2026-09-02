// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.alien


import java.io.{InputStream, OutputStream}
import java.util.concurrent.{ArrayBlockingQueue, ExecutorService, Executors, LinkedBlockingQueue, TimeUnit}

import io.grpc.{BindableService, CallOptions, Channel, Drainable, KnownLength, ManagedChannel, ManagedChannelBuilder}
import io.grpc.{Metadata, MethodDescriptor, Server, ServerBuilder, ServerMethodDefinition, ServerServiceDefinition}
import io.grpc.{Status, StatusException, StatusRuntimeException}
import io.grpc.inprocess.{InProcessChannelBuilder, InProcessServerBuilder}
import io.grpc.stub.{ClientCalls, ServerCalls, ServerCallStreamObserver, StreamObserver}

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.loom.{Chan, ChanIn, ChanN, ChanOut, RunStatus}


/** The ceremony-free rim of grpc-java, for either side of a connection: typed marshallers
  * straight off `Pb.Companion`, method descriptors and service assembly from plain functions,
  * `Ask`-valued calls, and closeable server/channel handles.  grpc-java stays in charge of
  * everything it is good at -- HTTP/2, flow control, deadlines, transports -- and this layer
  * replaces the parts a kse3 program would otherwise re-derive every time: `StreamObserver`
  * thread-safety, error plumbing, and lifecycle.
  *
  * Errors are values.  A failed call comes back as an `Err` carrying the `Status` (dig it out
  * with `Grpc.statusOf`); a server handler refuses by returning `Grpc.or(code, why)` -- or any
  * other `Err`, which the peer sees as `INTERNAL`.  Streams are blocking-flavored: pull requests
  * from an `Inbox`, push responses through a `Sender`, from ordinary straight-line code.  Every
  * executor this layer creates is virtual-thread-per-task, so blocking in a handler is the
  * intended style, not a hazard.
  *
  * Nothing here ties gRPC to the JVM's lifetime.  Servers and channels come wrapped in
  * closeable `Host` and `Link` handles; the executors this layer supplies are virtual threads
  * (daemon by definition), so a closed `Host` leaves the JVM exactly as it found it -- gRPC can
  * be used incidentally, mid-program, with no `sys.exit` anywhere.  (A transport given its own
  * platform event-loop threads is the caller's to stop; grpc's defaults are daemon.)
  *
  * The four call shapes, client side and server side:
  * {{{
  * val m = Grpc.unary("my.pkg.Svc", "Get", Query, Reply)      // descriptor, from generated companions
  * val svc = Grpc.Service().unary(m){ q => Is(Reply(...)) }   // server: A => Ask[B]
  * val host = Grpc.serve(9000)(svc.definition).?              // or serve(builder), or loopback()
  * val ans = Grpc.call(link.channel, m, Query(...))           // client: Ask[Reply]
  * }}}
  * Streaming shapes: `serverStream` is `(A, Sender[B]) => Ask[Unit]` against client-side
  * `stream(...)(f)`; `clientStream` is `Inbox[A] => Ask[B]` against `upload`; `bidi` is
  * `(Inbox[A], Sender[B]) => Ask[Unit]` against `converse`.
  *
  * grpc-java is a compile-only dependency of this module: using `Grpc` means adding grpc-api,
  * grpc-stub, and a transport (grpc-netty-shaded, grpc-okhttp, or grpc-inprocess) to your own
  * build; using only `Pb`/`Proto`/`PbGen` means adding nothing.
  */
object Grpc {

  //////////////////////////////
  /// Errors as values       ///
  //////////////////////////////

  /** An `Err` that remembers its gRPC `Status` (and response trailers, if any).  Client rims
    * produce these from failed calls; server handlers that return one control the status the
    * peer sees.  Anything else that reaches a server rim is reported as `INTERNAL`.
    */
  final class StatusErr(val status: Status, val trailers: Metadata) extends ErrType {
    type E = Status
    def error: Status = status
    override def toString =
      val d = status.getDescription
      if d eq null then status.getCode.toString else s"${status.getCode}: $d"
    def buildLines(sb: MkStr, prefix: String): Unit = ErrType.buildLinesFromString(sb, toString, prefix)
    def toThrowable: Throwable = status.asRuntimeException(trailers)
  }

  def err(status: Status): Err = Err(new StatusErr(status, new Metadata()))
  def err(code: Status.Code, description: String): Err = err(code.toStatus.withDescription(description))

  /** A refusal ready to return from a handler: `Grpc.or(Status.Code.NOT_FOUND, s"no such $id")`. */
  def or[X](code: Status.Code, description: String): X Or Err = Alt(err(code, description))

  /** The `Status` an error will be reported as: the `StatusErr` dug out from under any
    * explanation layers, or `INTERNAL` describing the whole error if there is none.
    */
  def statusOf(e: Err): Status =
    findStatus(e) match
      case se: StatusErr => se.status
      case null => Status.INTERNAL.withDescription(e.toString)

  /** The trailers riding with an error's `StatusErr`, or fresh empty ones. */
  def trailersOf(e: Err): Metadata =
    findStatus(e) match
      case se: StatusErr => se.trailers
      case null => new Metadata()

  private def findStatus(e: Err): StatusErr | Null = e.toOr.fold(_ => null): et =>
    et match
      case se: StatusErr => se
      case ex: ErrType.Explained => findStatus(ex.error)
      case _ => null

  /** An error in the form grpc-java propagates: `statusOf` and `trailersOf`, thrown. */
  def statusRuntime(e: Err): StatusRuntimeException = statusOf(e).asRuntimeException(trailersOf(e))

  /** An `Err` from anything a call can throw, keeping the `Status` when there is one. */
  def errOf(t: Throwable): Err = t match
    case sre: StatusRuntimeException =>
      Err(new StatusErr(sre.getStatus, if sre.getTrailers eq null then new Metadata() else sre.getTrailers))
    case se: StatusException =>
      Err(new StatusErr(se.getStatus, if se.getTrailers eq null then new Metadata() else se.getTrailers))
    case _ => Err(t)

  private inline def rim[X](inline work: => X): Ask[X] =
    try Is(work)
    catch
      case t: StatusRuntimeException => Alt(errOf(t))
      case t: StatusException => Alt(errOf(t))
      case t if t.catchable => Alt(Err(t))


  //////////////////////////////
  /// Marshalling            ///
  //////////////////////////////

  // The stream a marshaller hands grpc: length known up front (so the frame header needs no
  // staging) and drainable straight into the transport's own sink.
  private final class BytesIn(bs: Array[Byte]) extends java.io.ByteArrayInputStream(bs) with KnownLength with Drainable {
    def drainTo(target: OutputStream): Int =
      val n = count - pos
      if n > 0 then target.write(buf, pos, n)
      pos = count
      n
  }

  /** A grpc marshaller for any generated message, straight off its companion.  A payload that
    * does not parse fails the call with `INTERNAL`, which is the marshalling convention.
    */
  def marshal[A <: Pb.Writable](c: Pb.Companion[A]): MethodDescriptor.Marshaller[A] =
    new MethodDescriptor.Marshaller[A] {
      def stream(a: A): InputStream = new BytesIn(a.toBytes)
      def parse(in: InputStream): A = c.parse(in).getOrElse: e =>
        throw Status.INTERNAL.withDescription(s"message did not parse: $e").asRuntimeException()
    }

  /** Verbatim bytes, for proxying or observing calls without interpreting them. */
  val rawBytes: MethodDescriptor.Marshaller[Array[Byte]] =
    new MethodDescriptor.Marshaller[Array[Byte]] {
      def stream(bs: Array[Byte]): InputStream = new BytesIn(bs)
      def parse(in: InputStream): Array[Byte] = in.readAllBytes()
    }


  //////////////////////////////
  /// Method descriptors     ///
  //////////////////////////////

  private def method[A <: Pb.Writable, B <: Pb.Writable](
    t: MethodDescriptor.MethodType, service: String, name: String, in: Pb.Companion[A], out: Pb.Companion[B]
  ): MethodDescriptor[A, B] =
    MethodDescriptor.newBuilder(marshal(in), marshal(out))
      .setFullMethodName(MethodDescriptor.generateFullMethodName(service, name))
      .setType(t)
      .build()

  /** One request, one response.  `service` is the fully-qualified proto name, e.g. `"my.pkg.Svc"`. */
  def unary[A <: Pb.Writable, B <: Pb.Writable](service: String, name: String, in: Pb.Companion[A], out: Pb.Companion[B]): MethodDescriptor[A, B] =
    method(MethodDescriptor.MethodType.UNARY, service, name, in, out)

  /** One request, a stream of responses. */
  def serverStream[A <: Pb.Writable, B <: Pb.Writable](service: String, name: String, in: Pb.Companion[A], out: Pb.Companion[B]): MethodDescriptor[A, B] =
    method(MethodDescriptor.MethodType.SERVER_STREAMING, service, name, in, out)

  /** A stream of requests, one response. */
  def clientStream[A <: Pb.Writable, B <: Pb.Writable](service: String, name: String, in: Pb.Companion[A], out: Pb.Companion[B]): MethodDescriptor[A, B] =
    method(MethodDescriptor.MethodType.CLIENT_STREAMING, service, name, in, out)

  /** Streams both ways, each ending independently. */
  def bidi[A <: Pb.Writable, B <: Pb.Writable](service: String, name: String, in: Pb.Companion[A], out: Pb.Companion[B]): MethodDescriptor[A, B] =
    method(MethodDescriptor.MethodType.BIDI_STREAMING, service, name, in, out)


  //////////////////////////////
  /// Streaming endpoints    ///
  //////////////////////////////

  /** The outbound half of a stream: serialized (grpc observers are not thread-safe and real
    * streams get written from several threads), and closed at most once, after which sends
    * report `false` rather than throwing.  A peer that cancelled looks the same as a stream
    * that ended: the message goes nowhere and `send` says so.
    */
  final class Sender[A] private[alien] (obs: StreamObserver[A]) {
    private val lock = new Object
    private var open = true

    /** Send one message; false if the stream is closed or the peer is gone. */
    def send(a: A): Boolean = lock.synchronized:
      if !open then false
      else
        try
          obs.onNext(a)
          true
        catch case e if e.catchable =>
          open = false
          false

    /** True until the stream is completed, errored, cancelled, or found dead. */
    def alive: Boolean = lock.synchronized(open)

    /** End the stream normally (idempotent). */
    def complete(): Unit = lock.synchronized:
      if open then
        open = false
        nice{ obs.onCompleted() } __ Unit

    /** End the stream with an error (idempotent).  From a server handler the peer sees
      * `statusOf(e)`; from a client feed it cancels the call.
      */
    def error(e: Err): Unit = lock.synchronized:
      if open then
        open = false
        nice{ obs.onError(statusRuntime(e)) } __ Unit

    /** Send everything a channel yields, blocking as the channel and grpc flow control ask.
      * A `Chan` that closes cleanly (or a finite `Chan.Source`) ends the pump with `Is(())` --
      * the stream itself is NOT completed, so more may be sent (and the handler/`converse`
      * machinery completes on success anyway).  A channel that errored sends its error to the
      * peer and reports it; a peer that went away reports `CANCELLED`.  Waits only on channels
      * that can actually block -- a merely-empty non-blocking channel is refused, loudly,
      * rather than spun on or silently truncated.
      */
    def from(ch: ChanIn[A]): Ask[Unit] =
      var outcome: Err Or Unit = Alt.unit
      var going = true
      while going do
        ch.recv().fold{ a =>
          if !send(a) then
            outcome = Is(err(Status.Code.CANCELLED, "stream closed while feeding from channel"))
            going = false
        }{
          case RunStatus.Fail(e) =>
            error(e)
            outcome = Is(e)
            going = false
          case RunStatus.Wait =>
            outcome = Is(Err("cannot feed a stream from a non-blocking channel that is merely empty"))
            going = false
          case _ =>
            going = false
        }
      outcome.fold(e => Alt(e))(_ => Is(()))

    private[alien] def quietClose(): Unit = lock.synchronized{ open = false }
  }
  object Sender {
    private[alien] def apply[A](obs: StreamObserver[A]): Sender[A] =
      val s = new Sender[A](obs)
      obs match
        case sso: ServerCallStreamObserver[?] => nice{ sso.setOnCancelHandler(() => s.quietClose()) } __ Unit
        case _ => ()
      s
  }

  /** The inbound half of a stream, pulled rather than pushed: `next()` blocks for the next
    * message (`Is(Is(a))`), the clean end of the stream (`Is(Alt.unit)`), or the stream's
    * error (`Alt(err)`); after a terminal answer it keeps giving the same one.  Single
    * consumer.  The buffer is bounded, and a full one blocks the transport's delivery
    * callback, which is exactly grpc's flow-control signal -- a slow consumer slows the peer
    * instead of accumulating memory.
    */
  final class Inbox[A] private[alien] (capacity: Int) {
    private val q = new LinkedBlockingQueue[AnyRef](if capacity < 1 then 1 else capacity)
    @volatile private var trouble: Err Or Unit = Alt.unit
    private var over = false

    private[alien] val observer: StreamObserver[A] = new StreamObserver[A] {
      def onNext(a: A): Unit = q.put(a.asInstanceOf[AnyRef])
      def onError(t: Throwable): Unit =
        trouble = Is(errOf(t))
        q.put(Inbox.End)
      def onCompleted(): Unit = q.put(Inbox.End)
    }

    private def ended(): Ask[A Or Unit] =
      trouble.fold(e => Alt(e))(_ => Is(Alt.unit))

    /** The next message, the end of the stream, or the stream's error; blocks. */
    def next(): Ask[A Or Unit] =
      if over then ended()
      else q.take() match
        case Inbox.End =>
          over = true
          ended()
        case a => Is(Is(a.asInstanceOf[A]))

    /** Run `f` on each remaining message; `Is(())` at a clean end, or the stream's error. */
    def each(f: A => Unit): Ask[Unit] = Or.Ret:
      var going = true
      while going do
        next().?.fold(f)(_ => going = false)

    /** Move every remaining message into a channel, closing it when the stream ends so
      * consumers unblock -- this is how a stream joins a `Go` select world.  The stream's
      * error is the answer (the channel still just closes; a plain `Chan` has no error to
      * carry).  A receiver that closes its end first gets a clean `Is(())`: it chose to stop
      * listening, and unpulled messages simply stay with grpc's flow control.
      */
    def into(ch: Chan[A]): Ask[Unit] = intoImpl(ch, () => ch.close() __ Unit)

    def into(ch: ChanN[A]): Ask[Unit] = intoImpl(ch, () => ch.close() __ Unit)

    private def intoImpl(ch: ChanOut[A], shut: () => Unit): Ask[Unit] =
      var outcome: Ask[Unit] = Is(())
      var going = true
      while going do
        next().fold{ x =>
          x.fold{ a =>
            ch.send(a) match
              case RunStatus.Okay => ()
              case RunStatus.Fail(e) =>
                outcome = Alt(e)
                going = false
              case _ =>
                going = false
          }{ _ =>
            shut()
            going = false
          }
        }{ e =>
          shut()
          outcome = Alt(e)
          going = false
        }
      outcome
  }
  object Inbox {
    private object End
    /** How many undelivered messages an `Inbox` holds before it pushes back on the peer. */
    val DefaultCapacity = 32
  }


  /** Id-correlated calls multiplexed over one long-lived stream -- for protocols that speak
    * their own request/reply INSIDE a session (bidi) stream instead of one gRPC call per
    * exchange.  That is the shape whenever authority runs against transport: the sophisticated
    * peer is nominally the gRPC *client*, its workers are nominally *servers*, and a worker
    * that needs to ask the orchestrator something has no gRPC method to do it with -- so both
    * directions ride the session stream, matched by correlation id (LSP, DevTools, and plugin
    * hosts all live here).
    *
    * One correlator serves one stream and one direction of asking; a peer that both asks and
    * answers runs one correlator and one dispatch.  `request` allocates an id, builds and
    * sends the message, and blocks for the reply; the stream's read loop offers everything
    * it receives to `deliver`, and whatever is not claimed there is the peer's own traffic to
    * dispatch (their requests, unsolicited pushes).  When the stream dies, `fail` wakes every
    * waiter and every later `request` refuses immediately.  Replies that are semantically
    * errors (a protocol's error arm) are still just replies here -- classifying them is the
    * protocol's business, after `request` returns.
    */
  final class Correlator[A, B](snd: Sender[A], prefix: String = "q", timeoutMs: Long = 30000L) {
    private val seq = new java.util.concurrent.atomic.AtomicLong(0L)
    private val waiters = new java.util.concurrent.ConcurrentHashMap[String, ArrayBlockingQueue[AnyRef]]
    @volatile private var down: Err Or Unit = Alt.unit

    /** Send a request carrying a fresh id (embed it via `build`) and block for its reply. */
    def request(build: String => A): Ask[B] = request(timeoutMs)(build)

    def request(patienceMs: Long)(build: String => A): Ask[B] =
      val id = prefix + seq.getAndIncrement()
      val q = new ArrayBlockingQueue[AnyRef](1)
      waiters.put(id, q) __ Unit
      try
        down.fold{ e => Alt(e) }{ _ =>
          if !snd.send(build(id)) then Alt(Err("session stream is closed"))
          else q.poll(patienceMs, TimeUnit.MILLISECONDS) match
            case null => Alt(Err(s"no reply to request $id within ${patienceMs} ms"))
            case x => x.asInstanceOf[Ask[B]]
        }
      catch case t if t.catchable => Alt(Err(t))
      finally waiters.remove(id) __ Unit

    /** Route a reply to its waiter; false if `id` claims nobody -- then it is not a reply,
      * and the caller's dispatch handles it.
      */
    def deliver(id: String, b: B): Boolean =
      val q = waiters.get(id)
      if q eq null then false else q.offer(Is(b).asInstanceOf[AnyRef])

    /** The session is over: refuse future requests and wake every waiter with `e`. */
    def fail(e: Err): Unit =
      down = Is(e)
      waiters.forEach((_, q) => q.offer(Alt(e).asInstanceOf[AnyRef]) __ Unit)

    /** How many requests are waiting on replies right now. */
    def pending: Int = waiters.size
  }


  //////////////////////////////
  /// The server side        ///
  //////////////////////////////

  /** Handlers as plain functions, assembled into a service.  The service name comes from the
    * method descriptors (grpc checks they all agree), so it is never spelled twice.  Handlers
    * may block -- `serve` and `loopback` run them on virtual threads -- and refuse by returning
    * `Grpc.or(code, why)`; any other `Err` (or escaped exception) reaches the peer as
    * `INTERNAL`.  Streaming handlers own their whole stream: return `Is(())` after the last
    * send and this layer completes it.
    */
  final class Service() extends BindableService {
    private var entries: List[ServerMethodDefinition[?, ?]] = Nil

    private def add[A, B](m: MethodDescriptor[A, B], h: io.grpc.ServerCallHandler[A, B]): this.type =
      entries = ServerMethodDefinition.create(m, h) :: entries
      this

    def unary[A, B](m: MethodDescriptor[A, B])(f: A => Ask[B]): this.type =
      add(m, ServerCalls.asyncUnaryCall(new ServerCalls.UnaryMethod[A, B] {
        def invoke(a: A, obs: StreamObserver[B]): Unit =
          Ask.flat{ f(a) }.fold{ b =>
            nice{ obs.onNext(b); obs.onCompleted() } __ Unit   // throws only if the call died; nothing to tell it
          }{ e =>
            nice{ obs.onError(statusRuntime(e)) } __ Unit
          }
      }))

    def serverStream[A, B](m: MethodDescriptor[A, B])(f: (A, Sender[B]) => Ask[Unit]): this.type =
      add(m, ServerCalls.asyncServerStreamingCall(new ServerCalls.ServerStreamingMethod[A, B] {
        def invoke(a: A, obs: StreamObserver[B]): Unit =
          val snd = Sender(obs)
          Ask.flat{ f(a, snd) }.fold(_ => snd.complete())(e => snd.error(e))
      }))

    def clientStream[A, B](m: MethodDescriptor[A, B], capacity: Int = Inbox.DefaultCapacity)(f: Inbox[A] => Ask[B]): this.type =
      add(m, ServerCalls.asyncClientStreamingCall(new ServerCalls.ClientStreamingMethod[A, B] {
        def invoke(obs: StreamObserver[B]): StreamObserver[A] =
          val inbox = new Inbox[A](capacity)
          // The handler pulls, so it cannot run on the thread that must return the request
          // observer; it gets a virtual thread of its own.
          Thread.ofVirtual().name("kse-grpc-handler").start{ () =>
            Ask.flat{ f(inbox) }.fold{ b =>
              nice{ obs.onNext(b); obs.onCompleted() } __ Unit
            }{ e =>
              nice{ obs.onError(statusRuntime(e)) } __ Unit
            }
          } __ Unit
          inbox.observer
      }))

    def bidi[A, B](m: MethodDescriptor[A, B], capacity: Int = Inbox.DefaultCapacity)(f: (Inbox[A], Sender[B]) => Ask[Unit]): this.type =
      add(m, ServerCalls.asyncBidiStreamingCall(new ServerCalls.BidiStreamingMethod[A, B] {
        def invoke(obs: StreamObserver[B]): StreamObserver[A] =
          val inbox = new Inbox[A](capacity)
          val snd = Sender(obs)
          Thread.ofVirtual().name("kse-grpc-handler").start{ () =>
            Ask.flat{ f(inbox, snd) }.fold(_ => snd.complete())(e => snd.error(e))
          } __ Unit
          inbox.observer
      }))

    /** The assembled service; grpc rejects methods whose descriptors name different services. */
    def definition: ServerServiceDefinition =
      entries match
        case Nil => throw new IllegalStateException("gRPC service with no methods")
        case es =>
          val full = es.last.getMethodDescriptor.getFullMethodName
          val name = MethodDescriptor.extractFullServiceName(full)
          if name eq null then throw new IllegalStateException(s"method name '$full' has no service part")
          val b = ServerServiceDefinition.builder(name)
          es.reverse.foreach(e => b.addMethod(e) __ Unit)
          b.build()

    def bindService(): ServerServiceDefinition = definition
  }


  //////////////////////////////
  /// The client side        ///
  //////////////////////////////

  /** One request, one response, blocking; the failure `Status` rides the `Err`. */
  def call[A, B](ch: Channel, m: MethodDescriptor[A, B], a: A, opts: CallOptions = CallOptions.DEFAULT): Ask[B] =
    rim{ ClientCalls.blockingUnaryCall(ch, m, opts, a) }

  /** One request, each response fed to `f` as it arrives; returns when the stream ends. */
  def stream[A, B](ch: Channel, m: MethodDescriptor[A, B], a: A, opts: CallOptions = CallOptions.DEFAULT)(f: B => Unit): Ask[Unit] =
    rim:
      val it = ClientCalls.blockingServerStreamingCall(ch, m, opts, a)
      while it.hasNext do f(it.next())

  /** Feed a stream of requests through the `Sender`, then collect the one response.  Return
    * `Is(())` from `feed` when done sending; return an `Err` to cancel the call instead.
    */
  def upload[A, B](ch: Channel, m: MethodDescriptor[A, B], opts: CallOptions = CallOptions.DEFAULT)(feed: Sender[A] => Ask[Unit]): Ask[B] =
    val answer = new OneShot[B]
    rim{ Sender(ClientCalls.asyncClientStreamingCall(ch.newCall(m, opts), answer)) }.flatMap: snd =>
      Ask.flat{ feed(snd) }.fold{ _ =>
        snd.complete()
        answer.await()
      }{ e =>
        snd.error(e)
        Alt(e)
      }

  /** Converse over a bidi stream: send through the `Sender`, pull replies from the `Inbox`,
    * in whatever order the conversation wants.  When `f` returns `Is(())` the request stream
    * is completed and any unread replies are drained (so a late server error still surfaces);
    * an `Err` from `f` cancels the call.
    */
  def converse[A, B](
    ch: Channel, m: MethodDescriptor[A, B], opts: CallOptions = CallOptions.DEFAULT, capacity: Int = Inbox.DefaultCapacity
  )(f: (Sender[A], Inbox[B]) => Ask[Unit]): Ask[Unit] =
    val inbox = new Inbox[B](capacity)
    rim{ Sender(ClientCalls.asyncBidiStreamingCall(ch.newCall(m, opts), inbox.observer)) }.flatMap: snd =>
      Ask.flat{ f(snd, inbox) }.fold{ _ =>
        snd.complete()
        inbox.each(_ => ())
      }{ e =>
        snd.error(e)
        Alt(e)
      }

  // A unary response: at most one value, then exactly one terminal signal.
  private final class OneShot[B] extends StreamObserver[B] {
    private val q = new ArrayBlockingQueue[AnyRef](1)
    private var value: B Or Unit = Alt.unit
    def onNext(b: B): Unit = { value = Is(b) }
    def onError(t: Throwable): Unit = q.put(Alt(errOf(t)).asInstanceOf[AnyRef])
    def onCompleted(): Unit =
      q.put(value.fold(b => Is(b): Ask[B])(_ => Alt(Err("stream completed with no response"))).asInstanceOf[AnyRef])
    def await(): Ask[B] = Ask.flat{ q.take().asInstanceOf[Ask[B]] }
  }


  //////////////////////////////
  /// Lifecycle              ///
  //////////////////////////////

  /** The executor this layer hands grpc when it owns the choice: virtual threads, so handlers
    * may block and the JVM is never held open by an idle server.
    */
  def virtualThreads(): ExecutorService = Executors.newVirtualThreadPerTaskExecutor()

  /** A running server, closeable like anything else: `close()` refuses new calls, gives
    * in-flight ones a grace period, then cuts them off and releases any executor this layer
    * created.  Nothing waits on the JVM and the JVM waits on nothing.
    */
  final class Host private[alien] (val server: Server, ownedExec: ExecutorService | Null) extends java.io.Closeable {
    /** The bound port, or -1 for portless transports (in-process). */
    def port: Int = server.getPort
    def close(): Unit = closeIn(2000)
    def closeIn(graceMs: Long): Unit =
      server.shutdown() __ Unit
      var quit = false
      try quit = server.awaitTermination(graceMs, TimeUnit.MILLISECONDS)
      catch case _: InterruptedException => Thread.currentThread.interrupt()
      if !quit then
        server.shutdownNow() __ Unit
        try server.awaitTermination(graceMs, TimeUnit.MILLISECONDS) __ Unit
        catch case _: InterruptedException => Thread.currentThread.interrupt()
      if ownedExec ne null then ownedExec.shutdown()
  }

  /** A client channel, closeable the same way as [[Host]]. */
  final class Link private[alien] (val channel: ManagedChannel, ownedExec: ExecutorService | Null) extends java.io.Closeable {
    def close(): Unit = closeIn(2000)
    def closeIn(graceMs: Long): Unit =
      channel.shutdown() __ Unit
      var quit = false
      try quit = channel.awaitTermination(graceMs, TimeUnit.MILLISECONDS)
      catch case _: InterruptedException => Thread.currentThread.interrupt()
      if !quit then
        channel.shutdownNow() __ Unit
        try channel.awaitTermination(graceMs, TimeUnit.MILLISECONDS) __ Unit
        catch case _: InterruptedException => Thread.currentThread.interrupt()
      if ownedExec ne null then ownedExec.shutdown()
  }

  /** Serve on a builder configured by the caller (transport, TLS, executor all theirs).
    * Handlers may block, so give the builder an executor that tolerates that --
    * `Grpc.virtualThreads()` is the easy answer.
    */
  def serve(b: ServerBuilder[?])(services: ServerServiceDefinition*): Ask[Host] = nice:
    var bb: ServerBuilder[?] = b
    services.foreach(s => bb = bb.addService(s))
    new Host(bb.build().start(), null)

  /** Serve on a TCP port with whatever transport is on the classpath, handlers on virtual
    * threads.  Port 0 picks a free port; read it back from `Host.port`.
    */
  def serve(port: Int)(services: ServerServiceDefinition*): Ask[Host] =
    val ex = virtualThreads()
    val ans = nice:
      var bb: ServerBuilder[?] = ServerBuilder.forPort(port).executor(ex)
      services.foreach(s => bb = bb.addService(s))
      new Host(bb.build().start(), ex)
    if !ans.isIs then ex.shutdown()
    ans

  /** Connect via a builder configured by the caller (transport, TLS, executor all theirs). */
  def connect(b: ManagedChannelBuilder[?]): Ask[Link] = nice{ new Link(b.build(), null) }

  /** Connect in plaintext with callbacks on virtual threads: the same-host case -- a local
    * port, a test server -- where TLS would be ceremony.  Anything crossing a real network
    * deserves a configured builder and `connect` instead.
    */
  def connectLocal(target: String): Ask[Link] =
    val ex = virtualThreads()
    val ans = nice{ new Link(ManagedChannelBuilder.forTarget(target).usePlaintext().executor(ex).build(), ex) }
    if !ans.isIs then ex.shutdown()
    ans

  /** A server and a channel joined by the in-process transport: real grpc semantics, no
    * network, no extra transport dependency beyond grpc-inprocess.  This is the test-server
    * and test-client story -- a serious client gets exercised against a loopback service, and
    * a serious server against loopback calls, in one process.  Close both when done (the
    * `Link` first, politely).
    */
  def loopback(services: ServerServiceDefinition*): Ask[(Host, Link)] = Or.Ret:
    val name = InProcessServerBuilder.generateName()
    val sex = virtualThreads()
    val host =
      val ans = nice:
        var bb: ServerBuilder[?] = InProcessServerBuilder.forName(name).executor(sex)
        services.foreach(s => bb = bb.addService(s))
        new Host(bb.build().start(), sex)
      if !ans.isIs then sex.shutdown()
      ans.?
    val cex = virtualThreads()
    val link =
      val ans = nice{ new Link(InProcessChannelBuilder.forName(name).executor(cex).build(), cex) }
      if !ans.isIs then
        cex.shutdown()
        host.close()
      ans.?
    (host, link)
}
