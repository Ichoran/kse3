// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.eio


import java.lang.foreign.{Arena, MemorySegment, MemoryLayout, Linker, FunctionDescriptor}
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_BYTE, JAVA_SHORT, JAVA_INT, JAVA_LONG}
import java.lang.invoke.MethodHandle

import kse.basics.{given, _}
import kse.flow.{given, _}


/** The POSIX socket layer beneath [[FdSock]]: platform-resolved constants, the `errno` table with names
  * and classifiers, the struct layouts that differ between 64-bit Linux and macOS, the libc downcalls,
  * and the two forms an error takes here.
  *
  * A system call answers with a [[PosixSocket.Result]] — an opaque `Long` holding a non-negative count or
  * `-errno`, the C shape, so a hot loop classifies without allocating — and an `Ask` that fails on a
  * system call carries a [[PosixSocket.Failed]], so the code survives as data ([[errnoOf]] digs it out of an
  * explained chain).  Everything is public: a native socket user outside `FdSock` should reach for
  * these rather than carry its own copy of the tables.
  *
  * Downcalls live in [[Sys]] and are linked on first use, so touching a constant on Windows costs nothing.
  */
object PosixSocket {
  private lazy val osName = System.getProperty("os.name", "").toLowerCase
  lazy val mac: Boolean   = osName.contains("mac") || osName.contains("darwin")
  lazy val linux: Boolean = osName.contains("nux")
  /** True where these calls exist (Linux and macOS); Windows has no `AF_UNIX` descriptor passing. */
  def supported: Boolean = mac || linux
  def platformName: String = osName


  //////////////////////////////////////////////////////////
  // Socket constants (values differ across the platforms) //
  //////////////////////////////////////////////////////////

  val AF_UNIX        = 1
  val SOCK_STREAM    = 1
  val SOCK_DGRAM     = 2
  val SOCK_SEQPACKET = 5
  lazy val SOL_SOCKET  = if mac then 0xFFFF else 1
  lazy val SO_TYPE     = if mac then 0x1008 else 3
  lazy val SO_SNDBUF   = if mac then 0x1001 else 7
  lazy val SO_RCVBUF   = if mac then 0x1002 else 8
  lazy val SO_SNDTIMEO = if mac then 0x1005 else 21
  lazy val SO_RCVTIMEO = if mac then 0x1006 else 20
  /** macOS only (`ENOPROTOOPT` on Linux, where `MSG_NOSIGNAL` serves instead). */
  val SO_NOSIGPIPE = 0x1022
  val SCM_RIGHTS   = 1
  /** The most descriptors one message can carry on this platform — Linux's `SCM_MAX_FD` is 253; XNU admits
    * 254, the most whose in-kernel form fits its 2048-byte cluster — so a control buffer with room for this
    * many cannot be truncated by a single well-formed `SCM_RIGHTS` message.
    */
  lazy val ScmMaxFd: Int = if mac then 254 else 253
  lazy val MSG_DONTWAIT     = if mac then 0x80 else 0x40
  lazy val MSG_NOSIGNAL     = if mac then 0 else 0x4000
  lazy val MSG_CMSG_CLOEXEC = if mac then 0 else 0x40000000   // no such flag on macOS; fcntl afterwards instead
  lazy val MSG_CTRUNC       = if mac then 0x20 else 0x08
  lazy val MSG_TRUNC        = if mac then 0x10 else 0x20
  val F_GETFD    = 1
  val F_SETFD    = 2
  val F_GETFL    = 3
  val F_SETFL    = 4
  val FD_CLOEXEC = 1
  lazy val O_NONBLOCK = if mac then 0x4 else 0x800
  val POLLIN   = 0x1
  val POLLOUT  = 0x4
  val POLLERR  = 0x8
  val POLLHUP  = 0x10
  val POLLNVAL = 0x20
  /** Any `revents` bit that means a read (or a failing read) will not block. */
  val POLL_READABLE = POLLIN | POLLERR | POLLHUP | POLLNVAL
  val SHUT_RD   = 0
  val SHUT_WR   = 1
  val SHUT_RDWR = 2


  /////////////////////////////////////////////////////////////////////
  // errno: Linux and macOS agree on the first few and almost nothing //
  // else, so every name is resolved by platform and never carried   //
  // across; `name` gives the symbol back for messages.               //
  /////////////////////////////////////////////////////////////////////

  object Errno {
    val EPERM  = 1
    val ENOENT = 2
    val EINTR  = 4
    val EIO    = 5
    val EBADF  = 9
    val ENOMEM = 12
    val EACCES = 13
    val EEXIST = 17
    val EINVAL = 22
    val ENFILE = 23
    val EMFILE = 24
    val ENOSPC = 28
    val EPIPE  = 32
    lazy val EAGAIN       = if mac then 35 else 11
    lazy val EWOULDBLOCK  = EAGAIN
    lazy val ENOTSOCK     = if mac then 38 else 88
    lazy val EDESTADDRREQ = if mac then 39 else 89
    lazy val EMSGSIZE     = if mac then 40 else 90
    lazy val ENOPROTOOPT  = if mac then 42 else 92
    lazy val EADDRINUSE   = if mac then 48 else 98
    lazy val ECONNABORTED = if mac then 53 else 103
    lazy val ECONNRESET   = if mac then 54 else 104
    lazy val ENOBUFS      = if mac then 55 else 105
    lazy val EISCONN      = if mac then 56 else 106
    lazy val ENOTCONN     = if mac then 57 else 107
    lazy val ETOOMANYREFS = if mac then 59 else 109
    lazy val ETIMEDOUT    = if mac then 60 else 110
    lazy val ECONNREFUSED = if mac then 61 else 111

    private lazy val names: Map[Int, String] = Map(
      EPERM -> "EPERM", ENOENT -> "ENOENT", EINTR -> "EINTR", EIO -> "EIO", EBADF -> "EBADF", ENOMEM -> "ENOMEM",
      EACCES -> "EACCES", EEXIST -> "EEXIST", EINVAL -> "EINVAL", ENFILE -> "ENFILE", EMFILE -> "EMFILE",
      ENOSPC -> "ENOSPC", EPIPE -> "EPIPE", EAGAIN -> "EAGAIN", ENOTSOCK -> "ENOTSOCK", EDESTADDRREQ -> "EDESTADDRREQ",
      EMSGSIZE -> "EMSGSIZE", ENOPROTOOPT -> "ENOPROTOOPT", EADDRINUSE -> "EADDRINUSE", ECONNABORTED -> "ECONNABORTED",
      ECONNRESET -> "ECONNRESET", ENOBUFS -> "ENOBUFS", EISCONN -> "EISCONN", ENOTCONN -> "ENOTCONN",
      ETOOMANYREFS -> "ETOOMANYREFS", ETIMEDOUT -> "ETIMEDOUT", ECONNREFUSED -> "ECONNREFUSED"
    )

    /** The symbolic name of a code on this platform, or `errno N` for one not in the table. */
    def name(e: Int): String = names.getOrElse(e, s"errno $e")

    /** The call would block (a non-blocking socket, or a timeout on a blocking one). */
    def wouldBlock(e: Int): Boolean = e == EAGAIN || e == EWOULDBLOCK
    /** A signal interrupted the call; retrying is the usual answer. */
    def interrupted(e: Int): Boolean = e == EINTR
    /** Nothing can be sent right now: would-block, or the kernel is out of buffers or descriptor references. */
    def backpressure(e: Int): Boolean = wouldBlock(e) || e == ENOBUFS || e == ETOOMANYREFS
    /** The other end is gone or was never there. */
    def peerGone(e: Int): Boolean =
      e == EPIPE || e == ECONNREFUSED || e == ECONNRESET || e == ECONNABORTED || e == EDESTADDRREQ || e == ENOTCONN
  }


  //////////////////////////////////////////////////////////////////
  // The house form for a primitive answer or a small code: a Long //
  // that is a count when non-negative and -errno when negative.   //
  //////////////////////////////////////////////////////////////////

  /** A system call's answer: a non-negative count (bytes, descriptors, ready entries) or `-errno`.
    * Nothing is allocated; classify with the accessors, or turn it into an `Ask` with [[Result.ask]].
    */
  opaque type Result = Long
  object Result {
    inline def success(n: Long): Result = n
    inline def failure(e: Int): Result = -(e.toLong)
    /** The C convention made explicit: a negative raw return becomes `-errno` read from `cap`. */
    inline def of(raw: Long, cap: MemorySegment): Result = if raw < 0 then -(errnoOf(cap).toLong) else raw
    inline def of(raw: Int, cap: MemorySegment): Result = if raw < 0 then -(errnoOf(cap).toLong) else raw.toLong

    extension (r: Result) {
      /** The bare value: count, or `-errno`. */
      inline def raw: Long = r
      inline def ok: Boolean = r >= 0
      inline def failed: Boolean = r < 0
      /** The count if the call succeeded, else 0. */
      inline def count: Long = if r >= 0 then r else 0L
      /** The code if the call failed, else 0. */
      inline def errno: Int = if r < 0 then (-r).toInt else 0
      inline def wouldBlock: Boolean = r < 0 && Errno.wouldBlock((-r).toInt)
      inline def interrupted: Boolean = r < 0 && Errno.interrupted((-r).toInt)
      inline def backpressure: Boolean = r < 0 && Errno.backpressure((-r).toInt)
      inline def peerGone: Boolean = r < 0 && Errno.peerGone((-r).toInt)
      /** The errno's name, or the count as text. */
      def name: String = if r < 0 then Errno.name((-r).toInt) else r.toString
      /** The count, or an `Err` carrying a [[Failed]] that names `call`. */
      def ask(call: String): Ask[Long] = if r >= 0 then Is(r) else Err.or(Failed(call, (-r).toInt))
      def ask(call: String, detail: String): Ask[Long] = if r >= 0 then Is(r) else Err.or(Failed(call, (-r).toInt, detail))
    }
  }

  /** A system call failure as an error with its code intact: `call` is the libc function, `errno` the code
    * (see [[Errno.name]]), `detail` whatever the caller adds (a path, a descriptor).  Renders as
    * `connect failed: ECONNREFUSED (at /tmp/x.sock)`.
    */
  final class Failed(val call: String, val errno: Int, val detail: String = "") extends ErrType {
    type E = Int
    def error: Int = errno
    override def toString: String =
      if detail.isEmpty then s"$call failed: ${Errno.name(errno)}" else s"$call failed: ${Errno.name(errno)} ($detail)"
    def buildLines(sb: MkStr, prefix: String): Unit = ErrType.buildLinesFromString(sb, toString, prefix)
    def toThrowable: Throwable = new java.io.IOException(toString)
    override def equals(a: Any): Boolean = a match
      case f: Failed => f.call == call && f.errno == errno && f.detail == detail
      case _ => false
    override def hashCode: Int = (call.hashCode * 31 + errno) * 31 + detail.hashCode
  }

  /** The errno inside an `Err`, digging through explanations; -1 if the error is not a system call failure. */
  def errnoOf(err: Err): Int = err.underlying match
    case f: Failed => f.errno
    case x: ErrType.Explained => errnoOf(x.error)
    case _ => -1


  ///////////////////////////////////////////////////////////////
  // Struct layouts: 64-bit Linux and macOS, offsets in bytes  //
  ///////////////////////////////////////////////////////////////

  /** `struct sockaddr_un`: Linux `{ u16 family; char path[108] }`; macOS `{ u8 len; u8 family; char path[104] }`. */
  object SockAddrUn {
    lazy val size: Int = if mac then 106 else 110
    val pathOff = 2L
    /** Room for a path plus its terminating NUL. */
    lazy val pathMax: Int = if mac then 103 else 107
    /** Fills a zeroed `size`-byte segment for `AF_UNIX` at `path` (already checked against `pathMax`). */
    def write(seg: MemorySegment, path: Array[Byte]): Unit =
      if mac then
        seg.set(JAVA_BYTE, 0L, size.toByte)
        seg.set(JAVA_BYTE, 1L, AF_UNIX.toByte)
      else seg.set(JAVA_SHORT, 0L, AF_UNIX.toShort)
      MemorySegment.copy(path, 0, seg, JAVA_BYTE, pathOff, path.length)
  }

  /** `struct iovec { void* base; size_t len }`. */
  object IoVec {
    val size = 16L
    def set(seg: MemorySegment, base: MemorySegment, len: Long): Unit =
      seg.set(ADDRESS, 0L, base)
      seg.set(JAVA_LONG, 8L, len)
  }

  /** `struct msghdr`: the same fields, different widths and padding (Linux 56 bytes, macOS 48). */
  object MsgHdr {
    lazy val size: Long = if mac then 48L else 56L
    val iov        = 16L
    val iovLen     = 24L   // size_t on Linux, int on macOS
    val control    = 32L
    val controlLen = 40L   // size_t on Linux, socklen_t on macOS
    lazy val flags: Long = if mac then 44L else 48L
    /** Zeroes `seg` and points it at one iovec and a control buffer of `ctrlLen` bytes (0 for none). */
    def init(seg: MemorySegment, iovec: MemorySegment, ctrl: MemorySegment, ctrlLen: Long): Unit =
      seg.fill(0)
      seg.set(ADDRESS, iov, iovec)
      if mac then seg.set(JAVA_INT, iovLen, 1) else seg.set(JAVA_LONG, iovLen, 1L)
      if ctrlLen > 0 then
        seg.set(ADDRESS, control, ctrl)
        if mac then seg.set(JAVA_INT, controlLen, ctrlLen.toInt) else seg.set(JAVA_LONG, controlLen, ctrlLen)
    /** The control length the kernel reports after `recvmsg`. */
    def controlLength(seg: MemorySegment): Long =
      if mac then seg.get(JAVA_INT, controlLen).toLong else seg.get(JAVA_LONG, controlLen)
    def flagsOf(seg: MemorySegment): Int = seg.get(JAVA_INT, flags)
  }

  /** `struct cmsghdr`: Linux `{ u64 len; i32 level; i32 type }` 8-aligned; macOS `{ u32 len; i32 level; i32 type }` 4-aligned. */
  object CMsg {
    lazy val hdr: Long      = if mac then 12L else 16L
    lazy val align: Long    = if mac then 4L else 8L
    lazy val levelOff: Long = if mac then 4L else 8L
    lazy val typeOff: Long  = if mac then 8L else 12L
    def alignUp(n: Long): Long = (n + align - 1) & ~(align - 1)
    /** Bytes a control buffer needs for one message carrying `data` bytes (`CMSG_SPACE`, padded). */
    def space(data: Long): Long = hdr + alignUp(data)
    /** Bytes one message carrying `data` bytes occupies (`CMSG_LEN`, unpadded).  Offered as `msg_controllen`
      * on a receive, it admits exactly `data` bytes of payload: the padding of `space` could otherwise hold
      * an extra descriptor the caller never asked for.
      */
    def len(data: Long): Long = hdr + data
    /** Writes one `SCM_RIGHTS` message carrying `passFd` at the start of `ctrl`; answers its `CMSG_SPACE`. */
    def writeFd(ctrl: MemorySegment, passFd: Int): Long =
      if mac then ctrl.set(JAVA_INT, 0L, (hdr + 4L).toInt) else ctrl.set(JAVA_LONG, 0L, hdr + 4L)
      ctrl.set(JAVA_INT, levelOff, SOL_SOCKET)
      ctrl.set(JAVA_INT, typeOff, SCM_RIGHTS)
      ctrl.set(JAVA_INT, hdr, passFd)
      space(4L)
    /** Writes one `SCM_RIGHTS` message carrying all of `fds` at the start of `ctrl`; answers its `CMSG_SPACE`. */
    def writeFds(ctrl: MemorySegment, fds: Array[Int]): Long =
      val data = 4L * fds.length
      if mac then ctrl.set(JAVA_INT, 0L, (hdr + data).toInt) else ctrl.set(JAVA_LONG, 0L, hdr + data)
      ctrl.set(JAVA_INT, levelOff, SOL_SOCKET)
      ctrl.set(JAVA_INT, typeOff, SCM_RIGHTS)
      var i = 0
      while i < fds.length do
        ctrl.set(JAVA_INT, hdr + 4L * i, fds(i))
        i += 1
      space(data)
    /** Visits every descriptor in the `SCM_RIGHTS` messages of a `clen`-byte control buffer, in order.
      * A message whose declared length runs past `clen` is visited as far as the buffer goes and ends the
      * walk: on a truncated receive (`MSG_CTRUNC`) Darwin installs the descriptors and then cuts the copied
      * control bytes without shortening `cmsg_len`, so every descriptor whose number did fit is live and
      * must be seen, or it leaks.  A header shorter than a header is malformed and stops the walk.
      */
    inline def forEachFd(ctrl: MemorySegment, clen: Long)(inline f: Int => Unit): Unit =
      var off = 0L
      while off + hdr <= clen do
        val len = if mac then ctrl.get(JAVA_INT, off).toLong else ctrl.get(JAVA_LONG, off)
        if len < hdr then off = clen
        else
          val end = if off + len > clen then clen else off + len
          if ctrl.get(JAVA_INT, off + levelOff) == SOL_SOCKET && ctrl.get(JAVA_INT, off + typeOff) == SCM_RIGHTS then
            var d = off + hdr
            while d + 4 <= end do
              f(ctrl.get(JAVA_INT, d))
              d += 4
          off = if end == clen then clen else off + alignUp(len)
  }

  /** `struct pollfd { int fd; short events; short revents }`, the same on both platforms. */
  object PollFd {
    val size = 8L
    def set(seg: MemorySegment, i: Int, fd: Int, events: Int): Unit =
      seg.set(JAVA_INT, i * size, fd)
      seg.set(JAVA_SHORT, i * size + 4, events.toShort)
      seg.set(JAVA_SHORT, i * size + 6, 0.toShort)
    def clearRevents(seg: MemorySegment, i: Int): Unit = seg.set(JAVA_SHORT, i * size + 6, 0.toShort)
    def revents(seg: MemorySegment, i: Int): Int = seg.get(JAVA_SHORT, i * size + 6) & 0xFFFF
  }

  /** `struct timeval`: `{ i64 sec; i64 usec }` on Linux, `{ i64 sec; i32 usec }` on macOS; 16 bytes either way. */
  object TimeVal {
    val size = 16L
    def write(seg: MemorySegment, millis: Long): Unit =
      seg.set(JAVA_LONG, 0L, millis / 1000)
      if mac then seg.set(JAVA_INT, 8L, ((millis % 1000) * 1000).toInt)
      else seg.set(JAVA_LONG, 8L, (millis % 1000) * 1000)
  }


  ////////////////////////////////////////////////////////////////////
  // Downcalls.  Calls that can fail capture errno into a segment of //
  // `captureLayout` passed as their first argument.                 //
  ////////////////////////////////////////////////////////////////////

  /** The layout of an errno-capture segment; allocate one per thread that makes calls (see [[capture]]). */
  lazy val captureLayout: java.lang.foreign.StructLayout = Linker.Option.captureStateLayout()
  private lazy val errnoVH = captureLayout.varHandle(MemoryLayout.PathElement.groupElement("errno"))

  /** A fresh errno-capture segment in `arena`. */
  def capture(arena: Arena): MemorySegment = arena.allocate(captureLayout)

  /** The errno the last capturing call left in `cap`. */
  def errnoOf(cap: MemorySegment): Int = errnoVH.get(cap, 0L)

  /** The libc entry points, linked on first use.  `nfds_t` is `unsigned long` on Linux and `unsigned int`
    * on macOS, so `poll` is bound per platform; call it through [[Poller]] rather than directly.
    */
  object Sys {
    private val linker = Linker.nativeLinker()
    private val look   = linker.defaultLookup()
    private def dl(sym: String, fd: FunctionDescriptor, opts: Linker.Option*): MethodHandle =
      linker.downcallHandle(look.find(sym).orElseThrow(() => new UnsatisfiedLinkError(s"missing native symbol: $sym")), fd, opts*)
    private val captureErr = Linker.Option.captureCallState("errno")
    val socket     = dl("socket",     FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT), captureErr)
    val socketpair = dl("socketpair", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT, ADDRESS), captureErr)
    val bind       = dl("bind",       FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val listen     = dl("listen",     FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT), captureErr)
    val accept     = dl("accept",     FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS, ADDRESS), captureErr)
    val connect    = dl("connect",    FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val sendmsg    = dl("sendmsg",    FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val recvmsg    = dl("recvmsg",    FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val send       = dl("send",       FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_LONG, JAVA_INT), captureErr)
    val recv       = dl("recv",       FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_LONG, JAVA_INT), captureErr)
    val read       = dl("read",       FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_LONG), captureErr)
    val write      = dl("write",      FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_LONG), captureErr)
    val setsockopt = dl("setsockopt", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val getsockopt = dl("getsockopt", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT, ADDRESS, ADDRESS), captureErr)
    val fcntl      = dl("fcntl",      FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT), Linker.Option.firstVariadicArg(2), captureErr)
    val shutdown   = dl("shutdown",   FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT), captureErr)
    val close      = dl("close",      FunctionDescriptor.of(JAVA_INT, JAVA_INT))
    val poll       =
      if mac then dl("poll", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT), captureErr)
      else        dl("poll", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_LONG, JAVA_INT), captureErr)
  }


  ///////////////////////////////////////////////////////////////////
  // One-call wrappers on a descriptor.  Each takes the caller's    //
  // capture segment and answers a Result; nothing loops or blocks. //
  ///////////////////////////////////////////////////////////////////

  /** Closes `fd` (if non-negative), ignoring any error. */
  def closeQuietly(fd: Int): Unit = if fd >= 0 then (Sys.close.invoke(fd): Int) __ Unit

  /** Marks `fd` close-on-exec. */
  def cloexec(cap: MemorySegment, fd: Int): Result =
    Result.of((Sys.fcntl.invoke(cap, fd, F_SETFD, FD_CLOEXEC): Int), cap)

  /** Sets or clears `O_NONBLOCK` on `fd`. */
  def nonblocking(cap: MemorySegment, fd: Int, on: Boolean): Result =
    val fl: Int = Sys.fcntl.invoke(cap, fd, F_GETFL, 0)
    if fl < 0 then Result.of(fl, cap)
    else Result.of((Sys.fcntl.invoke(cap, fd, F_SETFL, if on then fl | O_NONBLOCK else fl & ~O_NONBLOCK): Int), cap)

  /** Whether `O_NONBLOCK` is set on `fd`: a count of 1 or 0. */
  def isNonblocking(cap: MemorySegment, fd: Int): Result =
    val fl: Int = Sys.fcntl.invoke(cap, fd, F_GETFL, 0)
    if fl < 0 then Result.of(fl, cap) else Result.success(if (fl & O_NONBLOCK) != 0 then 1L else 0L)

  /** Reads an `int`-valued `SOL_SOCKET` option; the count is its value. */
  def getOptInt(cap: MemorySegment, tmp: Arena, fd: Int, opt: Int): Result =
    val v = tmp.allocate(4L)
    val len = tmp.allocate(4L)
    len.set(JAVA_INT, 0L, 4)
    val r: Int = Sys.getsockopt.invoke(cap, fd, SOL_SOCKET, opt, v, len)
    if r != 0 then Result.of(r, cap) else Result.success(v.get(JAVA_INT, 0L).toLong)

  /** Sets an `int`-valued `SOL_SOCKET` option. */
  def setOptInt(cap: MemorySegment, tmp: Arena, fd: Int, opt: Int, value: Int): Result =
    val v = tmp.allocate(4L)
    v.set(JAVA_INT, 0L, value)
    Result.of((Sys.setsockopt.invoke(cap, fd, SOL_SOCKET, opt, v, 4): Int), cap)

  /** One `poll` over the `n` `PollFd` entries in `fds` for up to `timeoutMs` (negative: no limit); the count
    * is how many entries have an event, 0 on timeout.  `EINTR` is reported, not retried.
    */
  def poll(cap: MemorySegment, fds: MemorySegment, n: Int, timeoutMs: Int): Result =
    val r: Int = if mac then Sys.poll.invoke(cap, fds, n, timeoutMs) else Sys.poll.invoke(cap, fds, n.toLong, timeoutMs)
    Result.of(r, cap)

  /** Stops traffic on `fd` in the given direction(s); a blocked peer sees the socket gone. */
  def shutdown(cap: MemorySegment, fd: Int, how: Int): Result =
    Result.of((Sys.shutdown.invoke(cap, fd, how): Int), cap)

  /** A connected `AF_UNIX` pair of the given type, both ends close-on-exec. */
  def pair(sockType: Int): Ask[(Int, Int)] =
    if !supported then Err.or(s"AF_UNIX socket pairs are unsupported on '$osName'")
    else Ask:
      Resource.assemble:
        val tmp = Resource.whileAssembling(Arena.ofConfined())(_.close())
        val cap = capture(tmp)
        val sv = tmp.allocate(8L)
        if (Sys.socketpair.invoke(cap, AF_UNIX, sockType, 0, sv): Int) != 0 then Failed("socketpair", errnoOf(cap)).?
        val a = sv.get(JAVA_INT, 0L).onFailure(closeQuietly)
        val b = sv.get(JAVA_INT, 4L).onFailure(closeQuietly)
        val ca: Long = cloexec(cap, a)   // Result is transparent here: negative means -errno
        val cb: Long = if ca >= 0 then cloexec(cap, b) else ca
        if cb < 0 then Failed("fcntl(F_SETFD)", (-cb).toInt, "on a new socket pair").?
        (a, b)


  /** A reusable `poll` over a fixed number of descriptors.  Set each slot once (or whenever the
    * descriptor changes), then `poll` repeatedly; `revents` and `readable` read the last answer.
    * Use from one thread at a time.
    */
  final class Poller(val slots: Int) extends AutoCloseable {
    private val arena = Arena.ofShared()
    private val cap = capture(arena)
    private val fds = arena.allocate(PollFd.size * slots)

    def set(i: Int, fd: Int, events: Int = POLLIN): Unit = PollFd.set(fds, i, fd, events)

    /** Waits up to `timeoutMs` (negative: without limit) for an event; the count is how many slots have
      * one.  A count of 0 is a timeout; `EINTR` is reported as such, not retried.
      */
    def poll(timeoutMs: Int): Result =
      var i = 0
      while i < slots do
        PollFd.clearRevents(fds, i)
        i += 1
      PosixSocket.poll(cap, fds, slots, timeoutMs)

    def revents(i: Int): Int = PollFd.revents(fds, i)
    /** A read on slot `i` will not block (data, hang-up, error, or an invalid descriptor). */
    def readable(i: Int): Boolean = (PollFd.revents(fds, i) & POLL_READABLE) != 0
    def writable(i: Int): Boolean = (PollFd.revents(fds, i) & POLLOUT) != 0

    def close(): Unit = arena.close()
  }
}
