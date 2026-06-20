// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.eio


import java.lang.foreign.{Arena, MemorySegment, Linker, FunctionDescriptor, SymbolLookup}
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_INT, JAVA_LONG}
import java.lang.invoke.MethodHandle
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files, StandardOpenOption}

import kse.basics.{given, _}
import kse.flow.{given, _}


/** RAM-backed off-heap memory shared between processes (or threads) by mapping the same path read-write.
  *
  * For high-performance sharing the backing must never hit disk: the region lives on a RAM filesystem
  * (tmpfs), so pages stay in memory and only ever reach disk if the OS swaps.  There is no portable JDK
  * way to guarantee this — it relies on a tmpfs mount (Linux `/dev/shm`); on platforms without one,
  * [[create]] fails rather than silently spilling to disk.
  *
  * [[attach]] is the cross-platform read side: it views an already-existing OS-named object (created by any
  * process, in any language) — POSIX `shm_open` names on Linux/macOS, `CreateFileMapping` names on Windows —
  * via native `shm_open`/`mmap` (macOS) and `OpenFileMapping`/`MapViewOfFile` (Windows), and a plain
  * `FileChannel.map` of `<posixShmDir>/name` on Linux.  [[createNamed]] is the matching write side: it
  * creates a fresh, randomly-named, RAM-resident object and returns the owning `Tidy.Later`, whose cleanup
  * (at `close`, JVM shutdown, or GC) unmaps and destroys it.
  *
  * The native paths (everything except the Linux `FileChannel` route) call restricted FFM methods, so the
  * JVM must be started with `--enable-native-access=ALL-UNNAMED` (or the owning module); without it macOS and
  * Windows `attach`/`createNamed` fail.  The Linux file route needs no such flag.
  *
  * The backing file is the shared medium, so its naming, lifetime, and cleanup are the caller's job
  * (a tmpfs file holds RAM until deleted).  A region is sized to exactly `n * bytesOf[A]` bytes, so its
  * length is self-describing — another party need only learn the `path` (and agree on the element type)
  * and can attach with `path.openIOMem[A]()`, recovering the count from the file length.
  */
object SharedMemory {
  /** A writable RAM-filesystem (tmpfs) directory where mappings stay off disk, if one can be found.
    * Checked in order: `-Dkse.eio.shmdir`, then `/dev/shm`, `/run/shm`.  `None` on macOS/Windows.
    */
  lazy val ramDirectory: Option[Path] =
    val candidates =
      Option(System.getProperty("kse.eio.shmdir")).map(Path.of(_)).toList :::
      List(Path.of("/dev/shm"), Path.of("/run/shm"))
    candidates.find(p => Files.isDirectory(p) && Files.isWritable(p))
  /** A shared region: the owned mapping plus the `path` others can attach to.  `close` unmaps and unlinks
    * the backing file — POSIX semantics, so a mapping already open elsewhere stays valid until it too
    * closes, while the name disappears (no new attachers) and the memory is reclaimed once all unmap.
    */
  final class Region[A <: Mem.Type] private[SharedMemory] (val path: Path, val owned: Mem.Owned[A]) extends AutoCloseable {
    inline def memory: Mem[A] = owned.memory
    inline def op[B](inline f: Mem[A] => B): B = owned.op(f)
    inline def use(inline f: Mem[A] => Unit): Unit = owned.use(f)
    def close(): Unit =
      try owned.close()
      finally Files.deleteIfExists(path) __ Unit
  }

  /** Map `p` (created if absent) read-write shared, sized to exactly `bytes`, in a fresh shared `Arena`. */
  def mapShared[A <: Mem.Type](p: Path, bytes: Long): Region[A] =
    val arena = Arena.ofShared()
    try
      val owned = Mem.Owned.create[A](arena){ a =>
        val ch = FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE)
        try
          if bytes < ch.size then ch.truncate(bytes) __ Unit
          ch.map(FileChannel.MapMode.READ_WRITE, 0L, bytes, a)
        finally ch.close()
      }
      new Region[A](p, owned)
    catch
      case e if e.catchable =>
        arena.close()
        throw e

  /** Create a fresh file and share `n` items of `A` through it; the region's `path` is the new file.
    * Prefers a RAM filesystem (see [[ramDirectory]]).  With `allowBackingFile = false` (default) this
    * fails when no tmpfs is available rather than touching disk; set it `true` to permit a disk-backed
    * temp file as a cross-platform fallback (NOT RAM-resident — such a mapping spills to disk).
    */
  inline def create[A <: Mem.Type](n: Long, allowBackingFile: Boolean = false)(using Tidy.Nice[Region[A]]): Ask[Region[A]] =
    Ask:
      val file = ramDirectory match
        case Some(dir) => Files.createTempFile(dir, "kse-shm-", ".mem")
        case None =>
          if allowBackingFile then Files.createTempFile("kse-shm-", ".mem")
          else Err ?# "No RAM-backed (tmpfs) directory found; pass allowBackingFile = true to fall back to a disk-backed temp file"
      mapShared[A](file, n * Mem.bytesOf[A])

  /** Share `n` items of `A` through `p` (created if absent), a path you or another party chose.  For RAM
    * residence, `p` must live on a tmpfs mount (e.g. under [[ramDirectory]]); that is the caller's to ensure.
    */
  inline def createFrom[A <: Mem.Type](p: Path, n: Long)(using Tidy.Nice[Region[A]]): Ask[Region[A]] =
    Ask:
      mapShared[A](p, n * Mem.bytesOf[A])


  //////////////////////////////////////////////////////////////////////
  /// Attaching to an existing OS-named shared-memory object (read side) ///
  //////////////////////////////////////////////////////////////////////

  private lazy val osName    = System.getProperty("os.name", "").toLowerCase
  private lazy val onWindows = osName.contains("win")
  private lazy val onMac     = osName.contains("mac") || osName.contains("darwin")
  private lazy val onLinux   = osName.contains("nux")

  /** On Linux, the directory where POSIX `shm_open` objects appear as files and where [[attach]] resolves a
    * name; `-Dkse.eio.shmdir` overrides it.  Meaningful only where such a mount exists (Linux `/dev/shm`).
    */
  lazy val posixShmDir: Path =
    Option(System.getProperty("kse.eio.shmdir")).map(Path.of(_)).getOrElse(Path.of("/dev/shm"))

  /** POSIX `shm_open` names must begin with a single leading `/`. */
  private def posixName(name: String): String = if name.startsWith("/") then name else "/" + name

  /** Attach to a named shared-memory object created elsewhere (another process, possibly another language)
    * and view its first `n` elements of type `A` as a `Mem.Owned`.  We do not own the object's name or
    * lifetime, so `close` only unmaps our view — it never unlinks the object.
    *
    * The element count `n` is supplied rather than discovered: it must be agreed out-of-band anyway, and
    * taking it lets us skip platform-specific size queries.  `name` is interpreted in the host OS namespace:
    *  - Linux: a POSIX `shm_open` name (`/foo`), exposed as the file `<posixShmDir>/foo` and mapped directly.
    *  - macOS: a POSIX `shm_open` name (`/foo`), attached via native `shm_open` + `mmap`.
    *  - Windows: a `CreateFileMapping` object name (optionally `Local\`/`Global\`-qualified), opened via
    *    native `OpenFileMapping` + `MapViewOfFile`.
    */
  inline def attach[A <: Mem.Type](name: String, n: Long, readOnly: Boolean = false)(using Tidy.Nice[Mem.Owned[A]]): Ask[Mem.Owned[A]] =
    Ask:
      attachBytes[A](name, n * Mem.bytesOf[A], readOnly)

  /** Worker for [[attach]]: map `bytes` bytes of the named object, dispatched by host OS.  Throws on failure
    * (callers wrap it in `Ask`); the returned `Mem.Owned` unmaps but never unlinks on `close`.
    */
  def attachBytes[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    if bytes <= 0 then throw new IllegalArgumentException(s"shared-memory size must be positive, got $bytes bytes")
    if onLinux then attachPosixFile[A](name, bytes, readOnly)
    else if onMac then attachPosixNative[A](name, bytes, readOnly)
    else if onWindows then attachWindows[A](name, bytes, readOnly)
    else throw new UnsupportedOperationException(s"shared-memory attach is unsupported on '$osName'")

  /** Linux: a POSIX object is a file under `posixShmDir`, so a plain `FileChannel.map` attaches it — no
    * native code.  (We do not create or truncate: attaching to something that must already exist.)
    */
  private def attachPosixFile[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    val p = posixShmDir.resolve(name.stripPrefix("/"))
    val arena = Arena.ofShared()
    try
      Mem.Owned.create[A](arena){ a =>
        val ch =
          if readOnly then FileChannel.open(p, StandardOpenOption.READ)
          else FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE)
        try ch.map(if readOnly then FileChannel.MapMode.READ_ONLY else FileChannel.MapMode.READ_WRITE, 0L, bytes, a)
        finally ch.close()
      }
    catch
      case e if e.catchable =>
        arena.close()
        throw e

  /** Native `libSystem` bindings for POSIX shared memory on macOS, where `shm_open` objects are *not* files
    * and so cannot be reached through a `FileChannel`.  Initialized lazily on first macOS use only.
    */
  private object PosixNative {
    private val linker = Linker.nativeLinker()
    private val look   = linker.defaultLookup()
    private def bind(sym: String, fd: FunctionDescriptor, opts: Linker.Option*): MethodHandle =
      linker.downcallHandle(look.find(sym).orElseThrow(() => new UnsatisfiedLinkError(s"missing native symbol: $sym")), fd, opts*)
    // Acquisition calls capture `errno` (prepended capture-segment arg) so failures carry a code and a name
    // clash (EEXIST) is distinguishable from a real error; cleanup calls stay plain (their errors are ignored).
    private val captureErr = Linker.Option.captureCallState("errno")
    val captureLayout = Linker.Option.captureStateLayout()
    val errnoVH       = captureLayout.varHandle(java.lang.foreign.MemoryLayout.PathElement.groupElement("errno"))
    val shmOpen   = bind("shm_open",   FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT), captureErr)
    val mmap      = bind("mmap",       FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_LONG, JAVA_INT, JAVA_INT, JAVA_INT, JAVA_LONG), captureErr)
    val ftruncate = bind("ftruncate",  FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_LONG), captureErr)
    val munmap    = bind("munmap",     FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_LONG))
    val close     = bind("close",      FunctionDescriptor.of(JAVA_INT, JAVA_INT))
    val shmUnlink = bind("shm_unlink", FunctionDescriptor.of(JAVA_INT, ADDRESS))
  }

  private def attachPosixNative[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    val arena = Arena.ofShared()
    try
      Mem.Owned.create[A](arena){ a =>
        val tmp = Arena.ofConfined()
        try
          val cap = tmp.allocate(PosixNative.captureLayout)
          val fd: Int = PosixNative.shmOpen.invoke(cap, tmp.allocateFrom(posixName(name)), if readOnly then 0 else 0x2, 0)  // O_RDONLY / O_RDWR
          if fd < 0 then throw new java.io.IOException(s"shm_open failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
          try
            val prot = if readOnly then 0x1 else 0x3   // PROT_READ [| PROT_WRITE]
            val view: MemorySegment = PosixNative.mmap.invoke(cap, MemorySegment.NULL, bytes, prot, 0x1, fd, 0L)  // MAP_SHARED
            if view.address() == -1L then throw new java.io.IOException(s"mmap failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")  // MAP_FAILED
            view.reinterpret(bytes, a, s => (PosixNative.munmap.invoke(s, bytes): Int) __ Unit)
          finally (PosixNative.close.invoke(fd): Int) __ Unit
        finally tmp.close()
      }
    catch
      case e if e.catchable =>
        arena.close()
        throw e

  /** Native `kernel32` bindings for Windows named file mappings.  Initialized lazily on first Windows use. */
  private object WindowsNative {
    private val arena  = Arena.ofShared()   // keeps the kernel32 lookup alive for the JVM's lifetime
    private val linker = Linker.nativeLinker()
    private val k32    = SymbolLookup.libraryLookup("kernel32", arena)
    private def bind(sym: String, fd: FunctionDescriptor, opts: Linker.Option*): MethodHandle =
      linker.downcallHandle(k32.find(sym).orElseThrow(() => new UnsatisfiedLinkError(s"missing native symbol: $sym")), fd, opts*)
    // Acquisition calls capture GetLastError (prepended capture-segment arg): for create it is what makes it
    // truly exclusive (a valid handle + ERROR_ALREADY_EXISTS = clash), and it gives real failure codes throughout.
    private val captureErr = Linker.Option.captureCallState("GetLastError")
    val captureLayout = Linker.Option.captureStateLayout()
    val lastErrorVH   = captureLayout.varHandle(java.lang.foreign.MemoryLayout.PathElement.groupElement("GetLastError"))
    val openMapping   = bind("OpenFileMappingW",   FunctionDescriptor.of(ADDRESS, JAVA_INT, JAVA_INT, ADDRESS), captureErr)
    val createMapping = bind("CreateFileMappingW", FunctionDescriptor.of(ADDRESS, ADDRESS, ADDRESS, JAVA_INT, JAVA_INT, JAVA_INT, ADDRESS), captureErr)
    val mapView       = bind("MapViewOfFile",      FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_INT, JAVA_INT, JAVA_INT, JAVA_LONG), captureErr)
    val unmapView     = bind("UnmapViewOfFile",    FunctionDescriptor.of(JAVA_INT, ADDRESS))
    val closeHandle   = bind("CloseHandle",        FunctionDescriptor.of(JAVA_INT, ADDRESS))
  }

  private def attachWindows[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    val access = if readOnly then 0x0004 else 0x0004 | 0x0002   // FILE_MAP_READ [| FILE_MAP_WRITE]
    val arena = Arena.ofShared()
    try
      Mem.Owned.create[A](arena){ a =>
        val tmp = Arena.ofConfined()
        try
          val cap = tmp.allocate(WindowsNative.captureLayout)
          val handle: MemorySegment = WindowsNative.openMapping.invoke(cap, access, 0, tmp.allocateFrom(name, StandardCharsets.UTF_16LE))
          if handle.address() == 0L then throw new java.io.IOException(s"OpenFileMapping failed for '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
          try
            val view: MemorySegment = WindowsNative.mapView.invoke(cap, handle, access, 0, 0, bytes)
            if view.address() == 0L then throw new java.io.IOException(s"MapViewOfFile failed for '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
            view.reinterpret(bytes, a, s => (WindowsNative.unmapView.invoke(s): Int) __ Unit)
          finally (WindowsNative.closeHandle.invoke(handle): Int) __ Unit
        finally tmp.close()
      }
    catch
      case e if e.catchable =>
        arena.close()
        throw e


  ///////////////////////////////////////////////////////////////////////
  /// Creating a fresh RAM-resident OS-named object (write side)        ///
  ///////////////////////////////////////////////////////////////////////

  private val rng = new java.security.SecureRandom()

  /** A fresh, opaque, hard-to-guess name in POSIX form (leading `/`, no other `/`, short enough for macOS
    * `PSHMNAMLEN`).  Forward slash is legal in a Windows mapping name too, so one form serves all platforms. */
  private def freshName(): String =
    val bs = new Array[Byte](15)   // 120 bits
    rng.nextBytes(bs)
    "/kse-" + java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(bs)

  /** An owned, freshly-created named region: the rendezvous `name` plus the mapping.  `close` unmaps and
    * destroys the object (POSIX `shm_unlink` / file delete; Windows section-handle close), so once every
    * holder closes — or the process exits — the memory and the name are gone. */
  final class Created[A <: Mem.Type] private[SharedMemory] (val name: String, owned: Mem.Owned[A], destroy: () => Unit) extends AutoCloseable {
    def memory: Mem[A] = owned.memory
    def op[B](f: Mem[A] => B): B = owned.op(f)
    def use(f: Mem[A] => Unit): Unit = owned.use(f)
    def close(): Unit = try owned.close() finally destroy()
  }

  /** Create a fresh, RAM-resident, OS-named shared-memory object holding `n` items of `A`, and hand back the
    * owning `Tidy.Later` (cleanup at `close`, JVM shutdown, or GC).  The name is random and opaque — read it
    * via `later.op(_.name)` to hand to an attacher.  Unlike [[create]] there is no disk fallback: if the
    * platform cannot provide RAM-resident backing this fails rather than touching disk.
    */
  inline def createNamed[A <: Mem.Type](n: Long): Ask[Tidy.Later[Created[A]]] =
    Ask:
      createdLater[A](n * Mem.bytesOf[A])

  /** Worker for [[createNamed]]: create `bytes` bytes and wrap the result in a backstopped `Tidy.Later`. */
  def createdLater[A <: Mem.Type](bytes: Long): Tidy.Later[Created[A]] =
    if bytes <= 0 then throw new IllegalArgumentException(s"shared-memory size must be positive, got $bytes bytes")
    Resource.closedLater(createBytes[A](bytes))(_.close())

  private def createBytes[A <: Mem.Type](bytes: Long): Created[A] =
    if onLinux then createPosixFile[A](bytes)
    else if onMac then createPosixNative[A](bytes)
    else if onWindows then createWindows[A](bytes)
    else throw new UnsupportedOperationException(s"shared-memory creation is unsupported on '$osName'")

  /** Fail unless `posixShmDir` is a RAM filesystem, so a Linux create never silently spills to disk. */
  private def ensureRam(): Unit =
    val fsType =
      try Files.getFileStore(posixShmDir).`type`()
      catch case e if e.catchable => throw new java.io.IOException(s"no shared-memory directory at $posixShmDir", e)
    if fsType != "tmpfs" && fsType != "ramfs" then
      throw new java.io.IOException(s"$posixShmDir is on '$fsType', not a RAM filesystem; refusing to create disk-backed shared memory")

  /** Draw random names until one is unused.  `once` returns `null` to signal a name clash (retry); a real
    * post-creation failure throws straight through (no retry).  Five clashes means the namespace is borked. */
  private def withRetries[A <: Mem.Type](once: => Created[A]): Created[A] =
    var c: Created[A] = null
    var i = 0
    while (c eq null) && i < 5 do
      c = once
      i += 1
    if c eq null then throw new java.io.IOException("could not create a uniquely-named shared-memory object in 5 attempts")
    c

  private def shmUnlink(name: String): Unit =
    val tmp = Arena.ofConfined()
    try (PosixNative.shmUnlink.invoke(tmp.allocateFrom(name)): Int) __ Unit
    finally tmp.close()

  /** Linux: a POSIX object is a tmpfs file under `posixShmDir`, created exclusively (`CREATE_NEW`).  Destroy
    * = delete the file (= `shm_unlink`); the arena's `close` unmaps. */
  private def createPosixFile[A <: Mem.Type](bytes: Long): Created[A] =
    ensureRam()
    withRetries:
      val name = freshName()
      val p = posixShmDir.resolve(name.stripPrefix("/"))
      val ch =
        try FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE_NEW)
        catch case _: java.nio.file.FileAlreadyExistsException => null
      if ch eq null then null
      else
        val arena = Arena.ofShared()
        try
          val owned = Mem.Owned.create[A](arena){ a =>
            try ch.map(FileChannel.MapMode.READ_WRITE, 0L, bytes, a) finally ch.close()
          }
          new Created[A](name, owned, () => Files.deleteIfExists(p) __ Unit)
        catch
          case e if e.catchable =>
            arena.close()
            Files.deleteIfExists(p) __ Unit
            throw e

  /** macOS: native `shm_open(O_CREAT|O_EXCL|O_RDWR)` + `ftruncate` + `mmap`.  Destroy = `shm_unlink`.  `errno`
    * distinguishes a name clash (`EEXIST` → retry) from a real failure (→ fail loud with the code). */
  private def createPosixNative[A <: Mem.Type](bytes: Long): Created[A] =
    withRetries:
      val name = freshName()
      val tmp = Arena.ofConfined()
      try
        val cap = tmp.allocate(PosixNative.captureLayout)
        val fd: Int = PosixNative.shmOpen.invoke(cap, tmp.allocateFrom(name), 0x0200 | 0x0800 | 0x0002, 0x180)  // O_CREAT|O_EXCL|O_RDWR, 0600
        if fd < 0 then
          val e = (PosixNative.errnoVH.get(cap, 0L): Int)
          if e == 17 then null    // EEXIST: name clash, retry
          else throw new java.io.IOException(s"shm_open failed for '$name' (errno=$e)")
        else
          val arena = Arena.ofShared()
          try
            if (PosixNative.ftruncate.invoke(cap, fd, bytes): Int) != 0 then
              throw new java.io.IOException(s"ftruncate failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
            val owned = Mem.Owned.create[A](arena){ a =>
              val view: MemorySegment = PosixNative.mmap.invoke(cap, MemorySegment.NULL, bytes, 0x3, 0x1, fd, 0L)  // PROT_READ|WRITE, MAP_SHARED
              if view.address() == -1L then throw new java.io.IOException(s"mmap failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
              view.reinterpret(bytes, a, s => (PosixNative.munmap.invoke(s, bytes): Int) __ Unit)
            }
            new Created[A](name, owned, () => shmUnlink(name))
          catch
            case e if e.catchable =>
              arena.close()
              shmUnlink(name)
              throw e
          finally (PosixNative.close.invoke(fd): Int) __ Unit
      finally tmp.close()

  /** Windows: native `CreateFileMappingW(INVALID_HANDLE_VALUE, …)` — a pagefile-backed (RAM) section — then
    * `MapViewOfFile`.  The creator *holds the section handle* for the region's lifetime so the name stays
    * openable by attachers; destroy = `CloseHandle` (after the arena unmaps the view). */
  private def createWindows[A <: Mem.Type](bytes: Long): Created[A] =
    withRetries:
      val name = freshName()
      val tmp = Arena.ofConfined()
      try
        val cap = tmp.allocate(WindowsNative.captureLayout)
        val handle: MemorySegment = WindowsNative.createMapping.invoke(
          cap,                                                     // captured GetLastError (prepended arg)
          MemorySegment.ofAddress(-1L),                            // INVALID_HANDLE_VALUE: pagefile-backed, not a file
          MemorySegment.NULL,                                      // default security
          0x04,                                                    // PAGE_READWRITE
          (bytes >>> 32).toInt, (bytes & 0xFFFFFFFFL).toInt,
          tmp.allocateFrom(name, StandardCharsets.UTF_16LE))
        val lastErr = (WindowsNative.lastErrorVH.get(cap, 0L): Int)
        if handle.address() == 0L then
          throw new java.io.IOException(s"CreateFileMapping failed for '$name' (GetLastError=$lastErr)")   // real failure: fail loud
        else if lastErr == 183 then                                // ERROR_ALREADY_EXISTS: name clash, retry
          (WindowsNative.closeHandle.invoke(handle): Int) __ Unit
          null
        else
          val arena = Arena.ofShared()
          try
            val owned = Mem.Owned.create[A](arena){ a =>
              val view: MemorySegment = WindowsNative.mapView.invoke(cap, handle, 0x0006, 0, 0, bytes)  // FILE_MAP_READ|WRITE
              if view.address() == 0L then throw new java.io.IOException(s"MapViewOfFile failed for '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
              view.reinterpret(bytes, a, s => (WindowsNative.unmapView.invoke(s): Int) __ Unit)
            }
            new Created[A](name, owned, () => (WindowsNative.closeHandle.invoke(handle): Int) __ Unit)
          catch
            case e if e.catchable =>
              arena.close()
              (WindowsNative.closeHandle.invoke(handle): Int) __ Unit
              throw e
      finally tmp.close()
}
