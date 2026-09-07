// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.eio


import java.lang.foreign.{Arena, MemorySegment, Linker, FunctionDescriptor, SymbolLookup}
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_INT, JAVA_LONG}
import java.lang.invoke.MethodHandle
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files, StandardOpenOption}
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.time.Duration

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.maths.{given, _}


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
  * creates a fresh RAM-resident object under a random name, or under a name the caller (or a protocol)
  * chose, and returns the owning `Tidy.Later`, whose cleanup (at `close`, JVM shutdown, or GC) unmaps and
  * destroys it.  Either side can start: a process told only a name attaches with a count of 0 and takes the
  * size from the object itself, so memory is received as easily as it is given.
  *
  * Names and lifetimes differ between the platforms in ways a protocol must know.  A POSIX name is unlinked
  * when its creator closes: the name is gone for newcomers at once, while every existing mapping stays valid
  * and the memory is freed when the last unmaps.  A Windows section has no unlink: the name is findable
  * while any handle to the section is open, and the creator holds one until `close`, so the same rule
  * follows — attach while the creator lives — with the difference that an attacher who has closed its own
  * handle (as [[attach]] does, keeping only the view) does not keep the name alive for a third party.  A
  * discovered size is the object's, not the creator's: exact on Linux, where a tmpfs file has the length it
  * was given, but rounded up to whole pages on macOS and Windows, so a protocol that needs the logical
  * length must carry it.
  *
  * A Windows service and its clients need two things a single-session program does not.  Sessions have
  * separate namespaces, so a name created by a service (session 0) is invisible to a desktop client unless
  * it is `Global\`-qualified; creating a `Global\` name takes `SeCreateGlobalPrivilege`, which services have
  * and desktop programs do not, while opening one takes nothing.  And the default DACL admits only the
  * creating account, as POSIX mode 0600 does.  So the working pattern is: the service creates `Global\<name>`
  * with an [[Access]] that admits its clients, and the clients attach — whichever way the data flows after.
  * The same [[Access]] sets the POSIX mode, so one program serves all three platforms:
  * {{{
  * // the service, at start-up: a namespaced name it chose, open to every local account
  * val name = (if windows then "Global\\" else "/") + "my-svc-" + SharedMemory.freshName().drop(5)
  * Resource.nice(SharedMemory.createNamed[Long](name, 1024, SharedMemory.Access.Everyone))(_.close()){ later =>
  *   later.use(_.use(m => m(0) = 0L))       // element 0 says how many elements are meaningful; write it last
  *   tell(name)                             // over whatever channel the clients already have
  *   serve(later)
  * }
  * // a client, in any session and account, told only the name
  * Resource.nice(SharedMemory.attach[Long](name, 0))(_.close()){ view =>   // sized by the OS: whole pages, not 1024
  *   val used = view.op(_(0))                                              // the meaningful length is the protocol's
  *   ...
  * }
  * }}}
  * `Mem.Atom` gives the atomics such a protocol needs inside the region; the protocol itself — who writes
  * what, and how a reader knows it is complete — is the caller's to state.
  *
  * [[createFd]] / [[attachFd]], with [[FdSock]] sockets to carry the descriptor, are the anonymous
  * (`SCM_RIGHTS`) alternative on Linux and macOS: no name in any namespace, kernel-refcounted lifetime
  * (kill every holder, however rudely, and the memory is reclaimed), and on Linux a seal against shrinking
  * so no peer can truncate the region out from under the rest.  [[offerFd]] serves such a region at a
  * socket path and [[acceptFd]] connects and maps it, one call each.
  *
  * The native paths (everything except the Linux `FileChannel` route) call restricted FFM methods, so the
  * JVM must be started with `--enable-native-access=ALL-UNNAMED` (or the owning module); without it macOS and
  * Windows `attach`/`createNamed` fail.  The Linux file route needs no such flag, but the descriptor routes
  * are native everywhere, so on Linux they — unlike the name routes — need it too.
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
  /** The byte size of `n` elements of `per` bytes each, judged before it is multiplied: a negative count is
    * refused, a zero count is refused unless `discoverable` (where it means "the object's own size" and stays
    * 0), and a product that would not fit a `Long` is refused rather than wrapped — so no overflow can pass
    * for a size, or for the discovery signal.  An `Err`, never a throw, so every entry point answers a bad
    * count the same way.  Public only because the inline entry points call it.
    */
  def bytesFor(n: Long, per: Long, discoverable: Boolean): Ask[Long] =
    if n < 0 || (n == 0 && !discoverable) then
      Err.or(s"element count must be positive${if discoverable then " (or 0 to discover the size)" else ""}, got $n")
    else if n == 0 then Is(0L)
    else Ask:
      n *! per

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
    Resource.assemble:
      val arena = undo(Arena.ofShared())(_.close())
      val ch = scoped(FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE))(_.close())
      if bytes < ch.size then ch.truncate(bytes) __ Unit
      arena.into(a => new Region[A](p, Mem.Owned.create[A](a)(_ => ch.map(FileChannel.MapMode.READ_WRITE, 0L, bytes, a))))

  /** Create a fresh file and share `n` items of `A` through it; the region's `path` is the new file.
    * Prefers a RAM filesystem (see [[ramDirectory]]).  With `allowBackingFile = false` (default) this
    * fails when no tmpfs is available rather than touching disk; set it `true` to permit a disk-backed
    * temp file as a cross-platform fallback (NOT RAM-resident — such a mapping spills to disk).
    */
  inline def create[A <: Mem.Type](n: Long, allowBackingFile: Boolean = false)(using Tidy.Nice[Region[A]]): Ask[Region[A]] =
    Ask:
      val bytes = bytesFor(n, Mem.bytesOf[A], discoverable = false).?
      val file = ramDirectory match
        case Some(dir) => Files.createTempFile(dir, "kse-shm-", ".mem")
        case None =>
          if allowBackingFile then Files.createTempFile("kse-shm-", ".mem")
          else Err ?# "No RAM-backed (tmpfs) directory found; pass allowBackingFile = true to fall back to a disk-backed temp file"
      Resource.assemble:
        undo(file)(f => Files.deleteIfExists(f) __ Unit) __ Unit   // ours, and never shared: nothing to keep on failure
        undo(mapShared[A](file, bytes))(_.close())

  /** Share `n` items of `A` through `p` (created if absent), a path you or another party chose.  For RAM
    * residence, `p` must live on a tmpfs mount (e.g. under [[ramDirectory]]); that is the caller's to ensure.
    */
  inline def createFrom[A <: Mem.Type](p: Path, n: Long)(using Tidy.Nice[Region[A]]): Ask[Region[A]] =
    Ask:
      mapShared[A](p, bytesFor(n, Mem.bytesOf[A], discoverable = false).?)


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

  /** A name the OS would not read as given — empty, or holding a NUL that would cut it short — is refused. */
  private def checkName(name: String): String =
    if name.isEmpty then throw new IllegalArgumentException("a shared-memory name must not be empty")
    if name.indexOf('\u0000') >= 0 then throw new IllegalArgumentException("a shared-memory name must not contain NUL")
    name

  /** Attach to a named shared-memory object created elsewhere (another process, possibly another language)
    * and view its first `n` elements of type `A` as a `Mem.Owned` — or, with `n = 0`, all of it, sized by the
    * object itself (exact on Linux; rounded up to whole pages on macOS and Windows, see the class note).  We
    * do not own the object's name or lifetime, so `close` only unmaps our view — it never unlinks the object.
    *
    * `name` is interpreted in the host OS namespace:
    *  - Linux: a POSIX `shm_open` name (`/foo`), exposed as the file `<posixShmDir>/foo` and mapped directly.
    *  - macOS: a POSIX `shm_open` name (`/foo`), attached via native `shm_open` + `mmap`.
    *  - Windows: a `CreateFileMapping` object name (optionally `Local\`/`Global\`-qualified), opened via
    *    native `OpenFileMapping` + `MapViewOfFile`.
    */
  inline def attach[A <: Mem.Type](name: String, n: Long, readOnly: Boolean = false)(using Tidy.Nice[Mem.Owned[A]]): Ask[Mem.Owned[A]] =
    Ask:
      attachBytes[A](name, bytesFor(n, Mem.bytesOf[A], discoverable = true).?, readOnly)

  /** Worker for [[attach]]: map `bytes` bytes of the named object (0 = all of it, as measured), dispatched by
    * host OS.  Throws on failure (callers wrap it in `Ask`); the returned `Mem.Owned` unmaps but never
    * unlinks on `close`.
    */
  def attachBytes[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    if bytes < 0 then throw new IllegalArgumentException(s"shared-memory size must be positive, or 0 to discover it; got $bytes bytes")
    checkName(name) __ Unit
    if onLinux then attachPosixFile[A](checkedPosixName(name), bytes, readOnly)
    else if onMac then attachPosixNative[A](checkedPosixName(name), bytes, readOnly)
    else if onWindows then attachWindows[A](name, bytes, readOnly)
    else throw new UnsupportedOperationException(s"shared-memory attach is unsupported on '$osName'")

  /** Linux: a POSIX object is a file under `posixShmDir`, so a plain `FileChannel.map` attaches it — no
    * native code.  (We do not create or truncate: attaching to something that must already exist.)
    */
  private def attachPosixFile[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    val p = posixShmDir.resolve(name.stripPrefix("/"))
    Resource.assemble:
      val arena = undo(Arena.ofShared())(_.close())
      val ch = scoped(
        if readOnly then FileChannel.open(p, StandardOpenOption.READ)
        else FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE)
      )(_.close())
      val n = if bytes > 0 then bytes else ch.size()
      if n <= 0 then throw new java.io.IOException(s"shared-memory object '$name' is empty")
      val mode = if readOnly then FileChannel.MapMode.READ_ONLY else FileChannel.MapMode.READ_WRITE
      arena.into(a => Mem.Owned.create[A](a)(_ => ch.map(mode, 0L, n, a)))

  /** Native libc/libSystem bindings for POSIX shared memory: macOS names (where `shm_open` objects are
    * *not* files and so cannot be reached through a `FileChannel`) plus the descriptor-based routes on both
    * Linux and macOS.  Initialized lazily on first native use.
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
    // Darwin declares shm_open(const char*, int, ...): the mode is variadic, and Apple's arm64 ABI passes
    // variadic arguments on the stack, so the binding must say so or the mode is read from the wrong place.
    // glibc's is a plain three-argument function.
    val shmOpen   =
      if onMac then bind("shm_open", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT), Linker.Option.firstVariadicArg(2), captureErr)
      else          bind("shm_open", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT), captureErr)
    val mmap      = bind("mmap",       FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_LONG, JAVA_INT, JAVA_INT, JAVA_INT, JAVA_LONG), captureErr)
    val ftruncate = bind("ftruncate",  FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_LONG), captureErr)
    val munmap    = bind("munmap",     FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_LONG))
    val close     = bind("close",      FunctionDescriptor.of(JAVA_INT, JAVA_INT))
    val shmUnlink = bind("shm_unlink", FunctionDescriptor.of(JAVA_INT, ADDRESS))
    val lseek     = bind("lseek",      FunctionDescriptor.of(JAVA_LONG, JAVA_INT, JAVA_LONG, JAVA_INT), captureErr)
    val fcntl     = bind("fcntl",      FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT), Linker.Option.firstVariadicArg(2), captureErr)
    lazy val memfdCreate = bind("memfd_create", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT), captureErr)   // Linux-only symbol; touch only there
    // Darwin only: the 64-bit-inode fstat is plain `fstat` on arm64 and `fstat$INODE64` on x86_64, and glibc
    // before 2.33 exports neither name (Linux measures with lseek instead).  st_size is byte 96 of that struct.
    lazy val fstat =
      val sym = look.find("fstat$INODE64").or(() => look.find("fstat")).orElseThrow(() => new UnsatisfiedLinkError("missing native symbol: fstat"))
      linker.downcallHandle(sym, FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS), captureErr)
  }

  /** macOS: the size of the object behind `fd` by `fstat` (page-rounded for a `shm_open` object); -1 on
    * failure, with the errno left in `cap`. */
  private def darwinSize(cap: MemorySegment, tmp: Arena, fd: Int): Long =
    val st = tmp.allocate(512L)
    if (PosixNative.fstat.invoke(cap, fd, st): Int) != 0 then -1L else st.get(JAVA_LONG, 96L)

  private def attachPosixNative[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    Resource.assemble:
      val arena = undo(Arena.ofShared())(_.close())
      val tmp = scoped(Arena.ofConfined())(_.close())
      val cap = tmp.allocate(PosixNative.captureLayout)
      val fd: Int = PosixNative.shmOpen.invoke(cap, tmp.allocateFrom(posixName(name)), if readOnly then 0 else 0x2, 0)  // O_RDONLY / O_RDWR
      if fd < 0 then throw new java.io.IOException(s"shm_open failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
      scoped(fd)(PosixSocket.closeQuietly) __ Unit   // the mapping keeps the memory; the descriptor is transient
      val n = if bytes > 0 then bytes else darwinSize(cap, tmp, fd)
      if n <= 0 then throw new java.io.IOException(s"could not measure '$name' (fstat gave $n, errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
      val prot = if readOnly then 0x1 else 0x3   // PROT_READ [| PROT_WRITE]
      val view: MemorySegment = PosixNative.mmap.invoke(cap, MemorySegment.NULL, n, prot, 0x1, fd, 0L)  // MAP_SHARED
      if view.address() == -1L then throw new java.io.IOException(s"mmap failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")  // MAP_FAILED
      arena.into{ a =>
        val whole = view.reinterpret(n, a, s => (PosixNative.munmap.invoke(s, n): Int) __ Unit)
        Mem.Owned.create[A](a)(_ => if readOnly then whole.asReadOnly else whole)   // the pages are PROT_READ: a write must throw, not fault
      }

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
    val virtualQuery  = bind("VirtualQuery",       FunctionDescriptor.of(JAVA_LONG, ADDRESS, ADDRESS, JAVA_LONG), captureErr)
    val localFree     = bind("LocalFree",          FunctionDescriptor.of(ADDRESS, ADDRESS))
    private val adv   = SymbolLookup.libraryLookup("advapi32", arena)
    val sddlToSd      = linker.downcallHandle(
      adv.find("ConvertStringSecurityDescriptorToSecurityDescriptorW").orElseThrow(() => new UnsatisfiedLinkError("missing native symbol: ConvertStringSecurityDescriptorToSecurityDescriptorW")),
      FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, ADDRESS, ADDRESS), captureErr
    )
  }

  /** Windows: runs `f` with a `SECURITY_ATTRIBUTES` for `access` (`NULL` for the default DACL), freeing the
    * descriptor that `ConvertStringSecurityDescriptorToSecurityDescriptorW` allocated once `f` returns. */
  private def withSecurity[T](cap: MemorySegment, tmp: Arena, access: Access)(f: MemorySegment => T): T =
    val sddl = access.descriptor
    if sddl.isEmpty then f(MemorySegment.NULL)
    else
      val out = tmp.allocate(ADDRESS)
      if (WindowsNative.sddlToSd.invoke(cap, tmp.allocateFrom(sddl, StandardCharsets.UTF_16LE), 1, out, MemorySegment.NULL): Int) == 0 then   // SDDL_REVISION_1
        throw new java.io.IOException(s"'$sddl' is not a valid security descriptor (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
      val sd = out.get(ADDRESS, 0L)
      try
        val sa = tmp.allocate(24L)               // { DWORD nLength; LPVOID lpSecurityDescriptor; BOOL bInheritHandle }, 64-bit
        sa.set(JAVA_INT, 0L, 24)
        sa.set(ADDRESS, 8L, sd)
        sa.set(JAVA_INT, 16L, 0)
        f(sa)
      finally (WindowsNative.localFree.invoke(sd): MemorySegment) __ Unit

  /** Windows: the size of the mapped view at `view` — `RegionSize`, byte 24 of a 64-bit `MEMORY_BASIC_INFORMATION`,
    * which for a view of a pagefile section is its page-rounded length; 0 on failure, with the code in `cap`. */
  private def windowsViewSize(cap: MemorySegment, tmp: Arena, view: MemorySegment): Long =
    val mbi = tmp.allocate(48L)
    val got: Long = WindowsNative.virtualQuery.invoke(cap, view, mbi, 48L)
    if got == 0L then 0L else mbi.get(JAVA_LONG, 24L)

  private def attachWindows[A <: Mem.Type](name: String, bytes: Long, readOnly: Boolean): Mem.Owned[A] =
    val access = if readOnly then 0x0004 else 0x0002   // FILE_MAP_READ, or FILE_MAP_WRITE, which alone maps read-write and asks a DACL for no more
    Resource.assemble:
      val arena = undo(Arena.ofShared())(_.close())
      val tmp = scoped(Arena.ofConfined())(_.close())
      val cap = tmp.allocate(WindowsNative.captureLayout)
      val handle: MemorySegment = WindowsNative.openMapping.invoke(cap, access, 0, tmp.allocateFrom(name, StandardCharsets.UTF_16LE))
      if handle.address() == 0L then throw new java.io.IOException(s"OpenFileMapping failed for '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
      scoped(handle)(h => (WindowsNative.closeHandle.invoke(h): Int) __ Unit) __ Unit   // the view keeps the section; the handle is transient
      val view: MemorySegment = WindowsNative.mapView.invoke(cap, handle, access, 0, 0, bytes)   // 0 bytes maps the whole section
      if view.address() == 0L then throw new java.io.IOException(s"MapViewOfFile failed for '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
      val n = if bytes > 0 then bytes else windowsViewSize(cap, tmp, view)
      if n <= 0 then
        (WindowsNative.unmapView.invoke(view): Int) __ Unit
        throw new java.io.IOException(s"could not measure the view of '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
      arena.into{ a =>
        val whole = view.reinterpret(n, a, s => (WindowsNative.unmapView.invoke(s): Int) __ Unit)
        Mem.Owned.create[A](a)(_ => if readOnly then whole.asReadOnly else whole)   // FILE_MAP_READ pages: a write must throw, not fault
      }


  ///////////////////////////////////////////////////////////////////////
  /// Creating a fresh RAM-resident OS-named object (write side)        ///
  ///////////////////////////////////////////////////////////////////////

  private val rng = new java.security.SecureRandom()

  /** Who may open a created object, stated once for all three platforms.  `Default` is what the platform
    * gives a new object: POSIX mode 0600, which is the creating account alone, and on Windows the token's
    * default DACL — normally the creating account and administrators, but a configurable default, not an
    * enforced owner-only policy; state `Custom` for that.  `Everyone` admits any local account that learns
    * the name — what a service and a desktop client under different accounts need — as POSIX mode 0666 and
    * a Windows DACL granting Everyone full access.  `Custom` states both forms outright, a POSIX mode and a
    * Windows SDDL string, each used on its own platform only: `D:(A;;GR;;;WD)` admits everyone to read, and
    * a grant of `FILE_MAP_WRITE` alone (`0x2`) suffices for a read-write attach, which asks for no more.  On
    * macOS the process umask still narrows the mode (a shared-memory object cannot be chmod'ed afterwards);
    * on Linux the mode is applied exactly.
    */
  enum Access {
    case Default
    case Everyone
    case Custom(posixMode: Int, sddl: String)

    /** The POSIX permission bits asked for. */
    def mode: Int = this match
      case Default => 0x180
      case Everyone => 0x1B6
      case Custom(m, _) => m & 0xFFF

    /** The Windows security descriptor in SDDL, or empty for the default DACL. */
    def descriptor: String = this match
      case Default => ""
      case Everyone => "D:(A;;GA;;;WD)"
      case Custom(_, d) => d
  }

  /** A fresh, opaque, hard-to-guess name in POSIX form (leading `/`, no other `/`, short enough for macOS
    * `PSHMNAMLEN`).  Forward slash is legal in a Windows mapping name too, so one form serves all platforms;
    * a caller who wants a namespace prefix drops the slash: `"Global\\" + freshName().drop(1)`. */
  def freshName(): String =
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
      createdLater[A](bytesFor(n, Mem.bytesOf[A], discoverable = false).?, Access.Default)

  /** As [[createNamed]], stating who may open the object (see [[Access]]). */
  inline def createNamed[A <: Mem.Type](n: Long, access: Access): Ask[Tidy.Later[Created[A]]] =
    Ask:
      createdLater[A](bytesFor(n, Mem.bytesOf[A], discoverable = false).?, access)

  /** As [[createNamed]], under a name of the caller's choosing — the form for a protocol whose peer dictates
    * the rendezvous name, so that this side can receive as well as give.  A name already in use is an error,
    * never a retry.  On Linux and macOS the name is a POSIX one (a leading `/` is supplied if missing; no
    * other `/`; at most 31 bytes on macOS); on Windows it is a `CreateFileMapping` name, `Local\`/`Global\`
    * prefix and all, used exactly as given (a name holding a NUL is refused).  `access` says who may open it.
    */
  inline def createNamed[A <: Mem.Type](name: String, n: Long, access: Access = Access.Default): Ask[Tidy.Later[Created[A]]] =
    Ask:
      createdLater[A](name, bytesFor(n, Mem.bytesOf[A], discoverable = false).?, access)

  /** Worker for [[createNamed]]: create `bytes` bytes under a random name and wrap the result in a
    * backstopped `Tidy.Later`. */
  def createdLater[A <: Mem.Type](bytes: Long, access: Access): Tidy.Later[Created[A]] =
    if bytes <= 0 then throw new IllegalArgumentException(s"shared-memory size must be positive, got $bytes bytes")
    Resource.closedLater(createBytes[A](bytes, access))(_.close())

  /** Worker for the named [[createNamed]]: create `bytes` bytes under `name` and wrap the result in a
    * backstopped `Tidy.Later`. */
  def createdLater[A <: Mem.Type](name: String, bytes: Long, access: Access): Tidy.Later[Created[A]] =
    if bytes <= 0 then throw new IllegalArgumentException(s"shared-memory size must be positive, got $bytes bytes")
    Resource.closedLater(createBytesAs[A](name, bytes, access))(_.close())

  /** A random name, drawn again on a clash. */
  private def createBytes[A <: Mem.Type](bytes: Long, access: Access): Created[A] =
    if onLinux then withRetries(createPosixFile[A](freshName(), bytes, access))
    else if onMac then withRetries(createPosixNative[A](freshName(), bytes, access))
    else if onWindows then withRetries(createWindows[A](freshName(), bytes, access))
    else throw new UnsupportedOperationException(s"shared-memory creation is unsupported on '$osName'")

  /** The caller's name, checked for the platform's form; a clash is a failure. */
  private def createBytesAs[A <: Mem.Type](name: String, bytes: Long, access: Access): Created[A] =
    val c =
      if onLinux then createPosixFile[A](checkedPosixName(name), bytes, access)
      else if onMac then createPosixNative[A](checkedPosixName(name), bytes, access)
      else if onWindows then createWindows[A](checkName(name), bytes, access)
      else throw new UnsupportedOperationException(s"shared-memory creation is unsupported on '$osName'")
    if c eq null then throw new java.io.IOException(s"a shared-memory object named '$name' already exists")
    c

  /** A caller's POSIX name in canonical form: one leading `/`, no other, and within macOS's 31 bytes there. */
  private def checkedPosixName(name: String): String =
    val n = posixName(checkName(name))
    if n.length < 2 || n.indexOf('/', 1) >= 0 then
      throw new IllegalArgumentException(s"'$name' is not a POSIX shared-memory name (one leading '/', no other)")
    if onMac && n.getBytes(StandardCharsets.UTF_8).length > 31 then
      throw new IllegalArgumentException(s"'$name' is longer than the 31 bytes a macOS shared-memory name may have")
    n

  /** Fail unless `posixShmDir` is a RAM filesystem, so a Linux create never silently spills to disk. */
  private def ensureRam(): Unit =
    val fsType =
      try Files.getFileStore(posixShmDir).`type`()
      catch case e if e.catchable => throw new java.io.IOException(s"no shared-memory directory at $posixShmDir", e)
    if fsType != "tmpfs" && fsType != "ramfs" then
      throw new java.io.IOException(s"$posixShmDir is on '$fsType', not a RAM filesystem; refusing to create disk-backed shared memory")

  /** Draw random names until one is unused.  Each creator answers `null` for a name clash and nothing else, so
    * a caller with a chosen name reports the clash while this one draws again; a real post-creation failure
    * throws straight through.  Five clashes means the namespace is borked. */
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
  private def createPosixFile[A <: Mem.Type](name: String, bytes: Long, access: Access): Created[A] =
    ensureRam()
    val p = posixShmDir.resolve(name.stripPrefix("/"))
    val perms = posixPerms(access.mode)
    val ch =
      try FileChannel.open(p, java.util.EnumSet.of(StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE_NEW), PosixFilePermissions.asFileAttribute(perms))
      catch case _: java.nio.file.FileAlreadyExistsException => null
    if ch eq null then null
    else Resource.assemble:
      scoped(ch)(_.close()) __ Unit
      undo(p)(f => Files.deleteIfExists(f) __ Unit) __ Unit
      Files.setPosixFilePermissions(p, perms) __ Unit   // the umask narrowed the create; this is the mode asked for
      val arena = undo(Arena.ofShared())(_.close())
      arena.into(a => new Created[A](name, Mem.Owned.create[A](a)(_ => ch.map(FileChannel.MapMode.READ_WRITE, 0L, bytes, a)), () => Files.deleteIfExists(p) __ Unit))

  /** The `PosixFilePermission`s of a mode's nine low bits. */
  private def posixPerms(mode: Int): java.util.Set[PosixFilePermission] =
    val all = PosixFilePermission.values   // OWNER_READ .. OTHERS_EXECUTE: the bits from 0400 down
    val s = java.util.EnumSet.noneOf(classOf[PosixFilePermission])
    var i = 0
    while i < 9 do
      if (mode & (0x100 >> i)) != 0 then s.add(all(i)) __ Unit
      i += 1
    s

  /** macOS: native `shm_open(O_CREAT|O_EXCL|O_RDWR)` + `ftruncate` + `mmap`.  Destroy = `shm_unlink`.  `errno`
    * distinguishes a name clash (`EEXIST` → `null`) from a real failure (→ fail loud with the code). */
  private def createPosixNative[A <: Mem.Type](name: String, bytes: Long, access: Access): Created[A] =
    val tmp = Arena.ofConfined()
    try
      val cap = tmp.allocate(PosixNative.captureLayout)
      val fd: Int = PosixNative.shmOpen.invoke(cap, tmp.allocateFrom(name), 0x0200 | 0x0800 | 0x0002, access.mode)  // O_CREAT|O_EXCL|O_RDWR
      if fd < 0 then
        val e = (PosixNative.errnoVH.get(cap, 0L): Int)
        if e == 17 then null    // EEXIST: a clash, for the caller to retry or report
        else throw new java.io.IOException(s"shm_open failed for '$name' (errno=$e)")
      else Resource.assemble:
        scoped(fd)(PosixSocket.closeQuietly) __ Unit   // the mapping keeps the memory; the descriptor is transient
        undo(name)(shmUnlink) __ Unit                  // only if we fail from here on
        if (PosixNative.ftruncate.invoke(cap, fd, bytes): Int) != 0 then
          throw new java.io.IOException(s"ftruncate failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
        val arena = undo(Arena.ofShared())(_.close())
        arena.into{ a =>
          val view: MemorySegment = PosixNative.mmap.invoke(cap, MemorySegment.NULL, bytes, 0x3, 0x1, fd, 0L)  // PROT_READ|WRITE, MAP_SHARED
          if view.address() == -1L then throw new java.io.IOException(s"mmap failed for '$name' (errno=${PosixNative.errnoVH.get(cap, 0L): Int})")
          val owned = Mem.Owned.create[A](a)(_ => view.reinterpret(bytes, a, s => (PosixNative.munmap.invoke(s, bytes): Int) __ Unit))
          new Created[A](name, owned, () => shmUnlink(name))
        }
    finally tmp.close()

  /** Windows: native `CreateFileMappingW(INVALID_HANDLE_VALUE, …)` — a pagefile-backed (RAM) section — then
    * `MapViewOfFile`.  The creator *holds the section handle* for the region's lifetime so the name stays
    * openable by attachers; destroy = `CloseHandle` (after the arena unmaps the view). */
  private def createWindows[A <: Mem.Type](name: String, bytes: Long, access: Access): Created[A] =
    val tmp = Arena.ofConfined()
    try
      val cap = tmp.allocate(WindowsNative.captureLayout)
      val handle: MemorySegment = withSecurity(cap, tmp, access){ sa =>
        WindowsNative.createMapping.invoke(
          cap,                                                     // captured GetLastError (prepended arg)
          MemorySegment.ofAddress(-1L),                            // INVALID_HANDLE_VALUE: pagefile-backed, not a file
          sa,                                                      // who may open it; NULL is the default DACL
          0x04,                                                    // PAGE_READWRITE
          (bytes >>> 32).toInt, (bytes & 0xFFFFFFFFL).toInt,
          tmp.allocateFrom(name, StandardCharsets.UTF_16LE))
      }
      val lastErr = (WindowsNative.lastErrorVH.get(cap, 0L): Int)
      if handle.address() == 0L then
        throw new java.io.IOException(s"CreateFileMapping failed for '$name' (GetLastError=$lastErr)")   // real failure: fail loud
      else if lastErr == 183 then                                // ERROR_ALREADY_EXISTS: a clash, for the caller to retry or report
        (WindowsNative.closeHandle.invoke(handle): Int) __ Unit
        null
      else Resource.assemble:
        undo(handle)(h => (WindowsNative.closeHandle.invoke(h): Int) __ Unit) __ Unit   // the creator holds the section handle for the region's life
        val arena = undo(Arena.ofShared())(_.close())
        arena.into{ a =>
          val view: MemorySegment = WindowsNative.mapView.invoke(cap, handle, 0x0002, 0, 0, bytes)  // FILE_MAP_WRITE: a read-write view
          if view.address() == 0L then throw new java.io.IOException(s"MapViewOfFile failed for '$name' (GetLastError=${WindowsNative.lastErrorVH.get(cap, 0L): Int})")
          val owned = Mem.Owned.create[A](a)(_ => view.reinterpret(bytes, a, s => (WindowsNative.unmapView.invoke(s): Int) __ Unit))
          new Created[A](name, owned, () => (WindowsNative.closeHandle.invoke(handle): Int) __ Unit)
        }
    finally tmp.close()


  ///////////////////////////////////////////////////////////////////////
  /// Anonymous regions passed by file descriptor (Linux and macOS)    ///
  ///////////////////////////////////////////////////////////////////////

  /** An owned, anonymous shared-memory region: a local mapping plus the descriptor `fd` that another
    * process can map once it reaches them (via [[FdSock.Conn.sendFd]], or [[offerFd]] for the packaged
    * version).  Anonymous means no name in any namespace: the kernel reference-counts descriptors and
    * mappings, so however ungracefully every holder exits, the memory is reclaimed.  `close` unmaps and
    * closes `fd`; peers that already received the descriptor are unaffected.
    */
  final class Anon[A <: Mem.Type] private[SharedMemory] (val fd: Int, val byteSize: Long, owned: Mem.Owned[A]) extends AutoCloseable {
    def memory: Mem[A] = owned.memory
    def op[B](f: Mem[A] => B): B = owned.op(f)
    def use(f: Mem[A] => Unit): Unit = owned.use(f)
    def close(): Unit = try owned.close() finally (PosixNative.close.invoke(fd): Int) __ Unit
  }

  /** Create an anonymous RAM-resident region of `n` items of `A`, reachable by other processes only if
    * its descriptor is passed to them over a Unix socket ([[FdSock]], or the [[offerFd]]/[[acceptFd]]
    * pair).  Linux regions (`memfd_create`) are sealed against shrinking, so no peer can truncate the
    * region and SIGBUS the rest; macOS regions are `shm_open`ed and immediately unlinked (anonymous from
    * birth, but unsealable).  Cleanup at `close`, JVM shutdown, or GC unmaps and drops the descriptor;
    * the kernel frees the memory once the last holder lets go.  Linux/macOS only; needs
    * `--enable-native-access`.
    */
  inline def createFd[A <: Mem.Type](n: Long): Ask[Tidy.Later[Anon[A]]] =
    Ask.flat:
      anonLater[A](bytesFor(n, Mem.bytesOf[A], discoverable = false).?)

  /** Worker for [[createFd]]: create `bytes` bytes and wrap the result in a backstopped `Tidy.Later`. */
  def anonLater[A <: Mem.Type](bytes: Long): Ask[Tidy.Later[Anon[A]]] =
    anonBytes[A](bytes).map(a => Resource.closedLater(a)(_.close()))

  private def anonBytes[A <: Mem.Type](bytes: Long): Ask[Anon[A]] =
    if bytes <= 0 then Err.or(s"shared-memory size must be positive, got $bytes bytes")
    else if onLinux then anonMemfd(bytes)
    else if onMac then anonShm(bytes)
    else Err.or(s"anonymous shared-memory creation is unsupported on '$osName'")

  /** Map `size` bytes of `fd` as an owned Mem (`fd` remains the caller's); read-only mappings give
    * read-only segments, so a stray write is an exception rather than a SIGSEGV.
    */
  private def mapOwned[A <: Mem.Type](fd: Int, size: Long, readOnly: Boolean, cap: MemorySegment): Ask[Mem.Owned[A]] =
    Ask:
      Resource.assemble:
        val arena = undo(Arena.ofShared())(_.close())
        val prot = if readOnly then 0x1 else 0x3   // PROT_READ [| PROT_WRITE]
        val view: MemorySegment = PosixNative.mmap.invoke(cap, MemorySegment.NULL, size, prot, 0x1, fd, 0L)  // MAP_SHARED
        if view.address() == -1L then Err ?# s"mmap failed (errno=${PosixNative.errnoVH.get(cap, 0L): Int})"
        arena.into{ a =>
          val whole = view.reinterpret(size, a, s => (PosixNative.munmap.invoke(s, size): Int) __ Unit)
          Mem.Owned.create[A](a)(_ => if readOnly then whole.asReadOnly else whole)
        }

  /** Map an anonymous fd read-write and take ownership of it (on failure the caller cleans up the fd). */
  private def mapAnonFd[A <: Mem.Type](fd: Int, bytes: Long, cap: MemorySegment): Ask[Anon[A]] =
    mapOwned[A](fd, bytes, readOnly = false, cap).map(o => new Anon[A](fd, bytes, o))

  /** Linux: `memfd_create(MFD_CLOEXEC | MFD_ALLOW_SEALING)`, size it, seal it against shrinking. */
  private def anonMemfd[A <: Mem.Type](bytes: Long): Ask[Anon[A]] =
    val tmp = Arena.ofConfined()
    try
      val cap = tmp.allocate(PosixNative.captureLayout)
      Ask:
        Resource.assemble:
          val fd: Int = PosixNative.memfdCreate.invoke(cap, tmp.allocateFrom("kse-shm"), 0x1 | 0x2)   // MFD_CLOEXEC | MFD_ALLOW_SEALING
          if fd < 0 then Err ?# s"memfd_create failed (errno=${PosixNative.errnoVH.get(cap, 0L): Int})"
          undo(fd)(PosixSocket.closeQuietly) __ Unit   // the Anon owns it once mapped; until then it is ours to close
          if (PosixNative.ftruncate.invoke(cap, fd, bytes): Int) != 0 then
            Err ?# s"ftruncate failed (errno=${PosixNative.errnoVH.get(cap, 0L): Int})"
          if (PosixNative.fcntl.invoke(cap, fd, 1033, 0x2): Int) != 0 then                          // F_ADD_SEALS, F_SEAL_SHRINK
            Err ?# s"F_ADD_SEALS(F_SEAL_SHRINK) failed (errno=${PosixNative.errnoVH.get(cap, 0L): Int})"
          undo(mapAnonFd[A](fd, bytes, cap).?)(_.close())
    finally tmp.close()

  /** macOS: `shm_open` a fresh name, size it, then `shm_unlink` at once — anonymous from birth. */
  private def anonShm[A <: Mem.Type](bytes: Long): Ask[Anon[A]] =
    val tmp = Arena.ofConfined()
    try
      val cap = tmp.allocate(PosixNative.captureLayout)
      Ask.flat:
        var made: Ask[Anon[A]] = Err.or("could not create a uniquely-named shared-memory object in 5 attempts")
        var tries = 0
        var clashing = true
        while clashing && tries < 5 do
          tries += 1
          clashing = false
          val name = freshName()
          val fd: Int = PosixNative.shmOpen.invoke(cap, tmp.allocateFrom(name), 0x0200 | 0x0800 | 0x0002, 0x180)  // O_CREAT|O_EXCL|O_RDWR, 0600
          if fd < 0 then
            val e = (PosixNative.errnoVH.get(cap, 0L): Int)
            if e == 17 then clashing = true   // EEXIST: name clash, try another
            else Err ?# s"shm_open failed (errno=$e)"
          else
            made = Ask:
              Resource.assemble:
                undo(fd)(PosixSocket.closeQuietly) __ Unit   // the Anon owns it once mapped; until then it is ours to close
                scoped(name)(shmUnlink) __ Unit              // anonymous from birth: the name goes once the region is mapped, or sooner
                if (PosixNative.ftruncate.invoke(cap, fd, bytes): Int) != 0 then
                  Err ?# s"ftruncate failed (errno=${PosixNative.errnoVH.get(cap, 0L): Int})"
                undo(mapAnonFd[A](fd, bytes, cap).?)(_.close())
        made
    finally tmp.close()

  /** Map a shared-memory descriptor received from another process (see [[FdSock.Conn.recvFd]]) as `n`
    * items of `A`.  With `n = 0` the size is discovered from the descriptor itself: `lseek` to the end on
    * Linux (exact for a memfd or a file), `fstat` on macOS (where a `shm_open` object reports its length
    * rounded up to whole pages, so a protocol that needs the logical length must carry it).  The descriptor
    * is consumed either way: once mapped, the
    * mapping keeps the memory alive and the descriptor is closed; on failure it is closed too.
    * Linux/macOS only; needs `--enable-native-access`.
    */
  inline def attachFd[A <: Mem.Type](fd: Int, n: Long = 0L, readOnly: Boolean = false)(using Tidy.Nice[Mem.Owned[A]]): Ask[Mem.Owned[A]] =
    Ask.flat:
      val bytes = bytesFor(n, Mem.bytesOf[A], discoverable = true).peekAlt(_ => PosixSocket.closeQuietly(fd)).?   // consumed either way
      attachFdBytes[A](fd, bytes, readOnly)

  /** Worker for [[attachFd]]: map `bytes` bytes (0 = measure it, see [[attachFd]]), consuming the descriptor.
    * Unlike `attachFd`, nothing here asks how the mapping will be released: the caller owns the
    * returned `Mem.Owned` outright, and losing it leaks the mapping until process exit.  Prefer
    * `attachFd` (whose `Tidy.Nice` witness makes an enclosing `Resource` scope state the cleanup),
    * or wrap the result yourself -- `Tidy.Later` for a sole owner, `Tidy.Lease` for a registry that
    * concurrent readers borrow from and that must survive replacement.
    */
  def attachFdBytes[A <: Mem.Type](fd: Int, bytes: Long, readOnly: Boolean): Ask[Mem.Owned[A]] =
    if !(onLinux || onMac) then Err.or(s"descriptor attach is unsupported on '$osName'")
    else if bytes < 0 then
      PosixSocket.closeQuietly(fd)
      Err.or(s"shared-memory size must be positive, or 0 to discover it; got $bytes bytes")
    else Ask:
      Resource.assemble:
        scoped(fd)(PosixSocket.closeQuietly) __ Unit   // consumed either way; a successful mapping keeps the memory
        val tmp = scoped(Arena.ofConfined())(_.close())
        val cap = tmp.allocate(PosixNative.captureLayout)
        val size =
          if bytes > 0 then bytes
          else
            val z: Long = if onMac then darwinSize(cap, tmp, fd) else PosixNative.lseek.invoke(cap, fd, 0L, 2)   // SEEK_END; Darwin's shm descriptors do not seek
            if z <= 0 then Err ?# s"could not measure the shared descriptor (got $z, errno=${PosixNative.errnoVH.get(cap, 0L): Int}); pass n explicitly"
            z
        undo(mapOwned[A](fd, size, readOnly, cap).?)(_.close())

  // The 16-byte header sent ahead of an offered descriptor: magic, version, then the byte size (little-endian).
  private def fdTag(bytes: Long): Array[Byte] =
    val tag = new Array[Byte](16)
    tag(0) = 'k'; tag(1) = 's'; tag(2) = 'e'; tag(3) = 'M'
    tag(4) = 1
    var i = 0
    while i < 8 do
      tag(8 + i) = ((bytes >>> (8 * i)) & 0xFF).toByte
      i += 1
    tag

  /** The byte size from an offer header, or -1 if the header is not ours. */
  private def parseFdTag(tag: Array[Byte]): Long =
    if tag.length >= 16 && tag(0) == 'k'.toByte && tag(1) == 's'.toByte && tag(2) == 'e'.toByte && tag(3) == 'M'.toByte && tag(4) == 1 then
      var bytes = 0L
      var i = 7
      while i >= 0 do
        bytes = (bytes << 8) | (tag(8 + i) & 0xFFL)
        i -= 1
      bytes
    else -1L

  /** An anonymous region being offered at socket `path`: each [[serveOne]] hands the descriptor (plus a
    * small size-bearing header) to one connecting client — [[acceptFd]], or any `SCM_RIGHTS`-speaking
    * program.  `close` stops serving (unlinking `path`) and drops our mapping and descriptor; clients that
    * already received the descriptor keep the memory alive until they too let go.
    */
  final class Offer[A <: Mem.Type] private[SharedMemory] (anon: Anon[A], server: FdSock.Server) extends AutoCloseable {
    def path: Path = server.path
    def memory: Mem[A] = anon.memory
    def op[B](f: Mem[A] => B): B = anon.op(f)
    def use(f: Mem[A] => Unit): Unit = anon.use(f)

    /** Waits (up to the server timeout) for one client and hands it the descriptor. */
    def serveOne(): Ask[Unit] = Ask:
      val conn = server.accept().?
      try conn.sendFd(anon.fd, fdTag(anon.byteSize)).?
      finally conn.close()

    def close(): Unit = try server.close() finally anon.close()
  }

  /** Create an anonymous region of `n` items of `A` (see [[createFd]]) and offer its descriptor at socket
    * `path`: each [[Offer.serveOne]] call serves one connecting [[acceptFd]] (or foreign `SCM_RIGHTS`)
    * client.  Cleanup at `close`, JVM shutdown, or GC stops serving and unmaps; the kernel frees the
    * memory once the last holder is gone.  Linux/macOS only; needs `--enable-native-access`.
    */
  inline def offerFd[A <: Mem.Type](path: Path, n: Long, timeout: Duration = FdSock.defaultTimeout): Ask[Tidy.Later[Offer[A]]] =
    Ask.flat:
      offeredLater[A](path, bytesFor(n, Mem.bytesOf[A], discoverable = false).?, timeout)

  /** Worker for [[offerFd]]: create `bytes` bytes, listen at `path`, wrap in a backstopped `Tidy.Later`. */
  def offeredLater[A <: Mem.Type](path: Path, bytes: Long, timeout: Duration): Ask[Tidy.Later[Offer[A]]] =
    Ask:
      val offer = Resource.assemble:
        val anon = undo(anonBytes[A](bytes).?)(_.close())
        val server = undo(FdSock.listen(path, timeout).?)(_.close())
        anon.into(a => new Offer[A](a, server))
      Resource.closedLater(offer)(_.close())

  /** Connect to an [[offerFd]] socket at `path` and map the offered region; the element count comes from
    * the offer itself, so only the element type `A` need be agreed.  One call: connect, receive the
    * descriptor, map.  Linux/macOS only; needs `--enable-native-access`.
    */
  def acceptFd[A <: Mem.Type](path: Path, readOnly: Boolean = false, timeout: Duration = FdSock.defaultTimeout)(using Tidy.Nice[Mem.Owned[A]]): Ask[Mem.Owned[A]] =
    Ask.flat:
      val conn = FdSock.connect(path, timeout).?
      val tag = new Array[Byte](16)
      var fd = -1
      try
        Ask:
          val r = conn.recvFd(tag).?
          fd = r.fd
          var have = r.count
          while have < tag.length do            // a stream may split the header; the descriptor rode the first byte
            val more = new Array[Byte](tag.length - have)
            val m = conn.read(more).?
            if m == 0 then Err ?# s"peer at $path closed before completing the offer header"
            System.arraycopy(more, 0, tag, have, m)
            have += m
        .peekAlt: _ =>
          if fd >= 0 then (PosixNative.close.invoke(fd): Int) __ Unit
        .?
      finally conn.close()
      val bytes = parseFdTag(tag)
      if bytes <= 0 then
        (PosixNative.close.invoke(fd): Int) __ Unit
        Err ?# s"peer at $path did not speak the kse shared-memory offer protocol"
      attachFdBytes[A](fd, bytes, readOnly)
}
