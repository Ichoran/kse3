// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2020-23 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio


import java.io._
import java.nio.channels.SeekableByteChannel
import java.nio.file._
import java.nio.file.attribute.{ FileTime, BasicFileAttributes }
import java.time._
import java.util.zip._

import scala.annotation.tailrec
import scala.util.boundary

import kse.basics.{given, _}
import kse.flow.{given, _}


extension (pathname: String) {
  inline def file = new File(pathname)
  inline def path = FileSystems.getDefault.getPath(pathname)
  inline def pathIn(fs: FileSystem) = fs.getPath(pathname)
  inline def pathLike(p: Path) = p.getFileSystem.getPath(pathname)
}

extension (the_file: File) {
  inline def path = the_file.toPath
}

extension (the_path: Path) {
  inline def name = the_path.getFileName.toString

  inline def nameTo(s: String) = the_path resolveSibling s

  inline def nameOp(inline f: String => String) = the_path resolveSibling f(the_path.getFileName.toString)

  def ext =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 || i == n.length - 1 then "" else n.substring(i+1)
  
  def extTo(x: String) =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 || i == n.length - 1 then
      if x.isEmpty then the_path
      else the_path resolveSibling (n + "." + x)
    else
      if x.isEmpty then the_path resolveSibling n.substring(0, i)
      else the_path resolveSibling n.substring(0, i+1) + x
  
  def extOp(f: String => String) =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    val e = if i < 1 || i == n.length -1 then "" else n.substring(i+1)
    val x = f(e)
    if x == e then the_path
    else if e.isEmpty then the_path resolveSibling (n + "." + x)
    else if x.isEmpty then the_path resolveSibling n.substring(0, i)
    else the_path resolveSibling n.substring(0, i+1) + x
  
  def base =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 || i == n.length - 1 then n else n.substring(0, i)
  
  def baseTo(b: String) = 
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 || i == n.length - 1 then
      if n == b then the_path
      else the_path resolveSibling b
    else
      if i == b.length && n.substring(0, i) == b then the_path
      else the_path resolveSibling b + n.substring(i)
  
  def baseOp(f: String => String) =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    val b = if i < 1 || i == n.length - 1 then n else n.substring(0, i)
    val x = f(b)
    if b == x then the_path
    else if (b eq n) then the_path resolveSibling x
    else the_path resolveSibling x+n.substring(i)
  
  inline def parentName = the_path.getParent match
    case null => ""
    case p => p.getFileName.toString

  def namesIterator = Iterator.iterate(the_path)(_.getParent).takeWhile(_ != null).map(_.getFileName.toString)

  def pathsIterator = Iterator.iterate(the_path)(_.getParent).takeWhile(_ != null)

  def parent: Path Or Unit =
    val p = the_path.getParent
    if p eq null then Alt.unit else Is(p)

  inline def absolute = the_path.toAbsolutePath()

  inline def /(that: String) = the_path resolve that

  inline def /(that: Path) = the_path resolve that

  inline def `..` = the_path.getParent match
    case null => the_path
    case p => p

  inline def sib(that: String) = the_path resolveSibling that

  inline def sib(that: Path) = the_path resolveSibling that

  def reroot(oldRoot: Path, newRoot: Path): Path Or Unit =
    if the_path startsWith oldRoot then Is(newRoot resolve oldRoot.relativize(the_path))
    else Alt.unit

  inline def reroot(roots: (Path, Path)): Path Or Unit = reroot(roots._1, roots._2)

  def adopt(child: Path): Path Or Unit =
    if child startsWith the_path then Is(the_path relativize child)
    else Alt.unit

  inline def relativeTo(other: Path) = other relativize the_path


  inline def raw: kse.eio.PathsHelper.RawPath = PathsHelper.RawPath(the_path)

  def real: Path =
    ratchet(the_path): upath =>
      ratchet(upath.toAbsolutePath): abs =>
        ratchet(abs.normalize): norm =>
          PathsHelper.symlinkToReal(norm)
  
  inline def file: File Or Err = nice{ the_path.toFile }

  inline def exists = Files exists the_path

  inline def isDirectory = Files isDirectory the_path

  inline def isSymlink = Files isSymbolicLink the_path

  def size: Long = ratchet(-1L): _ =>
    if Files.exists(the_path) then Files.size(the_path)
    else -1L

  def time: FileTime Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else nice{ Files getLastModifiedTime the_path }

  def time_=(ft: FileTime): Unit Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else nice{ Files.setLastModifiedTime(the_path, ft) }

  def mkdir(): Boolean Or Err =
    if Files.exists(the_path) then
      if Files.isDirectory(the_path) then Is(false) else Err.or(s"$the_path already exists and is not a directory")
    else the_path.getParent match
      case null => nice{ Files createDirectory the_path ; true }
      case p =>
        if !Files.exists(p) then Err.or(s"Cannot create $the_path because $p not found")
          else nice{ Files createDirectory the_path ; true }

  inline def mkdirs(): Unit Or Err = nice{ Files createDirectories the_path ; () }

  inline def mkParents(): Unit Or Err = nice{ PathsHelper.RawPath(the_path).mkParents() }

  inline def delete(): Boolean = ratchet(false): _ =>
    Files deleteIfExists the_path

  def makeSymlink(s: String): Unit Or Err = Err.Or:
    val target = the_path.getFileSystem.getPath(s)
    if Files.exists(the_path) then
      if Files.isSymbolicLink(the_path) && Files.readSymbolicLink(the_path) == target then ()
      else Err.break(s"$the_path exists so can't create it as a symbolic link")
    else
      if Files.isSymbolicLink(the_path) then Files.delete(the_path)
      Files.createSymbolicLink(the_path, target)
      ()

  def symlinkTo(p: Path): Unit Or Err = Err.Or:
    if Files.exists(the_path) then
      if Files.isSymbolicLink(the_path) && Files.readSymbolicLink(the_path) == p then ()
      else Err.break(s"$the_path exists so can't create it as a symbolic link")
    else
      if Files.isSymbolicLink(the_path) then Files.delete(the_path)
      if p.isAbsolute then Files.createSymbolicLink(the_path, p)
      else the_path.getParent match
        case null => Files.createSymbolicLink(the_path, p)
        case q    => Files.createSymbolicLink(the_path, q relativize p)

  def symlink: String Or Err =
    if Files isSymbolicLink the_path then nice{ (Files readSymbolicLink the_path).toString }
    else Err.or(s"$the_path is not a symbolic link")

  def followSymlink: Path Or Err =
    if Files isSymbolicLink the_path then nice {
      val q = Files readSymbolicLink the_path
      if q.isAbsolute then q
      else the_path.getParent match
        case null => q
        case p    => (p resolve q).normalize
    }
    else Err.or(s"$the_path is not a symbolic link")

  def touch(): Unit Or Err = nice {
    if Files exists the_path then Files.setLastModifiedTime(the_path, FileTime from Instant.now)
    else Files.write(the_path, new Array[Byte](0))
  }

  def paths =
    if !(Files exists the_path) || !(Files isDirectory the_path) then PathsHelper.emptyPathArray
    else Resource
      .safe(Files list the_path)(_.close): list =>
        list.toArray(i => new Array[Path](i))
      .getOrElse(_ => PathsHelper.emptyPathArray)

  def slurp: Array[String] Or Err =
    if Files exists the_path then
      nice{ Resource(Files lines the_path)(_.close)(_.toArray(i => new Array[String](i))) }
    else Err.or(s"$the_path not found")

  def gulp: Array[Byte] Or Err =
    if Files exists the_path then nice{ Files readAllBytes the_path }
    else Err.or(s"$the_path not found")

  def write(data: Array[Byte]): Unit Or Err = nice{ Files.write(the_path, data) }

  def append(data: Array[Byte]): Unit Or Err =
    nice{ Files.write(the_path, data, StandardOpenOption.APPEND, StandardOpenOption.CREATE) }

  def create(data: Array[Byte]): Unit Or Err =
    if Files exists the_path then Err.or(s"$the_path already exists")
    else nice{ Files.write(the_path, data, StandardOpenOption.CREATE_NEW) }

  def createIfAbsent(data: Array[Byte]): Boolean Or Err =
    if Files.exists(the_path) then Is(false)
    else nice {
      Files.write(the_path, data, StandardOpenOption.CREATE_NEW)
      true
    }

  def writeLines(coll: scala.collection.IterableOnce[String]): Unit Or Err =
    nice{ Files.write(the_path, PathsHelper.javaIterable(coll)) }

  def appendLines(coll: scala.collection.IterableOnce[String]): Unit Or Err =
    nice{ Files.write(the_path, PathsHelper.javaIterable(coll), StandardOpenOption.APPEND, StandardOpenOption.CREATE) }

  def createLines(coll: scala.collection.IterableOnce[String]): Unit Or Err =
    if Files exists the_path then Err.or(s"$the_path already exists")
    else nice{ Files.write(the_path, PathsHelper.javaIterable(coll), StandardOpenOption.CREATE_NEW) }

  def createLinesIfAbsent(coll: scala.collection.IterableOnce[String]): Boolean Or Err =
    if Files.exists(the_path) then Is(false)
    else nice {
      Files.write(the_path, PathsHelper.javaIterable(coll), StandardOpenOption.CREATE_NEW)
      true
    }


  def openRead(bufferSize: Int = 8192)(using Tidy.Nice[InputStream]): InputStream Or Err =
    if Files exists the_path then nice {
      val is = Files newInputStream the_path
      if bufferSize > 0 then new BufferedInputStream(is, 8192) else is
    }
    else Err.or(s"$the_path not found")

  def openWrite(bufferSize: Int = 8192)(using Tidy.Nice[OutputStream]): OutputStream Or Err =
    nice{
      val os = Files newOutputStream the_path
      if bufferSize > 0 then new BufferedOutputStream(os, 8192) else os 
    }

  def openAppend(bufferSize: Int = 8192)(using Tidy.Nice[OutputStream]): OutputStream Or Err =
    nice {
      val os = Files.newOutputStream(the_path, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
      if bufferSize > 0 then new BufferedOutputStream(os, bufferSize) else os
    }

  def openCreate(bufferSize: Int = 8192)(using Tidy.Nice[OutputStream]): OutputStream Or Err =
    if Files.exists(the_path) then Err.or(s"$the_path not found")
    else nice {
      val os = Files.newOutputStream(the_path, StandardOpenOption.CREATE_NEW)
      if bufferSize > 0 then new BufferedOutputStream(os, bufferSize) else os
    }

  def openIO()(using Tidy.Nice[SeekableByteChannel]): SeekableByteChannel Or Err = nice {
    Files.newByteChannel(the_path, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE)
  }


  inline def inZip[A](inline f: boundary.Label[Unit Or Err] ?=> (Path => Unit)): Unit Or Err =
    Resource.Nice{
      val fsys = FileSystems.newFileSystem(the_path, null: ClassLoader)
      var result: Path Or Err = Err.or(s"No directory structure inside $the_path")
      try
        val i = fsys.getRootDirectories.iterator
        if i.hasNext then
          result = Is(i.next)
          if i.hasNext then result = Err.or(s"Multiple directories inside $the_path\n  $result\n  ${safe(i.next)}")
      finally
        if result.isAlt then fsys.close
      result
    }(_.getFileSystem.close)(f)

  def asUnmanagedFilesystem(): FileSystem Or Err =
    nice( FileSystems.newFileSystem(the_path, null: ClassLoader) )


  def copyTo(to: Path): Unit Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else nice{ Files.copy(the_path, to, StandardCopyOption.REPLACE_EXISTING) }

  def copyInto(that: Path): Path Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else if !Files.exists(that) then Err.or(s"Target directory $that not found")
    else if !Files.isDirectory(that) then Err.or(s"Target $that is not a directory")
    else nice:
      val target = that resolve the_path.getFileName
      Files.copy(the_path, target, StandardCopyOption.REPLACE_EXISTING)
      target

  def copyCreate(to: Path): Unit Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else if Files.exists(to) then Err.or(s"$to already exists")
    else nice{ Files.copy(the_path, to) }

  def moveTo(to: Path): Unit Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else nice{ Files.move(the_path, to, StandardCopyOption.REPLACE_EXISTING) }

  def moveCreate(to: Path): Unit Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else nice{ Files.move(the_path, to) }

  def moveInto(that: Path): Path Or Err =
    if !Files.exists(the_path) then Err.or(s"$the_path not found")
    else if !Files.exists(that) then Err.or(s"Target directory $that not found")
    else if !Files.isDirectory(that) then Err.or(s"Target $that is not a directory")
    else nice:
      val target = that resolve the_path.getFileName
      Files.move(the_path, target, StandardCopyOption.REPLACE_EXISTING)
      target


  inline def atomically: PathsHelper.AtomicPathOps = PathsHelper.AtomicPathOps(the_path)

  inline def recursively = new PathsHelper.RootedRecursion(the_path, the_path)

  def recurseIn(inside: Path) =
    if the_path startsWith inside then new PathsHelper.RootedRecursion(inside, the_path)
    else throw new IOException(s"Trying recursive operation in $inside but started outside at $the_path")
}


/*
extension (underlying: File) {
  def name = underlying.getName

  HERE -- TODO -- HERE

  def nameTo(s: String) = underlying resolveSibling s

  def nameFn(f: String => String) = underlying resolveSibling f(underlying.getFileName.toString)

  def ext =
    val n = underlying.getName
    val i = n.lastIndexOf('.')
    if (i < 1) "" else n.substring(i+1)
  
  def extTo(x: String) =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    if (i < 1) {
      if (x.isEmpty) underlying
      else underlying resolveSibling n + "." + x
    }
    else {
      if (x.isEmpty) underlying resolveSibling n.substring(0, i)
      else underlying resolveSibling n.substring(0, i+1) + x
    }
  
  def extFn(f: String => String) =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    val e = if (i < 1) "" else n.substring(i+1)
    val x = f(e)
    if (x == e) underlying
    else if (i < 1) underlying resolveSibling n + "." + x
    else if (x.isEmpty) underlying resolveSibling n.substring(0, i)
    else underlying resolveSibling n.substring(0, i+1) + x
  
  def base =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    if (i < 1) n else n.substring(0, i)
  
  def baseTo(b: String) = 
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    if (i < 1) {
      if (n == b) underlying
      else underlying resolveSibling b
    }
    else {
      if (i == b.length && n.substring(0, i) == b) underlying
      else underlying resolveSibling b + n.substring(i)
    }
  
  def baseFn(f: String => String) =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    val b = if (i < 1) n else n.substring(0, i)
    val x = f(b)
    if (b == x) underlying
    else if (i < 1) underlying resolveSibling x
    else underlying resolveSibling x+n.substring(i)
  
  def parentName = underlying.getParent match { case null => ""; case p => p.getFileName.toString }

  def namesIterator = Iterator.iterate(underlying)(_.getParent).takeWhile(_ != null).map(_.getFileName.toString)

  def pathsIterator = Iterator.iterate(underlying)(_.getParent).takeWhile(_ != null)

  def parentOption = Option(underlying.getParent)

  def absolute = underlying.toAbsolutePath()

  def real =
    var abs = underlying.toAbsolutePath().normalize()
    var tail: Path = null
    var found = false
    while (abs != null && !{ found = Files exists abs; found }) {
      tail = if (tail eq null) abs.getFileName else abs.getFileName resolve tail
      abs = abs.getParent
    }
    val trunk = if (found) abs.toRealPath() else abs
    if (tail eq null) trunk else trunk resolve tail
  
  def file = underlying.toFile

  def /(that: String) = underlying resolve that

  def /(that: Path) = underlying resolve that

  def `..` = underlying.getParent match { case null => underlying; case p => p }

  def sib(that: String) = underlying resolveSibling that

  def sib(that: Path) = underlying resolveSibling that

  def reroot(oldRoot: Path, newRoot: Path): Option[Path] =
    if (underlying startsWith oldRoot) Some(newRoot resolve oldRoot.relativize(underlying))
    else None

  def reroot(roots: (Path, Path)): Option[Path] = reroot(roots._1, roots._2)

  def prune(child: Path): Option[Path] =
    if (child startsWith underlying) Some(underlying relativize child)
    else None

  def exists = Files exists underlying

  def isDirectory = Files isDirectory underlying

  def isSymbolic = Files isSymbolicLink underlying

  def size = Files size underlying

  def safely = new PathShouldSafelyDoThis(underlying)

  def t: FileTime = Files getLastModifiedTime underlying

  def t_=(ft: FileTime): Unit =
    Files.setLastModifiedTime(underlying, ft)
  
  def mkdir() = Files createDirectory underlying

  def mkdirs() = Files createDirectories underlying

  def delete() = Files delete underlying

  def touch(): Unit =
    if (Files exists underlying) Files.setLastModifiedTime(underlying, FileTime from Instant.now)
    else Files.write(underlying, new Array[Byte](0))

  def paths =
    if (!(Files exists underlying)) PathsHelper.emptyPathArray
    else if (!(Files isDirectory underlying)) PathsHelper.emptyPathArray
    else safe{ 
      val list = Files.list(underlying)
      val ans = list.toArray(i => new Array[Path](i))
      list.close
      ans
    }.yesOr(_ => PathsHelper.emptyPathArray)

  def slurp: Ok[String, Array[String]] = safe {
    val s = Files lines underlying
    val vb = Array.newBuilder[String]
    s.forEach(vb += _)
    s.close
    vb.result
  }.mapNo(_.explain())

  def gulp: Ok[String, Array[Byte]] = safe {
    Files readAllBytes underlying
  }.mapNo(_.explain())

  /*
  // TODO -- evaluate whether it's better to use a ZipFileSystem to do this
  def unzipMap[A](selector: ZipEntry => Option[Array[Byte] => A]): Ok[String, List[A]] = safe{
    (new InputStreamShouldDoThis(Files newInputStream underlying)).unzipMap(selector).mapNo(e => s"Error while unzipping $underlying\n$e")
  }.mapNo(e => s"Could not open path $underlying\n${e.explain()}").flatten
  */

  def copyTo(to: Path): Unit =
    Files.copy(underlying, to, StandardCopyOption.REPLACE_EXISTING)

  def moveTo(to: Path): Unit =
    Files.move(underlying, to, StandardCopyOption.REPLACE_EXISTING)

  def atomicCopy(to: Path): Unit =
    val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
    to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
    Files.copy(underlying, temp, StandardCopyOption.REPLACE_EXISTING)
    Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  def atomicMove(to: Path): Unit =
    val up = to.getParent
    if (up != null) {
      if (!Files.exists(up)) Files.createDirectories(up)
    }
    if (up != null && Files.getFileStore(underlying) == Files.getFileStore(up))
      Files.move(underlying, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    else {        
      val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
      to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
      Files.copy(underlying, temp, StandardCopyOption.REPLACE_EXISTING)
      Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      Files.delete(underlying)
    }

  def atomicZipCopy(to: Path, compression: Option[Int] = None, maxDirectoryDepth: Int = 10): Unit =
    val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
    to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
    val zos = new ZipOutputStream(new FileOutputStream(temp.toFile))
    compression.foreach(zos.setLevel)
    if (Files.isDirectory(underlying)) {
      val base = underlying.getParent.fn{ fp => if (fp eq null) FileSystems.getDefault.getPath("") else fp }
      def recurse(current: Path, maxDepth: Int): Unit = {
        val stable = current.paths
        val (directories, files) = stable.sortBy(_.getFileName.toString).partition(x => Files.isDirectory(x))
        files.foreach{ fi =>
          val rel = base relativize fi
          val ze = new ZipEntry(rel.toString)
          ze.setLastModifiedTime(Files.getLastModifiedTime(fi))
          zos.putNextEntry(ze)
          Files.copy(fi, zos)
          zos.closeEntry
        }
        if (maxDepth > 1) directories.foreach(d => recurse(d, maxDepth-1))
      }
      recurse(underlying, maxDirectoryDepth)
    }
    else {
      val ze = new ZipEntry(underlying.getFileName.toString)
      ze.setLastModifiedTime(Files.getLastModifiedTime(underlying))
      zos.putNextEntry(ze)
      Files.copy(underlying, zos)
      zos.closeEntry
    }
    zos.close
    Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  def recursively = new PathsHelper.RootedRecursion(underlying, underlying)

  def recurseIn(inside: Path) =
    if (underlying startsWith inside) new PathsHelper.RootedRecursion(inside, underlying)
    else throw new IOException(s"Trying recursive operation in $inside but started outside at $underlying")
}
*/


object PathsHelper {
  val emptyPathArray = new Array[Path](0)

  @tailrec
  private[eio] def symlinkToReal(norm: Path, syms: List[(Path, Object, Int, Path)] = Nil): Path =
    var extant = norm
    var last: Path = null
    var n = 0
    while extant != null && !Files.exists(extant) do
      last = extant
      extant = extant.getParent
      n += 1
    if extant == null then
      norm
    else if n == 0 then
      extant.toRealPath()
    else if !Files.isSymbolicLink(last) then
      extant.toRealPath() resolve norm.subpath(norm.getNameCount - n, norm.getNameCount)
    else
      val real = extant.toRealPath()
      val symname = last.getFileName
      val direct = real resolve norm.subpath(norm.getNameCount - n, norm.getNameCount)
      val sympath = if n == 1 then direct else real resolve last.getFileName
      val key = Files.readAttributes(sympath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS).fileKey
      if syms.exists{ case (p, o, i, q) => (p == sympath || (o != null && key != null & o == key)) && !(n < i) } then
        syms.reduce{ (l, r) =>
          if l._3 < r._3 || (l._3 == r._3 && l._1.getNameCount < r._1.getNameCount) then l else r
        }._4
      else
        val link = Files.readSymbolicLink(sympath)
        val target = (if link.isAbsolute then link else real resolve link).normalize()
        val full = if n < 2 then target else target resolve norm.subpath(norm.getNameCount - (n-1), norm.getNameCount)
        symlinkToReal(full, ((sympath, key, n, direct)) :: syms)

  def javaIterable(i1: scala.collection.IterableOnce[String]): java.lang.Iterable[String] = new java.lang.Iterable[String] {
    def iterator(): java.util.Iterator[String] = new java.util.Iterator[String] {
      private val i = i1.iterator
      def next: String = i.next
      def hasNext: Boolean = i.hasNext
    }
  }


  opaque type AtomicPathOps = Path
  object AtomicPathOps {
    inline def apply(the_path: Path): kse.eio.PathsHelper.AtomicPathOps = the_path

    extension (the_path: AtomicPathOps)
      inline def underlying: Path = the_path

    extension (the_path: kse.eio.PathsHelper.AtomicPathOps) {
      def tempPath: Path = the_path.underlying.resolveSibling(the_path.underlying.getFileName.toString + ".atomic")

      def copyTo(to: Path): Unit =
        val temp = apply(to).tempPath
        to.getParent.tap{ gp => if gp ne null then { if !Files.exists(gp) then Files.createDirectories(gp) } }
        Files.copy(the_path.underlying, temp, StandardCopyOption.REPLACE_EXISTING)
        Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

      def moveTo(to: Path): Unit =
        val up = to.getParent
        if up != null then
          if !Files.exists(up) then Files.createDirectories(up)
        if up != null && Files.getFileStore(the_path) == Files.getFileStore(up) then
          Files.move(the_path.underlying, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        else       
          val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
          to.getParent.tap{ gp => if gp ne null then { if !Files.exists(gp) then Files.createDirectories(gp) } }
          Files.copy(the_path.underlying, temp, StandardCopyOption.REPLACE_EXISTING)
          Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
          Files.delete(the_path.underlying)

      def zipTo(to: Path, compression: Int Or Unit = Alt.unit, maxDirectoryDepth: Int = 10): Unit =
        val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
        to.getParent.tap{ gp => if gp ne null then { if !Files.exists(gp) then Files.createDirectories(gp) } }
        val zos = new ZipOutputStream(new FileOutputStream(temp.toFile))
        compression.foreach(zos.setLevel)
        if Files.isDirectory(the_path.underlying) then
          val base = the_path.underlying.getParent.fn{ fp => if fp eq null then the_path.underlying.getFileSystem.getPath("") else fp }
          def recurse(current: Path, maxDepth: Int): Unit =
            val stable = current.paths
            val (directories, files) = stable.sortBy(_.getFileName.toString).partition(x => Files.isDirectory(x))
            files.foreach{ fi =>
              val rel = base relativize fi
              val ze = new ZipEntry(rel.toString)
              ze.setLastModifiedTime(Files.getLastModifiedTime(fi))
              zos.putNextEntry(ze)
              Files.copy(fi, zos)
              zos.closeEntry
            }
            if maxDepth > 1 then directories.foreach(d => recurse(d, maxDepth-1))
          recurse(the_path, maxDirectoryDepth)
        else
          val ze = new ZipEntry(the_path.underlying.getFileName.toString)
          ze.setLastModifiedTime(Files.getLastModifiedTime(the_path))
          zos.putNextEntry(ze)
          Files.copy(the_path, zos)
          zos.closeEntry
        zos.close
        Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    }
  }


  val doNothingHook: Path => Unit = _ => ()

  class RootedRecursion(val root: Path, val origin: Path) {
    def delete(hook: Path => Unit = doNothingHook) = recursiveDelete(origin, root, hook)
    def atomicDelete(hook: Path => Unit = doNothingHook) = atomicRecursiveDelete(origin, root, hook)
  }
  private[PathsHelper] def recursiveDelete(f: Path, root: Path, hook: Path => Unit = _ => ()): Unit =
    if !(f startsWith root) then throw new IOException(s"Tried to delete $f but escaped root path $root")
    if Files.isDirectory(f) && !Files.isSymbolicLink(f) then
      f.paths.foreach(fi => recursiveDelete(fi, root, hook))
    hook(f)
    Files delete f

  private[PathsHelper] def atomicRecursiveDelete(f: Path, root: Path, hook: Path => Unit = _ => ()): Unit =
    if !(f startsWith root) then throw new IOException(s"Tried to delete $f but escaped root path $root")
    val name = f.getFileName.toString
    val i = name.lastIndexOf('.')
    val delext = if i > 0 then name.substring(i+1) else ""
    val delnum = 
      if !delext.startsWith("deleted") then None
      else if delext.length > "deleted".length + 8 then None
      else if delext.length > "deleted".length then
        val more = delext.substring("deleted".length)
        if !more.forall(_.isDigit) then None
        else safe{ more.toInt }.toOption
      else Some(1)
    val delname = delnum match
      case Some(n) => name.substring(0, i) + ".deleted" + (n+1).toString
      case _ => name + ".deleted"
    val del = f.resolveSibling(delname).normalize
    if Files exists del then atomicRecursiveDelete(del, root, hook)
    Files.move(f, del, StandardCopyOption.ATOMIC_MOVE)
    val drp = del.toRealPath()
    recursiveDelete(drp, drp, hook)

  opaque type RawPath = Path
  object RawPath {
    def apply(path: Path): kse.eio.PathsHelper.RawPath = path

    extension (the_path: RawPath)
      def path: Path = the_path

    extension (the_path: kse.eio.PathsHelper.RawPath) {
      def real = PathsHelper.symlinkToReal(the_path.path.toAbsolutePath().normalize())

      inline def file = the_path.path.toFile

      inline def size = Files size the_path.path

      inline def time: FileTime = Files getLastModifiedTime the_path.path

      inline def time_=(ft: FileTime): Unit = Files.setLastModifiedTime(the_path.path, ft)


      inline def mkdir(): Unit = Files createDirectory the_path.path

      inline def mkdirs(): Unit = Files createDirectories the_path.path

      def mkParents(): Unit =
        val p = the_path.path.getParent
        if (p ne null) && !Files.exists(p) then
          if Files.isSymbolicLink(p) then
            Files.createDirectories(PathsHelper.symlinkToReal(the_path.path.toAbsolutePath().normalize()))
          else Files.createDirectories(p)

      inline def delete() = Files delete the_path.path

      def makeSymlink(s: String): Unit =
        Files.createSymbolicLink(the_path.path, the_path.path.getFileSystem.getPath(s))
        ()

      def symlinkTo(p: Path): Unit =
        if p.isAbsolute then Files.createSymbolicLink(the_path.path, p)
        else the_path.path.getParent match
          case null => Files.createSymbolicLink(the_path.path, p)
          case q    => Files.createSymbolicLink(the_path.path, q relativize p)
        ()

      inline def symlink: String = (Files readSymbolicLink the_path.path).toString

      def followSymlink: Path =
        val q = Files readSymbolicLink the_path.path
        if q.isAbsolute then q
        else the_path.path.getParent match
          case null => q
          case p    => (p resolve q).normalize

      def touch(): Unit =
        if Files exists the_path.path then Files.setLastModifiedTime(the_path.path, FileTime from Instant.now)
        else Files.write(the_path.path, new Array[Byte](0))

      def slurp: Array[String] =
        Resource(Files lines the_path.path)(_.close)(_.toArray(i => new Array[String](i)))

      inline def gulp: Array[Byte] = Files readAllBytes the_path.path

      inline def openRead(): java.io.BufferedInputStream =
        new BufferedInputStream(Files newInputStream the_path, 8192)

      inline def write(data: Array[Byte]): Unit =
        Files.write(the_path, data)

      inline def append(data: Array[Byte]): Unit =
        Files.write(the_path, data, StandardOpenOption.APPEND, StandardOpenOption.CREATE)

      inline def create(data: Array[Byte]): Unit =
        Files.write(the_path, data, StandardOpenOption.CREATE_NEW)

      def writeLines(coll: scala.collection.IterableOnce[String]): Unit =
        Files.write(the_path, PathsHelper.javaIterable(coll))

      def appendLines(coll: scala.collection.IterableOnce[String]): Unit =
        Files.write(the_path, PathsHelper.javaIterable(coll), StandardOpenOption.APPEND, StandardOpenOption.CREATE)

      def createLines(coll: scala.collection.IterableOnce[String]): Unit =
        Files.write(the_path, PathsHelper.javaIterable(coll), StandardOpenOption.CREATE_NEW)

      inline def openWrite(): java.io.BufferedOutputStream =
        new BufferedOutputStream(Files newOutputStream the_path, 8192)

      inline def openAppend(): java.io.BufferedOutputStream =
        new BufferedOutputStream(
          Files.newOutputStream(the_path, StandardOpenOption.APPEND, StandardOpenOption.CREATE),
          8192
        )

      inline def openCreate(): java.io.BufferedOutputStream =
        new BufferedOutputStream(
          Files.newOutputStream(the_path, StandardOpenOption.CREATE_NEW),
          8192
        )

      inline def openIO(): SeekableByteChannel =
        Files.newByteChannel(the_path, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE)


      inline def copyTo(to: Path): Unit = Files.copy(the_path.path, to, StandardCopyOption.REPLACE_EXISTING)

      inline def moveTo(to: Path): Unit = Files.move(the_path.path, to, StandardCopyOption.REPLACE_EXISTING)
    }  
  }
}
