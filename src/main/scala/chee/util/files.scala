package chee.util

import eu.medsea.mimeutil.MimeUtil2
import eu.medsea.mimeutil.detector.{ExtensionMimeDetector, MagicMimeMimeDetector}
import java.io.{InputStream, OutputStream}
import java.net.{URL, URLConnection}
import java.nio.file.{Files, Paths}
import javax.activation.{MimeType, MimetypesFileTypeMap}
import java.util.zip.{ZipOutputStream, ZipEntry}
import better.files._
import better.files.File.LinkOptions
import scala.io.Codec
import scala.util.Try

object files {

  private[this] val mimeTypesMap = new MimetypesFileTypeMap()
  private[this] val mimeUtil = {
    val u = new MimeUtil2
    u.registerMimeDetector(classOf[ExtensionMimeDetector].getName)
    u.registerMimeDetector(classOf[MagicMimeMimeDetector].getName)
    u
  }

  object Directory {
    def unapply(f: File): Option[File] =
      if (f.isDirectory(LinkOptions.follow)) Some(f) else None
  }

  object RegularFile {
    def unapply(f: File): Option[File] =
      if (f.isRegularFile(LinkOptions.follow)) Some(f) else None
  }

  final class ZipArchive(zip: ZipOutputStream) {
    def add(f: File, name: String): ZipArchive = synchronized {
      val fin = f.newInputStream()
      add(fin, name)
      fin.close()
      this
    }

    def add(url: URL, name: String): ZipArchive = synchronized {
      val conn = url.openConnection
      conn.connect()
      val in = conn.getInputStream
      add(in, name)
      in.close()
      this
    }

    def add(in: InputStream, name: String): ZipArchive = synchronized {
      val entry = new ZipEntry(name)
      zip.putNextEntry(entry)
      transfer(in, zip)
      zip.closeEntry()
      this
    }

    def + (e: (File, String)) = add(e._1, e._2)

    private def transfer(in: InputStream, out: OutputStream): Int = {
      val buf = new Array[Byte](8192)
      def loop(n: Int): Int = in.read(buf) match {
        case -1 => n
        case len =>
          out.write(buf, 0, len)
          loop(n + len)
      }
      loop(0)
    }

    def close(): Unit = zip.close()
  }

  object ZipArchive {
    def uniqueEntry(name: String, entries: Set[String]): String = {
      val cwd = File("")
      val toName = (f: File) => (cwd relativize f).toString
      val unique = File(name).makeNonExisting(exists = f => entries(toName(f))).map(toName)
      unique getOrElse sys.error("Cannot create unique zip entry. Rename limit exceeded.")
    }
  }

  implicit class FileExt(f: File) {
    private val NonExistingRegex = """(.*?-)([0-9]+)""".r

    /**
      * Split the name of the file in basename and extension.
      */
    def splitFileName: (String, String) = f.name.lastIndexOf('.') match {
      case i if i > 0 => (f.name.substring(0, i), f.name.substring(i + 1))
      case _ => (f.name, "")
    }

    /**
      * Return the extension of the given file (without the dot). This
      * method works on names, i.e. it doesn't care whether this names
      * a directory, symlink or file.
      */
    def getExtension: Option[String] =
      Some(f.splitFileName._2).filter(_.nonEmpty)

    def stripExtension: File = getExtension match {
      case Some(ext) => File(f.pathAsString.substring(0, f.pathAsString.length - ext.length -1))
      case None => f
    }

    /**
      * Return the name without extension. This method works on names,
      * i.e. it doesn't care whether this names a directory, symlink
      * or file.
      */
    def getBaseName: String = splitFileName._1

    /**
      * Check whether the given path has an extension from the given
      * set. Compare case-insensitiv.
      */
    def hasExtensions(exts: Set[String]): Boolean =
      f.getExtension.map(_.toLowerCase()) match {
        case Some(ext) => exts contains ext
        case _ => exts.isEmpty
      }

    /**
      * Same as `this / fn`.
      */
    def mapPath(fn: File => File): File = fn(f)

    /**
      * Resolve to a sibling by mapping the file name.
      */
    def mapFileName(fn: String => String): File =
      mapPath(p => p.sibling(fn(f.name)))

    /**
      * Resolve to a sibling by mapping the base name.
      */
    def mapBaseName(fn: String => String): File = {
      def rename(ff: File): File = {
        val (bname, ext) = ff.splitFileName
        val suffix = if (ext.nonEmpty) s".$ext" else ""
        ff.sibling(s"${fn(bname)}$suffix")
      }
      mapPath(rename)
    }

    /**
      * Resolve to a sibling by mapping the extension of. If the
      * resulting extension is empty, the extension is removed from
      * `file`.
      */
    def mapExtension(fn: String => String): File = {
      def rename(ff: File): File = {
        val (bname, ext) = ff.splitFileName
        val newExt = fn(ext)
        if (newExt.isEmpty) ff.sibling(bname)
        else ff.sibling(s"$bname.${newExt}")
      }
      mapPath(rename)
    }

    def readPassword: Option[Array[Char]] = f match {
      case RegularFile(_) => f.lines.headOption.map(_.toCharArray)
      case _ => None
    }

    def ||(other: File): Option[File] =
      existing orElse other.existing

    def existing: Option[File] = if (f.exists) Some(f) else None

    /** Like {{isParentOf}} but works for non-existing files (filenames) */
    def parentOf(c: File): Boolean =
      c.path startsWith f.path

    /** Like {{isChildOf}} but works for non-existing files (filenames) */
    def childOf(p: File): Boolean =
      p.parentOf(f)

    /** Writes text by first writing it to a temporary file and then
      * moving it to current file */
    def writeMove(text: String, parent: Option[File] = None)
      (implicit openOptions: File.OpenOptions = File.OpenOptions.default, codec: Codec) = {
      val temp = File.newTemporaryFile("chee", "tmp", parent)
      temp < text
      temp.moveTo(f, overwrite = true)
    }
    def ==>>:(text: String) = writeMove(text)

    /** Append a number to the file until it does not exist. */
    def makeNonExisting(max: Int = 900, exists: File => Boolean = _.exists): Option[File] = {
      def nameMapper(n: Int)(basename: String): String = basename match {
        case NonExistingRegex(prefix, num) => prefix + (num.toInt + 1)
        case _ => basename + "-" + n
      }

      @annotation.tailrec
      def asNonExistent(f: File, n: Int = 1): Option[File] = f match {
        case _ if n >= max =>
          None
        case _ if exists(f) =>
          asNonExistent(f.mapBaseName(nameMapper(n)), n+1)
        case _ => Some(f)
      }
      asNonExistent(f)
    }

    def mimeType: Option[MimeType] = {
      def mime(os: Option[String]): MimeType =
        os.map(new MimeType(_).normalize).getOrElse(unknownMimeType)

      lazy val first = mime(Option(mimeTypesMap.getContentType(f.toJava)))
      lazy val second = mime(Option(URLConnection.guessContentTypeFromName(f.name)))

      lazy val ct = first.orElse(second).orElse {
        val m = mimeUtil.getMimeTypes(f.toJava)
        mime {
          if (m.size > 0) Some(m.iterator.next.toString)
          else None
        }
      }
      Try(ct).toOption
    }
  }

  private val unknownMimeType = new MimeType("application", "octet-stream")

  implicit class MimeTypeOps(mt: MimeType) {
    def isUnknown: Boolean = normalize.getBaseType == unknownMimeType.getBaseType
    def orElse(other: MimeType): MimeType = if (isUnknown) other else mt
    def normalize: MimeType =
      if (!mt.getBaseType.contains("unknown")) mt
      else unknownMimeType
  }

  implicit class UrlOpts(url: URL) {
    def downloadIn(dir: File, overwrite: Boolean = false): File = {
      val targetFile = Paths.get(url.getPath).getFileName
      val target = dir / targetFile.toString
      if (target.exists && overwrite) {
        target.delete()
      }
      if (!target.exists) {
        val conn = url.openConnection()
        conn.connect()
        val in = conn.getInputStream
        Files.copy(in, target.path)
        in.close()
      }
      target
    }

    def contentAsString: String = {
      io.Source.fromURL(url).getLines.mkString("\n")
    }

    def fileName: String =
      Paths.get(url.getPath).getFileName.toString

  }
}
