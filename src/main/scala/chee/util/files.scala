package chee.util

import better.files._
import better.files.File.LinkOptions
import scala.io.Codec

object files {
  object Directory {
    def unapply(f: File): Option[File] =
      if (f.isDirectory(LinkOptions.follow)) Some(f) else None
  }

  object RegularFile {
    def unapply(f: File): Option[File] =
      if (f.isRegularFile(LinkOptions.follow)) Some(f) else None
  }

  implicit class FileExt(f: File) {

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
        ff.sibling(s"${fn(bname)}.$ext")
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
  }
}
