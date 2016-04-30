package chee

import better.files._

package object cli {

  import chee.crypto.CryptMethod
  import com.typesafe.config.Config
  import better.files.File.LinkOptions

  def userError(s: String) = chee.UserError(s)

  def promptPassphrase(prompt: String = "Passphrase: "): Array[Char] = {
    def equals(a1: Array[Char], a2: Array[Char]): Boolean =
      a1.zip(a2).foldLeft(true) { case (r, (e1, e2)) => r && (e1 == e2) }

    print(prompt)
    val p1 = System.console().readPassword()
    print("Retype: ")
    val p2 = System.console().readPassword()
    if (p1.nonEmpty && equals(p1, p2)) p1
    else userError("Passphrases did not match or empty passphrase specified!")
  }

  def wrapLines(len: Int)(text: String): String = {
    def index(idx: (String, Int) => Int): String => Int = s =>
      idx(s, '\n') match { //wrap at newline if present
        case -1 => idx(s, ' ') // otherwise wrap at spaces
        case n => n
      }
    val searchBack: String => Int = index(_.lastIndexOf(_, len))
    val searchForward: String => Int = index(_.indexOf(_))
    @scala.annotation.tailrec
    def loop(start: Int, sepIndex: String => Int, result: StringBuilder): String =
      text.substring(start) match {
        case s if s.length() <= len => (result append s).toString
        case s => sepIndex(s) match {
          case -1 =>
            if (sepIndex eq searchForward) (result append s).toString
            else loop(start, searchForward, result)
          case n =>
            loop(start + n + 1, searchBack, result append s.substring(0, n) append '\n')
        }
      }

    loop(0, searchBack, new StringBuilder)
  }

  implicit val _readFile: scopt.Read[File] =
    scopt.Read.reads(File(_))

  private val numberRegex = """([0-9]+)""".r
  private val sizeRegex = """([0-9]+)x([0-9]+)""".r

  implicit val _readWitdhxHeight: scopt.Read[Size] =
    scopt.Read.reads(str => str match {
      case numberRegex(n) => Size(n.toInt)
      case sizeRegex(w, h) => Size(w.toInt, h.toInt)
      case _ => UserError(s"Invalid size string. Either a single number or `<width>x<height>' is allowed.")
    })

  implicit val _readCryptMethod: scopt.Read[CryptMethod] =
    scopt.Read.reads(v => v.toLowerCase() match {
      case "password" => CryptMethod.Password
      case "pubkey" => CryptMethod.Pubkey
      case _ => userError(s"Allowed are: password or pubkey")
    })

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

  }
}
