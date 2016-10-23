package chee

import better.files._
import chee.crypto.CryptMethod
import chee.FileOps.Result

package cli {
  case class ResultCount private (counter: Map[Result, Int]) {
    def inc(r: Result): ResultCount = {
      ResultCount(counter + (r -> (get(r) + 1)))
    }

    def get(r: Result): Int = counter(r)
  }
  object ResultCount {
    val empty = ResultCount(Map.empty withDefaultValue 0)
  }
}

package object cli {

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
    scopt.Read.reads(s => java.nio.file.Paths.get(s).toAbsolutePath)

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

}
