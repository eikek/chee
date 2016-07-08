package chee.cli

import better.files._
import chee.CheeApi.{ DecryptSettings, FileSettings }
import chee.crypto.CryptMethod
import com.typesafe.config.Config

// A collection of cli options that are shared among some
// commands. Parsers can mix in the traits and add desired options.
//
// For convenience options accept a function that is used to implement
// an action using a default case class for holding the
// options. However, it is set to the identity function so parsers can
// specify their own action.

trait Options {
  def noAction[C, F]: (C, F) => C = (c, _) => c
}

trait LsOptions[C] extends Options { self: CheeOptionParser[C] =>
  import LsOptions.Opts

  def file(a: (C, Opts => Opts) => C = noAction) =
    opt[File]('f', "file") optional() action { (f, c) =>
      a(c, _.copy(directory = Some(f)))
    } textW ("A directory to search instead of the index.")

  def recursive(a: (C, Opts => Opts) => C = noAction, text: String) =
    opt[Unit]('r', "recursive") optional() action { (_, c) =>
      a(c, _.copy(recursive = true))
    } textW (text)

  def indexed(a: (C, Opts => Opts) => C = noAction) =
    opt[Boolean]('i', "indexed") action { (b, c) =>
      a(c, _.copy(indexed = Some(b)))
    } textW ("Find indexed or not indexed files. Only applicable if `-f' is "+
      "specified.")

  def all(a: (C, Opts => Opts) => C = noAction, text: String) =
    opt[Unit]('a', "all") optional() action { (_, c) =>
      a(c, _.copy(all = true))
    } textW (text)

  def skip(a: (C, Opts => Opts) => C = noAction) =
    opt[Int]("skip") valueName ("<n>") action { (n, c) =>
      a(c, _.copy(skip = Some(n)))
    } textW ("Drop the first n items. This is applied before `first'.")

  def first(a: (C, Opts => Opts) => C = noAction) =
    opt[Int]("first") valueName("<n>") optional() action { (n, c) =>
      a(c, _.copy(first = Some(n)))
    } text ("Limit output to the first n items.")


  def queryArg(a: (C, Opts => Opts) => C = noAction) =
    arg[String]("<query>") optional() unbounded() action { (q, c) =>
      a(c, _.appendQuery(q))
    } textW ("The query string. See the manual page about queries for" +
      " more information.")

  def queryOpt(a: (C, Opts => Opts) => C = noAction) =
    opt[String]('q', "query") optional() action { (q, c) =>
      a(c, _.appendQuery(q))
    } textW ("The query string. See the manual page about queries for" +
      " more information.")

  def addLsOptions(a: (C, Opts => Opts) => C, title: Option[String] = Some("\nFind options:")): Unit = {
    title.foreach(noteW)
    file(a)
    recursive(a, "Find files recursively. Only applicable if `-f` is specified.")
    indexed(a)
    all(a, "When used with `-f', ignore the default query, otherwise select non-existing files.")
    skip(a)
    first(a)
  }

  def addFileSettings(a: (C, FileSettings => FileSettings) => C, title: Option[String] = Some("\nFind options:")): Unit = {
    title.foreach(noteW)
    val xa: (C, Opts => Opts) => C =
      (c, f) => a(c, fs => f(Opts(fs)).toFileSettings)

    recursive(xa, "Find files recursively.")
    all(xa, "Ignore the default query that only selects images and video files,")
    skip(xa)
    first(xa)
    queryOpt(xa)
  }

  def addQuery(a: (C, Opts => Opts) => C, title: Option[String] = Some("")): Unit = {
    title.foreach(noteW)
    queryArg(a)
  }
}

object LsOptions {
  case class Opts(
    directory: Option[File] = None,
    recursive: Boolean = false,
    all: Boolean = false,
    skip: Option[Int] = None,
    first: Option[Int] = None,
    indexed: Option[Boolean] = None,
    query: String = "") {
    def appendQuery(q: String) = copy(query = query +" "+ q)
    def toFileSettings = FileSettings(recursive, all, query, skip, first)
  }
  object Opts {
    def apply(fs: FileSettings): Opts = Opts(None, fs.recursive, fs.all, fs.skip, fs.first, None, fs.query)
  }
}

trait ProcessingOptions[C] extends Options { self: CheeOptionParser[C] =>
  import ProcessingOptions.Opts

  def concurrent(a: (C, Opts => Opts) => C = noAction) =
    opt[Unit]('c', "concurrent") action { (_, c) =>
      a(c, _.copy(parallel = true))
    } text("Process files concurrently.")

  def pattern(a: (C, Opts => Opts) => C = noAction) =
    opt[String]('p', "pattern") action { (p, c) =>
      a(c, _.copy(pattern = Some(p)))
    } textW ("The format pattern used to print the result to stdout.")

  def outDir(a: (C, Opts => Opts) => C = noAction) =
    opt[File]('o', "outdir") action { (d, c) =>
      a(c, _.copy(outdir = Some(d)))
    } text ("The directory to place generated images.") validate { f =>
      if (f.exists && !f.isDirectory) failure(s"${f.path} is an existing file")
      else success
    }

  def nameformat(a: (C, Opts => Opts) => C = noAction) =
    opt[String]("nameformat") action { (f, c) =>
      a(c, _.copy(nameformat = Some(f)))
    } textW ("The format pattern used to create the target file name. It is"+
      " evaluated with the properties of the original file with"+
      " `width' and `height' replaced by the desired target values.")

  def addProcessingOptions(a: (C, Opts => Opts) => C, title: Option[String] = Some("\nProcessing options")) = {
    title.foreach(noteW)
    concurrent(a)
    pattern(a)
    outDir(a)
    nameformat(a)
  }
}

object ProcessingOptions {
  case class Opts(
    pattern: Option[String] = None,
    parallel: Boolean = false,
    outdir: Option[File] = None,
    nameformat: Option[String] = None
  )
}

trait CryptOptions[C] extends Options { self: CheeOptionParser[C] =>

  import CryptOptions.Opts

  def enable(a: (C, Opts => Opts) => C = noAction) =
    opt[Unit]('d', "decrypt") action { (_, c) =>
      a(c, _.copy(enable = true))
    } text ("Enable transparent decryption.")

  def cryptMethod(a: (C, Opts => Opts) => C = noAction) =
    opt[CryptMethod]("method") valueName("password|pubkey") action { (m, c) =>
      a(c, _.copy(cryptMethod = Some(m)))
    } textW ("The encryption method: either pubkey or password. The default value "+
      "is taken from the config file or is `pubkey'.")

  def passPrompt(a: (C, Opts => Opts) => C = noAction) =
    opt[Unit]('W', "passprompt") action { (_, c) =>
      a(c, _.copy(passPrompt = true))
    } textW ("Always prompt for a passphrase. Do not use the default-passphrase-file "+
      "from in the config file. Only applicable when password-based encryption is used.")

  def keyFile(what: String, a: (C, Opts => Opts) => C = noAction) =
    opt[File]("key-file") valueName("<file>") action { (f, c) =>
      a(c, _.copy(keyFile = Some(f)))
    } textW (s"The file containing the $what." +
      (if (what.startsWith("public")) " A key-id must also be specified." else "") +
      " The openpgp formats (ascii and binary) can be used.")

  def keyId(a: (C, Opts => Opts) => C = noAction) =
    opt[String]("key-id") action { (k, c) =>
      a(c, _.copy(keyId = Some(k)))
    } textW ("A key id matching a public key in the `key-file'. Can be part"+
      " of the user-id or key-id and must uniquely identify a key.")

  def keyPass(a: (C, Opts => Opts) => C = noAction) =
    opt[File]("secret-key-pass") valueName("<file>") action { (p, c) =>
      a(c, _.copy(secretKeyPass = Some(p)))
    } textW ("The passphrase file to access the private key. If not specified, it"+
      " is prompted for.")

  def passphrase(a: (C, Opts => Opts) => C = noAction) =
    opt[File]("passphrase") valueName("<file>") action { (p, c) =>
      a(c, _.copy(passphrase = Some(p)))
    } textW ("Specify a passphrase file to use for password-based encryption. The"+
      " `-W' option overrides this.")

  def addEncryptOptions(a: (C, Opts => Opts) => C, title: Option[String] = Some("\nEncrypt options:")): Unit = {
    title.foreach(noteW)
    cryptMethod(a)
    passPrompt(a)
    keyFile("public key", a)
    keyId(a)
    passphrase(a)
  }

  def addDecryptOptions(a: (C, Opts => Opts) => C, title: Option[String] = Some("\nDecrypt options:"), enableOpt: Boolean = false): Unit = {
    title.foreach(noteW)
    if (enableOpt) {
      enable(a)
    }
    cryptMethod(a)
    passPrompt(a)
    keyFile("secret key", a)
    keyPass(a)
    passphrase(a)
  }
}

object CryptOptions {
  case class Opts(
    enable: Boolean = false,
    cryptMethod: Option[CryptMethod] = None,
    keyFile: Option[File] = None,
    keyId: Option[String] = None,
    passPrompt: Boolean = false,
    secretKeyPass: Option[File] = None,
    passphrase: Option[File] = None
  ) {

    def toSettings(cfg: Config): DecryptSettings =
      DecryptSettings(
      if (enable) CryptCommand.pubKeySecret(cfg, this) else None,
      if (enable) CryptCommand.passphraseSecret(cfg, this) else None)
  }
}
