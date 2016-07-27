package chee

import better.files._
import java.util.UUID
import chee.crypto._
import chee.query.{Index, SqliteBackend}
import chee.util.files._
import org.scalacheck.Gen
import org.scalatest.FlatSpec

trait FileLoan {
  self: FlatSpec =>

  private lazy val testDir = {
    val d = TestInfo.targetDir / "test" / "files"
    if (!d.exists) d.createDirectories()
    d
  }

  private def newFile: File = {
    val name = UUID.randomUUID().toString
    testDir / name
  }

  def deleteFile(file: File)(code: => Any): Unit = {
    info(s"Using file: $file")
    try { code } finally {
      if (file.exists && file.childOf(testDir))
       file.delete()
    }
  }

  def withNewFile(code: File => Any): Unit = {
    val file = newFile
    file.path.toFile.deleteOnExit()
    deleteFile(file)(code(file))
  }

  def withExistingFile(code: File => Any): Unit = withNewFile { f =>
    f.createIfNotExists(createParents = true)
    code(f)
  }

  def newDirectory(code: File => Any): Unit = withNewFile { f =>
    f.createDirectories()
    deleteFile(f)(code(f))
  }

  def withIndex(code: Index => Any): Unit = withNewFile { f =>
    val db = TestInfo.sampleDb.copyTo(f)
    val index = new SqliteBackend(db, None)
    code(index)
  }

  def repoIndex(code: Index => Any): Unit = withNewFile { f =>
    val db = TestInfo.sampleRepoDb.copyTo(f)
    val index = new SqliteBackend(db, Some(File("/home/eike/workspace")))
    code(index)
  }

  def withNewIndex(code: Index => Any): Unit = withExistingFile { f =>
    val index = new SqliteBackend(f, None)
    code(index)
  }

  def copyFile(in: File, target: Option[File] = None)(code: File => Any): Unit = {
    val t = target match {
      case Some(Directory(dir)) => dir / in.name
      case Some(f) => f
      case None => newFile.mapExtension(_ => in.getExtension.getOrElse(""))
    }
    in.copyTo(t)
    t.toJava.deleteOnExit()
    deleteFile(t)(code(t))
  }

  def randomImage(code: File => Any): Unit = {
    val img = TestInfo.images.randomGet
    copyFile(img)(code)
  }

  def randomImageAccept(p: File => Boolean)(code: File => Any): Unit = {
    def loop(n: Int = 0): File =
      if (n > 300) sys.error("limit exceeded. cannot find file accepting predicate")
      else {
        val img = TestInfo.images.randomGet
        if (p(img)) img
        else loop(n + 1)
      }
    copyFile(loop())(code)
  }

  def encryptFile(in: File, pass: Array[Char], target: Option[File] = None)(code: File => Any): Unit = {
    val out = target match {
      case Some(Directory(dir)) => dir / (in.mapFileName(_ + "." + CheeCrypt.passwordEncryptExtension)).name
      case Some(f) => f.mapFileName(_ + "." + CheeCrypt.passwordEncryptExtension)
      case None => (target getOrElse in).mapFileName(_ + "." + CheeCrypt.passwordEncryptExtension)
    }
    FileProcessor.encryptSymmetric(in, out, pass, Algorithm.AES256)
    deleteFile(out)(code(out))
  }

  def decryptFile(in: File, pass: Array[Char], target: Option[File] = None)(code: File => Any): Unit = {
    val out = (target, in.getExtension) match {
      case (Some(Directory(dir)), Some(CheeCrypt.passwordEncryptExtension)) =>
        dir / (in.stripExtension).name
      case (Some(Directory(dir)), _) =>
        dir / in.name
      case (Some(f), Some(CheeCrypt.passwordEncryptExtension)) =>
        f.stripExtension
      case (Some(f), _) =>
        f.mapFileName(_ + ".dec")
      case (_, Some(CheeCrypt.passwordEncryptExtension)) =>
        (target getOrElse in).stripExtension
      case _ =>
        (target getOrElse in).mapFileName(_ + ".dec")
    }
    FileProcessor.decryptSymmetric(in, out, pass)
    deleteFile(out)(code(out))
  }

  def combine[A, B](f: (A => Any) => Unit, g: (B => Any) => Unit)(code: (A, B) => Any) =
    f { a => g { b => code(a, b) } }

  def combine[A, B, C](f: (A => Any) => Unit, g: (B => Any) => Unit, h: (C => Any) => Unit)(code: (A, B, C) => Any) =
    f { a => g { b => h { c => code(a, b, c) } } }

  implicit class SeqOps[A](s: Seq[A]) {
    def randomGet: A = Gen.oneOf(s).sample.get
  }
}
