package chee

import better.files._
import CheeApi.PubkeySecret
import chee.crypto.CheeCrypt
import chee.metadata.MetadataFile
import chee.properties._
import chee.util.files._
import MapGet._
import chee.query.Index
import chee.query.Index.UpdateParam
import com.typesafe.scalalogging.LazyLogging
import java.nio.file.Files
import scala.util.{ Failure, Success, Try }

/** Functions to work with files and index. */
object FileOps extends LazyLogging {

  sealed trait Result
  object Result {
    case object Skipped extends Result
    case object Added extends Result
    case object Deleted extends Result
    case object Updated extends Result
    case object Duplicate extends Result
    case object Ok extends Result
    case class Error(msg: String) extends Result
  }

  case class CryptSettings(pubSecret: Option[PubkeySecret], passphrase: Option[Array[Char]], outFile: MapGet[File], mf: MetadataFile)
  object CryptSettings {
    def apply(pass: Array[Char], out: MapGet[File]): CryptSettings =
      CryptSettings(None, Some(pass), out, MetadataFile.empty)
  }

  /** Check whether {{data}} is indexed by testing its path property */
  def isPathIndexed(index: Index): MapGet[Try[Boolean]] =
    index.exists(Condition.lookup(Comp.Eq, Ident.path, Index.realPath))

  def isFileIndexed(index: Index, file: MapGet[File]): MapGet[Try[Boolean]] =
    file.map { f =>
      index.exists(Prop(Comp.Eq, Ident.path -> f.pathAsString))
    }

  val isPathEncrypted: MapGet[Boolean] = CheeCrypt.isEncrypted

  def isEncryptedPathIndexed(index: Index): MapGet[Try[Option[String]]] = {
    def toOption(m: MapGet[File]): MapGet[Try[Option[String]]] =
      isFileIndexed(index, m).sflatMap {
        case true => m.map(p => Some(p.pathAsString))
        case false => unit(None)
      }
    toOption(CheeCrypt.passwordEncryptedFile).tflatMap {
      case f@Some(_) => success(f)
      case None => toOption(CheeCrypt.publicKeyEncryptedFile)
    }
  }

  def isNonEncryptedPathIndexed(index: Index): MapGet[Try[Option[String]]] =
    pair(MapGet.path, CheeCrypt.encryptedExtension(Ident.path)).map {
      case (path, Some(ext)) =>
        val p = path.mapFileName(f => f.substring(0, f.length - ext.length - 1)).pathAsString
        index.exists(Prop(Comp.Eq, Ident.path -> p)).map(b => if (b) Some(p) else None)
      case _ => Success(None)
    }


  def updatePath(index: Index, newPath: String, wherePath: Option[String] = None): MapGet[Try[Result]] =
    unit(wherePath).orElse(value(Ident.path)).map {
      case Some(p) =>
        val data = LazyMap(Ident.path -> newPath, Ident.path.in("db") -> p)
        val r = index.updateOne(UpdateParam.updatePathWith(Ident.path.in("db"))).result(data)
        r.map(_ => Result.Updated)
      case None =>
        Failed("No key for lookup")
    }

  def preprocess(filter: MapGet[Boolean], mf: MetadataFile): MapGet[Unit] =
    tuple3(valueForce(Ident.path), valueForce(Ident.filename), value(Ident.lastModified)).flatMap {
      case (realPath, realName, lastmod) =>
        filter.flatMap {
          case false => unit(())
          case true =>
            pair(value(Ident.location), valueForce(Ident.path)).flatMap {
              case (loc, newPath) =>
                set(LazyMap.fromFile(File(newPath), mf)
                  .add(Ident.filename -> realName)
                  .add(lastmod.map(Property(Ident.lastModified, _))) // TODO really?? good when adding, but not when updating
                  .add(loc.map(Property(Ident.location, _)))
                  .add(Index.realPathIdent -> realPath))
            }
        }
    }

  def decrypt(crypt: CryptSettings): MapGet[Unit] = {
    logger.trace(s"Decrypt with $crypt")
    preprocess(Processing.decryptFile(crypt.pubSecret, crypt.passphrase, crypt.outFile), crypt.mf).flatMap { _ =>
      MapGet.valueForce(Ident.filename).flatMap { name =>
        add(Ident.filename -> File(name).stripExtension.name)
      }
    }
  }

  def addPlain(index: Index, added: DateTime): MapGet[Try[Result]] =
    for {
      _ <- MapGet.add(Extraction.added(added))
      r <- index.insertOne
    } yield r.map(b => if (b) Result.Added else Result.Skipped)

  def addEncrypted(index: Index, added: DateTime, crypt: Option[CryptSettings]): MapGet[Try[Result]] = {
    logger.trace("add encrypted file")
    crypt.map { c =>
      decrypt(c).flatMap(_ => addPlain(index, added))
    } getOrElse success(Result.Skipped)
  }

  /** Add a new file to the index.
    *
    * If {{path}} is already in the index, skip this file. Otherwise,
    * check if the encrypted or decrypted version is indexed. If so,
    * just update the path. Otherwise add the new file (decrypting
    * first, if it is an encrypted file.*/
  def addToIndex(index: Index, added: DateTime, crypt: Option[CryptSettings]): MapGet[Try[Result]] = {
    isPathIndexed(index).tflatMap {
      case true => success(Result.Skipped)
      case false =>
        isPathEncrypted.flatMap { enc =>
          isNonEncryptedPathIndexed(index).orElse(isEncryptedPathIndexed(index)).tflatMap {
            case Some(path) =>
              updatePath(index, path)
            case None =>
              if (enc) addEncrypted(index, added, crypt)
              else addPlain(index, added)
            }
        }
    }
  }

  /** Check whether the checksum of the file matches the checksum of the
    * entry at {{wherePath}} or the entry of the given file. */
  def checksumMatch(index: Index, wherePath: Option[String] = None): MapGet[Try[Boolean]] =
    pair(unit(wherePath).orElse(Index.realPath), value(Ident.checksum)).map {
      case (Some(p), Some(sum)) =>
        index.findOne(Prop(Comp.Eq, Ident.path -> p)).flatMap {
          case Some(m) =>
            val dbsum = valueForce(Ident.checksum).result(m)
            Success(sum == dbsum)
          case None => Failed(s"Cannot compare checksums. No entry found: $p")
        }
      case (_, None) => Failed("No checksum available")
      case _ => Success(false)
    }

  /** Check whether the given file is indexed via checksum. */
  def checksumExists(index: Index): MapGet[Try[Boolean]] =
    valueForce(Ident.checksum).map { cs =>
      index.exists(Prop(Comp.Eq, Ident.checksum -> cs))
    }


  def updateIndex(index: Index, where: MapGet[Condition], except: Seq[Ident] = Seq(Ident.added)): MapGet[Try[Result]] =
    index.updateOne(UpdateParam(where = where, columns = MapGet.idents(false).map(_ diff except))).map(_.map {
      case true => Result.Updated
      case false => Result.Skipped
    })


  private def syncChecksumUpdate(index: Index, wherePath: Option[String], newPath: Option[String], alwaysUpdate: Boolean): MapGet[Try[Result]] = {
    // select entry to update either by given path or lookup itself
    val update = updateIndex(index, Condition.lookup(Comp.Eq, Ident.path, unit(wherePath).orElse(value(Ident.path))))
    if (alwaysUpdate) update
    else checksumMatch(index, wherePath).tflatMap {
      case true =>
        // checksum matches, so update path if given
        newPath.map { p => updatePath(index, p, wherePath) } getOrElse success(Result.Ok)
      case false =>
        update
    }
  }

  /** Synchronise a file with the index.
    *
    * This is almost like {{addToIndex}} but it verifies checksums. If
    * checksums differ, but a file is already in the index, the index
    * is updated with properties of the new file. If checksum matches
    * then either nothing happens or the path is updated if it has
    * changed. If {{awlaysUpdate}} is true, checksums are not checked
    * and the entry is updated with properties of the new file.*/
  def syncWithIndex(index: Index, added: DateTime, crypt: Option[CryptSettings], alwaysUpdate: Boolean = false): MapGet[Try[Result]] =
    MapGet.path.flatMap { p =>
      isPathIndexed(index).tflatMap {
        case true =>
          val update = syncChecksumUpdate(index, None, None, alwaysUpdate)
          doDecrypted(update, crypt)
        case false =>
          isPathEncrypted.flatMap { enc =>
            isNonEncryptedPathIndexed(index).orElse(isEncryptedPathIndexed(index)).tflatMap {
              case np@Some(_) =>
                val update = syncChecksumUpdate(index, np, Some(p.pathAsString), alwaysUpdate)
                doDecrypted(update, crypt)
              case None =>
                if (enc) addEncrypted(index, added, crypt)
                else addPlain(index, added)
            }
          }
      }
    }


  def copyFile(targetDir: File, inRelative: Option[File]): MapGet[Try[Unit]] =
    existingPath.flatMap {
      case None => success(Result.Error("Source file does not exist"))
      case Some(inFile) =>
        val sub = inRelative.map(_ relativize inFile).map(_.toString) getOrElse inFile.name
        (targetDir / sub).makeNonExisting() match {
          case None => failed("Cannot make unique filename.")
          case Some(targetFile) =>
            targetFile.parent.createDirectories()
            inFile.copyTo(targetFile)
            add(Ident.path -> targetFile.pathAsString,
              Ident.location -> targetDir.pathAsString,
              Ident.path.in("origin") -> inFile.pathAsString).map(Success(_))
        }
    }

  /** Import given file into {{targetDir}} by copying it there and
    * calling {{addToIndex}}. It uses the {{location}} property of the
    * input file to calculate the relatve path that is resolved
    * against the target directory.*/
  def importFile(index: Index, added: DateTime, crypt: Option[CryptSettings],
    targetDir: File, duplicates: Boolean = false): MapGet[Try[Result]] =
  {
    val importPlain = checksumExists(index).tflatMap { indexed =>
      if (indexed && !duplicates) success(Result.Duplicate)
      else value(Ident.location).flatMap { location =>
        copyFile(targetDir, location.map(File(_))).flatMap(_ => addToIndex(index, added, crypt))
      }
    }
    doDecrypted(importPlain, crypt)
  }

  /** Delete the given file from the index if it does not exist on the
    * file system. */
  def deleteNonExistingFile(index: Index): MapGet[Try[Result]] =
    path.map { p =>
      if (p.exists) Success(Result.Skipped)
      else index.delete(Prop(Comp.Eq, Ident.path -> p.pathAsString)).map {
        case 0 => Result.Ok
        case _ => Result.Deleted
      }
    }


  private def doDecrypted(action: MapGet[Try[Result]], crypt: Option[CryptSettings]) =
    isPathEncrypted.flatMap { enc =>
      if (!enc) action
      else crypt.map { c =>
        decrypt(c).flatMap(_ => action)
      } getOrElse success(Result.Skipped)
    }

  private def failed[A](msg: String): MapGet[Try[A]] =
    unit(Failed(msg))

  private def Failed[A](msg: String): Try[A] =
    Failure(new RuntimeException(msg))

  private def success[A](a: A): MapGet[Try[A]] =
    unit(Success(a))

  private implicit class MapGetTryOps[A](m: MapGet[Try[A]]) {
    def sflatMap[B](f: A => MapGet[B]): MapGet[Try[B]] =
      m.flatMap {
        case Success(a) => f(a).map(Success(_))
        case Failure(e) => unit(Failure(e))
      }

    def tflatMap[B](f: A => MapGet[Try[B]]): MapGet[Try[B]] =
      m.flatMap {
        case Success(a) => f(a)
        case Failure(e) => unit(Failure(e))
      }
  }

  private implicit class MapGetTryOptionOps[A](m: MapGet[Try[Option[A]]]) {
    def orElse(o: MapGet[Try[Option[A]]]): MapGet[Try[Option[A]]] =
      m.tflatMap(a => if (a.isDefined) m else o)
  }
}
