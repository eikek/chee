package chee

import chee.crypto.{ Algorithm, CheeCrypt, FileProcessor }
import chee.query.SqliteBackend
import org.bouncycastle.openpgp.{ PGPPrivateKey, PGPPublicKey }
import scala.util.Try
import chee.properties._
import chee.properties.MapGet._
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.nio._
import better.files._

object Processing {

  // -- image procesing ----------------------------------------------------------------------------

  private val image: MapGet[Option[Image]] = existingPath.map {
    case Some(f) => Try(Image.fromPath(f.path)).toOption
    case _ => None
  }

  val targetWidth = Ident.width.in("target")
  val targetHeight = Ident.height.in("target")

  private def setTargetSize(size: Size): MapGet[Unit] =
    modify(m => m +
      (targetWidth -> size.width.toString) +
      (targetHeight -> size.height.toString))

  private val removeTargetSize =
    modify(lm => lm remove targetWidth remove targetHeight)


  private def makeOut(size: Size, outFile: MapGet[File]): MapGet[File] =
    for {
      _ <- setTargetSize(size)
      f <- outFile
      _ <- removeTargetSize
    } yield f

  private val originProps = Ident.imageProperties.filterNot(id => id == Ident.width || id == Ident.height)

  def originMapping: Ident => Ident = id => id.in("origin")

  private def result(out: File): MapGet[Unit] =
    modify { omap =>
      val map = originProps.foldLeft(LazyMap.fromFile(out)) { (m, id) =>
        m addVirtual(VirtualProperty.defaults.alias(id -> originMapping(id)))
      }
      map ++ omap.mapIdents(originMapping)
    }

  private def processImage(outFile: MapGet[File])(p: Image => Image): MapGet[Boolean] =
    outFile.flatMap { out =>
      val success = result(out).map(_ => true)
      if (out.exists) success
      else image.flatMap {
        case Some(img) =>
          out.parent.createDirectories()
          p(img).output(out.path)(JpegWriter())
          success
        case None =>
          set(LazyMap()).map(_ => false)
      }
    }

  def cover(size: Size, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.FastScale): MapGet[Boolean] =
    processImage(makeOut(size, outFile)) { img =>
      img.cover(size.width, size.height)
    }

  def scaleTo(size: Size, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.Bicubic): MapGet[Boolean] =
    processImage(makeOut(size, outFile)) { img =>
      img.scaleTo(size.width, size.height, method)
    }

  def scaleByFactor(factor: Double, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.Bicubic): MapGet[Boolean] =
    pair(value(Ident.width), value(Ident.height)).flatMap {
      case (Some(w), Some(h)) =>
        scaleTo(Size((w.toInt * factor).toInt, (h.toInt * factor).toInt), outFile, method)
      case _ =>
        unit(false)
    }

  def scaleMaxLen(maxlen: Int, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.Bicubic): MapGet[Boolean] =
    pair(value(Ident.width), value(Ident.height)).flatMap {
      case (Some(w), Some(h)) =>
        val max = math.max(w.toInt, h.toInt)
        if (max <= maxlen) unit(true)
        else {
          val factor = maxlen.toDouble / max
          scaleTo(Size((w.toInt * factor).toInt, (h.toInt * factor).toInt), outFile, method)
        }
      case _ =>
        unit(false)
    }


  // -- encryption ----------------------------------------------------------------------------

  val originPath = originMapping(Ident.path)

  private val originFile: MapGet[File] =
    valueForce(originPath).map(File(_))

  private def originPathIndexed(sqlite: SqliteBackend): MapGet[Boolean] =
    valueForce(originPath).map(sqlite.pathExists).map(_.get)

  private def cryptFile(outFile: MapGet[File], cf: (File, File) => Unit, skip: MapGet[Boolean]) =
    pair(existingPath.whenNot(skip), outFile).flatMap {
      case (Some(Some(in)), out) =>
        if (!out.exists) cf(in, out)
        modify { m =>
          m.add(originPath -> in.pathAsString)
           .add(Ident.path -> out.pathAsString)
        } map (_ => true)
      case _ =>
        unit(false)
    }

  /** Encrypts the input file to `outFile' and updates the `path'
    * property to the new encrypted file. The original path property
    * is saved to `origin-path'. */
  def encryptPubkey(key: PGPPublicKey, outFile: MapGet[File]): MapGet[Boolean] =
    cryptFile(outFile, FileProcessor.encryptPubkey(_, key, _), CheeCrypt.isEncrypted)

  /** Encrypts the input file to `outFile' and updates the `path'
    * property to the new encrypted file. The original path property
    * is saved to `origin-path'. */
  def encryptPassword(passphrase: Array[Char], algo: Algorithm, outFile: MapGet[File]): MapGet[Boolean] =
    cryptFile(outFile, FileProcessor.encryptSymmetric(_, _, passphrase, algo), CheeCrypt.isEncrypted)

  /** Decrypts the input file to `outFile' and updates the `path'
    * property to the new decrypted file. The original path property
    * is saved to `origin-path'. */
  def decryptPubkey(keyFile: File, pass: Array[Char], outFile: MapGet[File]): MapGet[Boolean] =
    cryptFile(outFile, FileProcessor.decryptPubkey(_, keyFile, pass, _), CheeCrypt.isNotEncrypted)

  /** Decrypts the input file to `outFile' and updates the `path'
    * property to the new decrypted file. The original path property
    * is saved to `origin-path'. */
  def decryptPassword(passphrase: Array[Char], outFile: MapGet[File]): MapGet[Boolean] =
    cryptFile(outFile, FileProcessor.decryptSymmetric(_, _, passphrase), CheeCrypt.isNotEncrypted)

  case class DecryptSecret(keyFile: File, keyPass: Array[Char])

  /** Decrypts the file either with the given password or secret key. If
    * one is not given, those files are skipped. If both are not
    * given, an exception is thrown. */
  def decryptFile(pubSecret: Option[DecryptSecret], passphrase: Option[Array[Char]], outFile: MapGet[File]): MapGet[Boolean] = {
    import CheeCrypt._
    if (pubSecret.isEmpty && passphrase.isEmpty) {
      throw UserError("Either a secret key or passphrase muste be given!")
    }
    value(VirtualProperty.idents.encrypted).flatMap {
      case Some(`passwordEncryptExtension`) if passphrase.isDefined =>
        decryptPassword(passphrase.get, outFile)
      case Some(`publicKeyEncryptExtension`) if pubSecret.isDefined =>
        val s = pubSecret.get
        decryptPubkey(s.keyFile, s.keyPass, outFile)
      case _ => unit(false)
    }
  }

  /** Postprocess encryption/decryption.
    *
    * Deletes the old file. If the old file is indexed, its path
    * property is updated to reflect the new existing file. */
  def cryptInplacePostProcess(sqlite: SqliteBackend): MapGet[Boolean] =
    pair(pair(path, originFile), originPathIndexed(sqlite)).flatMap {
      case ((newFile, oldFile), indexed) if newFile.exists =>
        if (indexed) {
          modify { m =>
            val (next, success) = sqlite.updateOne(m, unit(Seq(Ident.path)), Ident.path -> originPath).get
            if (success && oldFile.exists) oldFile.delete()
            next.add(originPath -> oldFile.pathAsString)
          } map (_ => true)
        } else {
          if (oldFile.exists) oldFile.delete()
          unit(true)
        }
      case _ =>
        unit(false)
    }

}
