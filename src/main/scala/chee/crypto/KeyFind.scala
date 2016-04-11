package chee.crypto

import better.files._
import chee.UserError
import java.io.InputStream
import org.bouncycastle.openpgp.operator.jcajce.JcePBESecretKeyDecryptorBuilder
import org.bouncycastle.openpgp.{ PGPPrivateKey, PGPPublicKey, PGPPublicKeyRing, PGPSecretKey, PGPSecretKeyRing, PGPSecretKeyRingCollection }
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator
import org.bouncycastle.openpgp.{ PGPPublicKeyRingCollection, PGPUtil }
import scala.util.{ Success, Try }

object KeyFind {
  import scala.collection.JavaConverters._

  private def pubkeyOf(id: String)(k: PGPPublicKeyRing): Boolean = {
    val pk = k.getPublicKey
    pk.getUserIDs.asScala.map(_.toString).filter(_ contains id).nonEmpty ||
      "%x".format(pk.getKeyID).endsWith(id.toLowerCase)
  }

  private def seckeyOf(id: String)(k: PGPSecretKeyRing): Boolean = {
    val sk = k.getSecretKey
    sk.getUserIDs.asScala.map(_.toString).filter(_ contains id).nonEmpty ||
      "%x".format(sk.getKeyID).endsWith(id.toLowerCase)
  }

  def findPublicKey(in: File, id: String): PGPPublicKey = {
    val inStream = in.newInputStream
    try {
      val pgpPub = new PGPPublicKeyRingCollection(
        PGPUtil.getDecoderStream(inStream), new JcaKeyFingerprintCalculator())

      pgpPub.asScala.filter(pubkeyOf(id)).toList match {
        case k :: Nil => k.getPublicKey
        case _ => UserError(s"No public key for $id")
      }
    } finally {
      inStream.close()
    }
  }

  def extractPrivateKey(skey: PGPSecretKey, pass: Array[Char]): PGPPrivateKey = {
    skey.extractPrivateKey(new JcePBESecretKeyDecryptorBuilder().setProvider(bcProvider).build(pass))
  }

  def findSecretKeyByKeyID(in: File, keyID: Long): PGPSecretKey = {
    val inStream = in.newInputStream
    try {
      val pgpSec = new PGPSecretKeyRingCollection(
        PGPUtil.getDecoderStream(inStream), new JcaKeyFingerprintCalculator())
      Option(pgpSec.getSecretKey(keyID)) match {
        case Some(skey) => skey
        case _ => UserError(s"No secret key with keyID $keyID")
      }
    } finally {
      inStream.close()
    }
  }

  def findSecretKey(in: File, id: String): PGPSecretKey = {
    val inStream = in.newInputStream
    try {
      findSecretKey(inStream, id)
    } finally {
      inStream.close()
    }
  }

  def findSecretKey(inStream: InputStream, id: String): PGPSecretKey = {
    val pgpSec = new PGPSecretKeyRingCollection(
      PGPUtil.getDecoderStream(inStream), new JcaKeyFingerprintCalculator())

    pgpSec.asScala.filter(seckeyOf(id)).toList match {
      case k :: Nil => k.getSecretKey
      case _ => UserError(s"No secret key for $id")
    }
  }

  def parseLong(id: String): Try[Long] =
    Try(java.lang.Long.parseLong(id, 16))
}
