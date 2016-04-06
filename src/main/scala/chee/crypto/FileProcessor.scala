package chee.crypto

import better.files._
import java.io.{ BufferedOutputStream, FileOutputStream }

import java.security.SecureRandom
import java.security.Security

import org.bouncycastle.bcpg.ArmoredOutputStream
import org.bouncycastle.bcpg.CompressionAlgorithmTags
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.PGPCompressedData
import org.bouncycastle.openpgp.PGPPrivateKey;
import org.bouncycastle.openpgp.PGPPublicKey;
import org.bouncycastle.openpgp.PGPPublicKeyRing;
import org.bouncycastle.openpgp.PGPPublicKeyRingCollection;
import org.bouncycastle.openpgp.PGPSecretKey;
import org.bouncycastle.openpgp.PGPSecretKeyRing;
import org.bouncycastle.openpgp.PGPSecretKeyRingCollection;
import org.bouncycastle.openpgp.PGPEncryptedData
import org.bouncycastle.openpgp.PGPEncryptedDataGenerator
import org.bouncycastle.openpgp.PGPEncryptedDataList
import org.bouncycastle.openpgp.PGPException
import org.bouncycastle.openpgp.PGPLiteralData
import org.bouncycastle.openpgp.PGPPBEEncryptedData
import org.bouncycastle.openpgp.PGPUtil
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory
import org.bouncycastle.openpgp.operator.jcajce.JcaPGPDigestCalculatorProviderBuilder
import org.bouncycastle.openpgp.operator.jcajce.JcePBEDataDecryptorFactoryBuilder
import org.bouncycastle.openpgp.operator.jcajce.JcePBEKeyEncryptionMethodGenerator
import org.bouncycastle.openpgp.operator.jcajce.JcePGPDataEncryptorBuilder
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator;
import org.bouncycastle.openpgp.operator.jcajce.JcePGPDataEncryptorBuilder;
import org.bouncycastle.openpgp.operator.jcajce.JcePublicKeyDataDecryptorFactoryBuilder;
import org.bouncycastle.openpgp.operator.jcajce.JcePublicKeyKeyEncryptionMethodGenerator;
import org.bouncycastle.util.io.Streams

object FileProcessor {
  // don't compress, because this is for already compressed files (jpg, etc)

  private def makeBuffer = Array.fill[Byte](1 << 16)(0)

  def encryptSymmetric(in: File, out: File, passPhrase: Array[Char], algo: Algorithm): Unit = {
    val encGen = new PGPEncryptedDataGenerator(
      new JcePGPDataEncryptorBuilder(algo.tag)
        .setWithIntegrityPacket(true)
        .setSecureRandom(new SecureRandom())
        .setProvider("BC"))

    encGen.addMethod(new JcePBEKeyEncryptionMethodGenerator(passPhrase).setProvider("BC"))
    pipeData(in, encGen, out)
  }

  def decryptSymmetric(in: File, out: File, passPhrase: Array[Char]): Unit = {
    val inStream = PGPUtil.getDecoderStream(in.newInputStream)
    val pgpf = new JcaPGPObjectFactory(inStream)

    val enc = pgpf.nextObject() match {
      case d: PGPEncryptedDataList => d
      case _ => pgpf.nextObject().asInstanceOf[PGPEncryptedDataList]
    }

    val pbe = enc.get(0).asInstanceOf[PGPPBEEncryptedData]

    val clear = pbe.getDataStream(new JcePBEDataDecryptorFactoryBuilder(
      new JcaPGPDigestCalculatorProviderBuilder()
        .setProvider("BC")
        .build())
      .setProvider("BC").build(passPhrase))

    val (pgpFact, ld) = {
      val fact = new JcaPGPObjectFactory(clear)
      fact.nextObject() match {
        case cd: PGPCompressedData =>
          val fact = new JcaPGPObjectFactory(cd.getDataStream)
          (fact, fact.nextObject().asInstanceOf[PGPLiteralData])
        case o =>
          (fact, o.asInstanceOf[PGPLiteralData])
      }
    }

    val unc = ld.getInputStream
    val fout = out.newOutputStream
      //new BufferedOutputStream(new FileOutputStream(ld.getFileName))
    try {
      Streams.pipeAll(unc, fout)
    } finally {
      fout.close
      unc.close
    }
    if (pbe.isIntegrityProtected()) {
      if (!pbe.verify()) {
        chee.UserError("Integrity-check failed")
      }
    }
  }

  def encryptPubkey(in: File, key: PGPPublicKey, out: File): Unit = {
    val encGen = new PGPEncryptedDataGenerator(
      new JcePGPDataEncryptorBuilder(Algorithm.CAST5.tag)
        .setWithIntegrityPacket(true)
        .setSecureRandom(new SecureRandom())
        .setProvider("BC"))

    encGen.addMethod(new JcePublicKeyKeyEncryptionMethodGenerator(key).setProvider("BC"))
    pipeData(in, encGen, out)
  }

  private def pipeData(in: File, encGen: PGPEncryptedDataGenerator, out: File): Unit = {
    val outStream = out.newOutputStream
    val cout = encGen.open(outStream, makeBuffer)
    try {
      PGPUtil.writeFileToLiteralData(cout, PGPLiteralData.BINARY, in.path.toFile(), makeBuffer)
    } finally {
      cout.close
      outStream.close
    }
  }
}
