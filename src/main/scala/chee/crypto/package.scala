package chee

package crypto {

  import org.bouncycastle.bcpg.{SymmetricKeyAlgorithmTags => Tags}

  sealed trait Algorithm {
    def tag: Int
  }
  object Algorithm {
    case object AES128 extends Algorithm {
      val tag = Tags.AES_128
    }
    case object AES256 extends Algorithm {
      val tag = Tags.AES_256
    }
    case object Twofish extends Algorithm {
      val tag = Tags.TWOFISH
    }
    case object Blowfish extends Algorithm {
      val tag = Tags.BLOWFISH
    }
    case object DES3 extends Algorithm {
      val tag = Tags.TRIPLE_DES
    }
    case object IDEA extends Algorithm {
      val tag = Tags.IDEA
    }
    case object CAST5 extends Algorithm {
      val tag = Tags.CAST5
    }
  }
}

package object crypto {

  import java.security.Security
  import org.bouncycastle.jce.provider.BouncyCastleProvider

  Security.addProvider(new BouncyCastleProvider)

  val bcProvider = "BC"
}
