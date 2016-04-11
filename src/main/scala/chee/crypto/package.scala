package chee

package crypto {

  import org.bouncycastle.bcpg.{SymmetricKeyAlgorithmTags => Tags}

  case class Algorithm(tag: Int)
  object Algorithm {
    val AES128 = Algorithm(Tags.AES_128)
    val AES256 = Algorithm(Tags.AES_256)
    val Twofish = Algorithm(Tags.TWOFISH)
    val Blowfish = Algorithm(Tags.BLOWFISH)
    val DES3 = Algorithm(Tags.TRIPLE_DES)
    val Idea = Algorithm(Tags.IDEA)
    val CAST5 = Algorithm(Tags.CAST5)
    val Camellia128 = Algorithm(Tags.CAMELLIA_128)
    val Camellia256 = Algorithm(Tags.CAMELLIA_256)

    val all = Map(
      "AES128" -> AES128,
      "AES256" -> AES256,
      "TWOFISH" -> Twofish,
      "BLOWFISH" -> Blowfish,
      "DES3" -> DES3,
      "IDEA" -> Idea,
      "CAST5" -> CAST5,
      "CAMELLIA128" -> Camellia128,
      "CAMELLIA256" -> Camellia256
    )

    def find(algo: String): Option[Algorithm] = all.get(algo.toUpperCase)
  }

  sealed trait CryptMethod
  object CryptMethod {
    case object Pubkey extends CryptMethod
    case object Password extends CryptMethod
  }

}

package object crypto {

  import java.security.Security
  import org.bouncycastle.jce.provider.BouncyCastleProvider

  Security.addProvider(new BouncyCastleProvider)

  val bcProvider = "BC"
}
