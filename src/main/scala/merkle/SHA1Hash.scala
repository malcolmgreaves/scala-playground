package merkle

import java.security.MessageDigest

object SHA1Hash extends CryptographicHash {
  private val hasher = MessageDigest.getInstance("SHA-1")

  override def hash(byteSequence: Seq[Byte]): Vector[Byte] =
    hasher.digest(byteSequence.toArray).toVector
}
