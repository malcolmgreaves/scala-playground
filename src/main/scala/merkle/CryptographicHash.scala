package merkle

trait CryptographicHash {
  def hash(byteSequence: Seq[Byte]): Vector[Byte]
}
