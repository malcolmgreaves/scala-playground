package merkle

import java.security.MessageDigest

object MerkleTreeApp {
  def main(args: Array[String]) {
    val sha1Hash = MessageDigest.getInstance("SHA-1")
    val data = List(1)

    val merkle = MerkleTree.create(sha1Hash)(data)
    println(merkle)
  }
}
