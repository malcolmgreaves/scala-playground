package merkle

object MerkleTreeApp {
  def main(args: Array[String]) {
    val data1 = List(1, 2, 3)
    val merkle1 = MerkleTree.create(data1)
    val hash1 = merkle1.hash

    val data2 = List(3, 2, 1).reverse
    val merkle2 = MerkleTree.create(data2)
    val hash2 = merkle2.hash

    val data3 = List(1, 2, 3, 4)
    val merkle3 = MerkleTree.create(data3)
    val hash3 = merkle3.hash

    println("Hash of tree #1 and #2 should match")
    println("===================================")
    println(s"Hash of tree #1: ${hash1.mkString(" ")}")
    println(s"Hash of tree #2: ${hash2.mkString(" ")}")

    println()

    println("This hash should be different")
    println("=============================")
    println(s"Hash of tree #3: ${hash3.mkString(" ")}")
  }
}
