package merkle

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.math

object MerkleTree {
  sealed trait Tree[+A] { def hash: Vector[Byte] }

  case class Node[+A](hash: Vector[Byte], left: Tree[A], right: Tree[A]) extends Tree[A]

  case class Leaf[+A](hash: Vector[Byte], data: Option[A]) extends Tree[A]

  case object EmptyLeaf extends Tree[Nothing] {
    override val hash: Vector[Byte] = Vector.empty[Byte]
  }

  private def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {
    def log2(x: Double): Double = math.log(x) / math.log(2)

    math.ceil(log2(numberOfDataBlocks)).toInt
  }

  private def dataHashPairToLeaf[A](dataHashPair: (Vector[Byte], A)): Leaf[A] =
    Leaf(dataHashPair._1, Some(dataHashPair._2))

  @tailrec
  private def makeTree[A](trees: Seq[Tree[A]], hashFunction: CryptographicHash): Tree[A] = {
    def createParents[A](treePair: Seq[Tree[A]]): Tree[A] = {
      val child1 +: child2 +: _ = treePair
      val newHash = hashFunction.hash((child1.hash ++ child2.hash))
      Node(newHash, child1, child2)
    }

    if (trees.size == 0) {
      EmptyLeaf
    } else if (trees.size == 1) {
      trees.head
    } else {
      makeTree(trees.grouped(2).map(createParents).toSeq, hashFunction)
    }
  }

  def create[A](dataBlocks: Seq[A])(implicit hashFunction: CryptographicHash = SHA1Hash): Tree[A] = {
    val level = calculateRequiredLevel(dataBlocks.size)

    val hashedDataBlocks =
      dataBlocks.map(data => hashFunction.hash(data.toString.getBytes))

    val paddingNeeded = math.pow(2, level).toInt - dataBlocks.size
    val padding = Seq.fill(paddingNeeded)(EmptyLeaf)

    val leaves = hashedDataBlocks.zip(dataBlocks).map(dataHashPairToLeaf) ++ padding

    makeTree(leaves, hashFunction)
  }
}
