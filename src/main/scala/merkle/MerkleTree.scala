package merkle

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.math

object MerkleTree {
  sealed trait Tree[+A] {
    def hash: Vector[Byte]
  }

  case class Node[+A](hash: Vector[Byte], left: Tree[A], right: Tree[A]) extends Tree[A]

  case class Leaf[+A](data: Option[A], hash: Vector[Byte]) extends Tree[A]

  def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {
    def log2(x: Double): Double = math.log(x) / math.log(2)

    math.ceil(log2(numberOfDataBlocks)).toInt
  }

  def dataHashPairToLeaf[A](dataHashPair: (A, Vector[Byte])): Leaf[A] =
    Leaf(Some(dataHashPair._1), dataHashPair._2)

  @tailrec
  def makeTree[A](trees: List[Tree[A]]): Tree[A] = {
    def createParents[A](treePair: List[Tree[A]]): Tree[A] = {
      val child1 :: child2 :: _ = treePair
      Node(child1.hash ++ child2.hash, child1, child2)
    }

    if (trees.size == 1) {
      trees.head
    } else {
      makeTree(trees.grouped(2).map(createParents).toList)
    }
  }

  def create[A](hashFunction: MessageDigest)(dataBlocks: List[A]): Tree[A] = {
    val level = calculateRequiredLevel(dataBlocks.size)

    val hashedDataBlocks =
      dataBlocks.map(data => hashFunction.digest(data.toString.getBytes).toVector)

    val paddingNeeded = math.pow(2, level).toInt - dataBlocks.size
    val padding = List.fill(paddingNeeded)(Leaf(None, Vector.empty[Byte]))

    val leaves = dataBlocks.zip(hashedDataBlocks).map(dataHashPairToLeaf) ++ padding

    makeTree(leaves)
  }
}
