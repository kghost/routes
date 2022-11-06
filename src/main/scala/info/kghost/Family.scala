package info.kghost

import java.math.BigInteger
import java.net.InetAddress

object BinaryTree {
  val leaf = BinaryTree(null, null)
}
case class BinaryTree(val left: BinaryTree, val right: BinaryTree) {
  def isLeaf = left == null && right == null
}
object Leaf {
  def unapply(node: BinaryTree): Boolean = node != null && node.isLeaf
}

abstract class Family {
  val bits: Int

  def ipToBigInteger(addr: String): BigInteger = new BigInteger(1, InetAddress.getByName(addr).getAddress())
  def ipFromBitInteger(number: BigInteger): InetAddress = {
    val arr = number.toByteArray();
    val signDropped = if (arr(0) == 0) arr.drop(1) else arr
    InetAddress.getByAddress(new Array[Byte](bits/8 - signDropped.length) ++ signDropped)
  }

  def lineToTree(line: String): Option[(String, BinaryTree)]

  val exclude: BinaryTree

  def buildTree(prefix: BigInteger, length: Int): BinaryTree = {
    (0 until length).foldRight(BinaryTree.leaf) { (i, tree) =>
      if (prefix.testBit(bits - 1 - i))
        BinaryTree(null, tree)
      else
        BinaryTree(tree, null)
    }
  }

  def buildTree(begin: BigInteger, end: BigInteger): BinaryTree = {
    val left = buildTree(begin, bits)
    val right = buildTree(end, bits)

    object Left {
      def unapply(node: BinaryTree) = Option(node.left)
    }
    object Right {
      def unapply(node: BinaryTree) = Option(node.right)
    }
    def between: (BinaryTree, BinaryTree) => BinaryTree = {
      case (null, null) => null
      case (Leaf(), Leaf()) => BinaryTree.leaf
      case (Left(l), Left(r)) => BinaryTree(between(l, r), null)
      case (Right(l), Right(r)) => BinaryTree(null, between(l, r))
      case (Left(l), Right(r)) => {
        def left: BinaryTree => BinaryTree = {
          case null => null
          case Leaf() => BinaryTree.leaf
          case Left(t) => BinaryTree(left(t), BinaryTree.leaf)
          case Right(t) => BinaryTree(null, left(t))
        }
        def right: BinaryTree => BinaryTree = {
          case null => null
          case Leaf() => BinaryTree.leaf
          case Left(t) => BinaryTree(right(t), null)
          case Right(t) => BinaryTree(BinaryTree.leaf, right(t))
        }
        BinaryTree(left(l), right(r))
      }
    }

    between(left, right)
  }

  def union: (BinaryTree, BinaryTree) => BinaryTree = {
    case (null, r) => r
    case (l, null) => l
    case (Leaf(), r) => BinaryTree.leaf
    case (l, Leaf()) => BinaryTree.leaf
    case (l, r) => (union(l.left, r.left), union(l.right, r.right)) match {
      case (Leaf(), Leaf()) => BinaryTree.leaf
      case (left, right) => BinaryTree(left, right)
    }
  }

  def subtract: (BinaryTree, BinaryTree) => BinaryTree = {
    case (null, r) => null
    case (l, null) => l
    case (l, Leaf()) => null
    case (Leaf(), r) => (subtract(BinaryTree.leaf, r.left), subtract(BinaryTree.leaf, r.right)) match {
      case (null, null) => null
      case (left, right) => BinaryTree(left, right)
    }
      case (l, r) => (subtract(l.left, r.left), subtract(l.right, r.right)) match {
        case (null, null) => null
        case (left, right) => BinaryTree(left, right)
      }
  }

  def reverse: (BinaryTree => BinaryTree) = {
    case null => BinaryTree.leaf
    case Leaf() => null
    case BinaryTree(l, r) => BinaryTree(reverse(l), reverse(r))
  }

  def optimize: (BinaryTree, BinaryTree) => BinaryTree = {
    case (null, _) => null
    case (_, Leaf()) => null
    case (include, null) => BinaryTree.leaf
    case (Leaf(), exclude) => BinaryTree(optimize(BinaryTree.leaf, exclude.left), optimize(BinaryTree.leaf, exclude.right))
    case (include, exclude) => (optimize(include.left, exclude.left), optimize(include.right, exclude.right)) match {
      case (null, null) => null
      case (left, right) => BinaryTree(left, right)
    }
  }

  def cidr(ip: BigInteger, depth: Int): String
  def mask(ip: BigInteger, depth: Int): String

  def print(tree: BinaryTree, format: (BigInteger, Int) => String) { print(tree, BigInteger.valueOf(0), 0, format) }
  def print(tree: BinaryTree, prefix: BigInteger, depth: Int, format: (BigInteger, Int) => String) {
    if (tree.isLeaf)
      println(format(prefix.shiftLeft(bits - depth), depth))
    else {
      if (tree.left != null) print(tree.left, prefix.shiftLeft(1), depth + 1, format)
      if (tree.right != null) print(tree.right, prefix.shiftLeft(1).add(BigInteger.valueOf(1)), depth + 1, format)
    }
  }
}

class V4 extends Family {
  val bits: Int = 32

  val row = "[^|]*\\|([A-Z]*)\\|ipv4\\|([0-9.]*)\\|([0-9]*)\\|[^|]*\\|(?:(?:allocated)|(?:assigned)).*".r
  def lineToTree(line: String): Option[(String, BinaryTree)] = line match {
    case row(loc, ip, count) => {
      val longIp = ipToBigInteger(ip)
      Some((loc, buildTree(longIp, longIp.add(BigInteger.valueOf(java.lang.Integer.parseInt(count) - 1)))))
    }
    case _ => None
  }

  private def lengthToMask(i: Int): InetAddress = ipFromBitInteger(BigInteger.valueOf(~((1 << (bits - i)) - 1)))

  def cidr(ip: BigInteger, depth: Int): String = "%s/%d".format(com.google.common.net.InetAddresses.toAddrString(ipFromBitInteger(ip)), depth)
  def mask(ip: BigInteger, depth: Int): String = "%s %s".format(ipFromBitInteger(ip), lengthToMask(depth))

  val exclude = Array(
    buildTree(ipToBigInteger("127.0.0.0"), 8),
    buildTree(ipToBigInteger("10.0.0.0"), 8),
    buildTree(ipToBigInteger("100.64.0.0"), 10),
    buildTree(ipToBigInteger("169.254.0.0"), 16),
    buildTree(ipToBigInteger("172.16.0.0"), 12),
    buildTree(ipToBigInteger("192.168.0.0"), 16),
    buildTree(ipToBigInteger("224.0.0.0"), 4),
    buildTree(ipToBigInteger("240.0.0.0"), 4)).reduce(union)
}

class V6 extends Family {
  val bits: Int = 128

  val row6 = "[^|]*\\|([A-Z]*)\\|ipv6\\|([0-9a-f:]*)\\|([0-9]*)\\|[^|]*\\|(?:(?:allocated)|(?:assigned)).*".r
  def lineToTree(line: String): Option[(String, BinaryTree)] = line match {
    case row6(loc, ip, length) => Some((loc, buildTree(ipToBigInteger(ip), java.lang.Integer.parseInt(length))))
    case _ => None
  }

  def cidr(ip: BigInteger, depth: Int): String = "%s/%d".format(com.google.common.net.InetAddresses.toAddrString(ipFromBitInteger(ip)), depth)
  def mask(ip: BigInteger, depth: Int): String = throw new RuntimeException("IPv6 doesn't support mask")

  val exclude = reverse(Array(
    buildTree(ipToBigInteger("2001::"), 16),
    buildTree(ipToBigInteger("2002::"), 16),
    buildTree(ipToBigInteger("2003::"), 16),
    buildTree(ipToBigInteger("2400::"), 12),
    buildTree(ipToBigInteger("2600::"), 12),
    buildTree(ipToBigInteger("2610::"), 23),
    buildTree(ipToBigInteger("2620::"), 23),
    buildTree(ipToBigInteger("2630::"), 12),
    buildTree(ipToBigInteger("2800::"), 12),
    buildTree(ipToBigInteger("2a00::"), 12),
    buildTree(ipToBigInteger("2a10::"), 12),
    buildTree(ipToBigInteger("2c00::"), 12),
  ).reduce(union))
}

