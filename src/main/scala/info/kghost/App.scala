package info.kghost

import java.math.BigInteger
import java.net.InetAddress
import java.io.InputStream
import org.apache.commons.cli.Options
import org.apache.commons.cli.DefaultParser
import org.apache.commons.cli.HelpFormatter

/**
 * @author ${user.name}
 */
object App {
  def longToIp(i: Long) = (24 to 0 by -8) map { b => (i & (0xFFL << b)) >> b } mkString "."
  def ipToLong(ip: String) = ((24 to 0 by -8) zip ip.split("\\.")).foldLeft(0L) {
    case (result, (position, atom)) => result | (java.lang.Long.parseLong(atom) << position)
  }
  def countToMask(i: Int) = 32 - (0 to 32).find(1 << _ == i).get
  def lengthToMask(i: Int) = longToIp(~((1 << (32 - i)) - 1))
  def cidrToLong(cidr: String) = {
    val c = cidr.split('/')
    (ipToLong(c(0)), java.lang.Integer.parseInt(c(1)))
  }

  object BinaryTree {
    def cidr(ip: Long, depth: Int) = "%s/%d".format(longToIp(ip), depth)
    def mask(ip: Long, depth: Int) = "%s %s".format(longToIp(ip), lengthToMask(depth))
  }
  case class BinaryTree(val left: BinaryTree, val right: BinaryTree) {
    def leaf = left == null && right == null
    def print(format: (Long, Int) => String) { print(0, 0)(format) }
    def print(prefix: Long, depth: Int)(format: (Long, Int) => String) {
      if (leaf)
        println(format(prefix << (32 - depth), depth))
      else {
        if (left != null) left.print(prefix << 1, depth + 1)(format)
        if (right != null) right.print((prefix << 1) + 1, depth + 1)(format)
      }
    }
  }

  val leaf = BinaryTree(null, null)
  object Leaf {
    def unapply(node: BinaryTree) = node != null && node.leaf
  }

  def buildTree(cidr: String): BinaryTree = {
    val (ip, mask) = cidrToLong(cidr)
    buildTree(ip, mask)
  }

  def buildTree(ip: Long, mask: Int): BinaryTree = {
    (0 until mask).foldRight(leaf) { (i, tree) =>
      if ((ip & 1 << (31 - i)) == 0)
        BinaryTree(tree, null)
      else
        BinaryTree(null, tree)
    }
  }

  def buildTree(begin: Long, end: Long): BinaryTree = {
    val left = buildTree(begin, 32)
    val right = buildTree(end, 32)

    object Left {
      def unapply(node: BinaryTree) = Option(node.left)
    }
    object Right {
      def unapply(node: BinaryTree) = Option(node.right)
    }
    def between: (BinaryTree, BinaryTree) => BinaryTree = {
      case (null, null) => null
      case (Leaf(), Leaf()) => leaf
      case (Left(l), Left(r)) => BinaryTree(between(l, r), null)
      case (Right(l), Right(r)) => BinaryTree(null, between(l, r))
      case (Left(l), Right(r)) => {
        def left: BinaryTree => BinaryTree = {
          case null => null
          case Leaf() => leaf
          case Left(t) => BinaryTree(left(t), leaf)
          case Right(t) => BinaryTree(null, left(t))
        }
        def right: BinaryTree => BinaryTree = {
          case null => null
          case Leaf() => leaf
          case Left(t) => BinaryTree(right(t), null)
          case Right(t) => BinaryTree(leaf, right(t))
        }
        BinaryTree(left(l), right(r))
      }
    }

    between(left, right)
  }

  def union: (BinaryTree, BinaryTree) => BinaryTree = {
    case (null, r) => r
    case (l, null) => l
    case (Leaf(), r) => leaf
    case (l, Leaf()) => leaf
    case (l, r) => (union(l.left, r.left), union(l.right, r.right)) match {
      case (Leaf(), Leaf()) => leaf
      case (left, right) => BinaryTree(left, right)
    }
  }

  def subtract: (BinaryTree, BinaryTree) => BinaryTree = {
    case (null, r) => null
    case (l, null) => l
    case (l, Leaf()) => null
    case (Leaf(), r) => (subtract(leaf, r.left), subtract(leaf, r.right)) match {
      case (null, null) => null
      case (left, right) => BinaryTree(left, right)
    }
    case (l, r) => (subtract(l.left, r.left), subtract(l.right, r.right)) match {
      case (null, null) => null
      case (left, right) => BinaryTree(left, right)
    }
  }

  def reverse: (BinaryTree => BinaryTree) = {
    case null => leaf
    case Leaf() => null
    case BinaryTree(l, r) => BinaryTree(reverse(l), reverse(r))
  }

  def optimize: (BinaryTree, BinaryTree) => BinaryTree = {
    case (null, _) => null
    case (_, Leaf()) => null
    case (include, null) => leaf
    case (Leaf(), exclude) => BinaryTree(optimize(leaf, exclude.left), optimize(leaf, exclude.right))
    case (include, exclude) => (optimize(include.left, exclude.left), optimize(include.right, exclude.right)) match {
      case (null, null) => null
      case (left, right) => BinaryTree(left, right)
    }
  }

  val row = "[^|]*\\|([A-Z]*)\\|ipv4\\|([0-9.]*)\\|([0-9]*)\\|[^|]*\\|(?:(?:allocated)|(?:assigned)).*".r

  def main(args: Array[String]) {
    val options = new Options();
    options.addOption("h", "help", false, "display help");
    options.addOption("o", "online", false, "online mode");
    options.addOption("c", "country", true, "filter which country");
    options.addOption("f", "format", true, "output format, cidr or mask");
    options.addOption("r", "reverse", false, "output reverse (negetive) route");

    val parser = new DefaultParser();
    val cmd = parser.parse(options, args);

    if (cmd.hasOption("h")) {
      val formatter = new HelpFormatter();
      formatter.printHelp("rs", options);
    } else {
      val data = if (cmd.hasOption("o"))
        Data.data1
      else
        Data.data2

      val country = cmd.getOptionValue("c", "CN")
      val format = cmd.getOptionValue("f", "mask") match {
        case "cidr" => BinaryTree.cidr _
        case "mask" => BinaryTree.mask _
      }
      val reverse = cmd.hasOption("r")

      val exc = Array(
        buildTree("127.0.0.0/8"),
        buildTree("10.0.0.0/8"),
        buildTree("100.64.0.0/10"),
        buildTree("169.254.0.0/16"),
        buildTree("172.16.0.0/12"),
        buildTree("192.168.0.0/16"),
        buildTree("224.0.0.0/4"),
        buildTree("240.0.0.0/4")).reduce(union)
      val (include, exclude) = data.foldLeft(null: BinaryTree, exc) {
        case ((include, exclude), line) => line match {
          case row(loc, ip, count) => {
            val longIp = ipToLong(ip)
            val tree = buildTree(longIp, longIp + java.lang.Integer.parseInt(count) - 1)
            if (loc == country)
              (union(include, tree), exclude)
            else
              (include, union(exclude, tree))
          }
          case _ => (include, exclude)
        }
      }
      val result = if (!reverse)
        optimize(include, exclude)
      else
        optimize(exclude, include)
      result.print(format)
    }
  }
}
