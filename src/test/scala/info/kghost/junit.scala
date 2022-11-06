package info.kghost

import org.junit.runner.RunWith
import org.scalatest._
import matchers._
import funspec._
import org.scalatestplus.junit.JUnitRunner

import info.kghost.App._

@RunWith(classOf[JUnitRunner])
class Tests extends AnyFunSpec with must.Matchers {
  describe("IPv4") {
    val family = new V4
    it("can union") {
      val set1 = family.buildTree(family.ipToBigInteger("0.0.1.1"), 24)
      val set2 = family.buildTree(family.ipToBigInteger("0.0.1.0"), 24)
      set1 must be(set2)
    }

    it("can union 2") {
      val set1 = family.buildTree(family.ipToBigInteger("0.0.1.1"), 32)
      val set2 = family.buildTree(family.ipToBigInteger("0.0.1.0"), 32)
      val set = family.buildTree(family.ipToBigInteger("0.0.1.0"), 31)
      family.union(set1, set2) must be(set)
    }

    it ("xx") {
      val set1 = family.buildTree(family.ipToBigInteger("0.0.0.1"), 32)
      val set2 = family.buildTree(family.ipToBigInteger("0.0.1.1"), 32)
      val set = family.buildTree(family.ipToBigInteger("0.0.0.0"), 31)
      family.union(set1, set2) must not be (set)
    }
  }
}
