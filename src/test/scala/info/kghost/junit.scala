package info.kghost

import org.junit.runner.RunWith
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner

import info.kghost.App._

@RunWith(classOf[JUnitRunner])
class BuildTest extends Spec with ShouldMatchers {
  val set1 = buildTree("0.0.1.1/24")
  val set2 = buildTree("0.0.1.0/24")
  set1 should be(set2)
}

@RunWith(classOf[JUnitRunner])
class UnionTest extends Spec with ShouldMatchers {
  val set1 = buildTree("0.0.1.1/32")
  val set2 = buildTree("0.0.1.0/32")
  val set = buildTree("0.0.1.0/31")
  union(set1, set2) should be(set)
}

@RunWith(classOf[JUnitRunner])
class UnionTest2 extends Spec with ShouldMatchers {
  val set1 = buildTree("0.0.0.1/32")
  val set2 = buildTree("0.0.1.1/32")
  val set = buildTree("0.0.0.0/31")
  union(set1, set2) should not be (set)
}