package info.kghost

import org.junit.runner.RunWith
import org.scalatest.MustMatchers._
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner

import info.kghost.App._

@RunWith(classOf[JUnitRunner])
class BuildTest extends Spec {
  val set1 = buildTree("0.0.1.1/24")
  val set2 = buildTree("0.0.1.0/24")
  set1 must be(set2)
}

@RunWith(classOf[JUnitRunner])
class UnionTest extends Spec {
  val set1 = buildTree("0.0.1.1/32")
  val set2 = buildTree("0.0.1.0/32")
  val set = buildTree("0.0.1.0/31")
  union(set1, set2) must be(set)
}

@RunWith(classOf[JUnitRunner])
class UnionTest2 extends Spec {
  val set1 = buildTree("0.0.0.1/32")
  val set2 = buildTree("0.0.1.1/32")
  val set = buildTree("0.0.0.0/31")
  union(set1, set2) must not be (set)
}
