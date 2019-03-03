package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {
  import Tree._

  "size" should "return number of leaves and branches" in {
    Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) shouldBe 7
    Tree.size(Leaf("a")) shouldBe 1
    Tree.size(Branch(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c")), Leaf("d"))) shouldBe 7
  }

  "maximum" should "return maximum value of tree" in {
    maximum(Branch(Branch(Branch(Leaf(-5), Leaf(7)), Leaf(3)), Branch(Leaf(4), Leaf(2)))) shouldBe 7
  }

  "depth" should "return the depth of the tree" in {
    depth(Branch(Leaf("3"), Leaf("4"))) shouldBe 1
    depth(Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)), Leaf(5))) shouldBe 4
  }

  "map" should "apply given function to all leaves of the tree" in {
    map(Branch(Leaf(3), Leaf(4)))(_ + 2) shouldBe Branch(Leaf(5), Leaf(6))
  }

  "fold" should "be used to implement size, maximum and depth" in {
    size2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) shouldBe 7
    size2(Leaf("a")) shouldBe 1
    size2(Branch(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c")), Leaf("d"))) shouldBe 7

    maximum2(Branch(Branch(Branch(Leaf(-5), Leaf(7)), Leaf(3)), Branch(Leaf(4), Leaf(2)))) shouldBe 7

    depth2(Branch(Leaf("3"), Leaf("4"))) shouldBe 1
    depth2(Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)), Leaf(5))) shouldBe 4

    map2(Branch(Leaf(3), Leaf(4)))(_ + 2) shouldBe Branch(Leaf(5), Leaf(6))
  }
}
