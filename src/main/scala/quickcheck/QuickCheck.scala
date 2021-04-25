package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    j <- oneOf(const(empty), genHeap)
  } yield insert(i, j)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == scala.math.min(a, b)
  }

  property("delete1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("delete2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == scala.math.max(a, b)
  }

  property("sort") = forAll { (a: H) =>
    def genList(a: H, l: List[Int]): List[Int] = {
      if (isEmpty(a)) List() // 0 item
      else if (isEmpty(deleteMin(a))) List(findMin(a)) // 1 item
      else genList(deleteMin(a), findMin(a) :: l)
    }
    genList(a, List()).sorted == a
  }

  property("minMeld") = forAll { (a: H, b: H) =>
    val h = meld(a, b)
    findMin(h) == scala.math.min(findMin(a), findMin(b))
  }
}
