package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, a1: Int, a2: Int ) =>
    val m = List(a, a1, a2).min
    val h = insert(a2, insert(a1, insert(a, empty)))
    findMin(h) == m
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("del2") = forAll { (a: Int, a1: Int, a2: Int ) =>
    val l = List(a, a1, a2).sorted
    val h = deleteMin(insert(a2, insert(a1, insert(a, empty))))
    findMin(h) == l(1)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("gen2") = forAll { (h: H, h2: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val m2 = if (isEmpty(h)) 0 else findMin(h2)
    val hm = meld(h, h2)
    if(m2>m) findMin(hm)==m else findMin(hm)==m2
  }

  property("gen3") = forAll { (h: H) =>
    def isSorted(prev:Int, h: H):Boolean ={
      if (isEmpty(h)) true else prev <= findMin(h) && isSorted(findMin(h), deleteMin(h))
    }
    if (isEmpty(h)) true else isSorted(findMin(h), deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}


