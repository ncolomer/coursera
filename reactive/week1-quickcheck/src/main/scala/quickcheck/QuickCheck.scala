package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.collection.mutable.ListBuffer

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
   * If you insert any two elements into an empty heap,
   * finding the minimum of the resulting heap should
   * get the smallest of the two elements back.
   */
  property("findMin from a 2-element heap should yield the min") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  property("findMin from a 1-element heap should yield the element") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
   * If you insert an element into an empty heap, then
   * delete the minimum, the resulting heap should be empty.
   */
  property("insert and delete from an empty heap should yield an empty heap") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  /**
   * Given any heap, you should get a sorted sequence
   * of elements when continually finding and deleting
   * minima. (Hint: recursion and helper functions are
   * your friends.)
   */
  property("findMin and deleteMin from a random heap should yield a sorted sequence") = forAll { h: H =>
    def rec(h: H): List[Int] = h match {
      case h if h == empty => Nil
      case h => findMin(h) :: rec(deleteMin(h))
    }
    val l = rec(h)
    (l, l.tail).zipped.forall(_ <= _)
  }

  /**
   * Finding a minimum of the melding of any two heaps
   * should return a minimum of one or the other.
   */
  property("findMin a meld heap should yield the min of the mins") = forAll { (h1: H, h2: H) =>
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("min after insert element gt min of a random heap should yield min") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a+1 else a
    val h1 = insert(b+1, insert(b, insert(b+2, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(b+2, insert(b+1, empty))
  }

  property("2 insertions and 2 deletions should be empty") = forAll { (a: Int) =>
    val h1 = insert(a, insert(a, empty))
    val h2 = deleteMin(h1)
    !isEmpty(h2)
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(i, h)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
