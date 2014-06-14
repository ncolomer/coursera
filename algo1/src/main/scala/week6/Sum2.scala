package scala.week6

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.util.Sorting

/**
 * Created by Nicolas on 13/06/2014.
 */
object Sum2 {

  def solve(array: Array[Long], k: Int = 10000): Int = solve4(array, k)

  def solve1(array: Array[Long], k: Int): Int = {
    def hash(x: Long): Int = (x / k).toInt
    lazy val list: Array[Long] = {
      Sorting.quickSort(array)
      array.foldRight(List[Long]()) {
        case (n, list) =>
          if (list.nonEmpty && list.head == n) list
          else n :: list
      }
    }.toArray
    //val treeSet = mutable.TreeSet[Long]() ++= array
    val hashMap = list.groupBy(hash) withDefaultValue Array.empty
    val values = for {
      x <- list
      c <- Seq(-k, 0, k).map(d => hashMap(hash(-x + d)))
    } yield c.count(y => {val t = x + y; (x < y) && (-k <= t) && (t <= k)})
    values.sum
  }

  def solve2(array: Array[Long], k1: Int): Long = {
    val treeSet = mutable.TreeSet[Long]() ++= array
    val counter = new AtomicInteger
    def rec(items: List[Long], acc: Int): Int = items match {
      case Nil => acc
      case x :: xs =>
        if (counter.incrementAndGet % 10 == 0) println(counter.get)
        rec(xs, acc + xs.map(x + _).count(t => -k1 <= t && t <= k1))
    }
    rec(treeSet.toList, 0)
  }

  def solve3(a: Array[Long], k: Int): Int = {
    def hash(x: Long) = (x / k).toInt
    val treeSet = mutable.TreeSet[Long]() ++= a
    val hashMap = treeSet.groupBy(hash) withDefaultValue mutable.TreeSet.empty[Long]
    val values = for {
      x <- treeSet.iterator
      c <- Seq(-k - x, -x, k - x).map(hash).distinct.map(hashMap)
    } yield {
      val count = c.count(y => {val t = x + y; (x < y) && (-k <= t) && (t <= k)})
      //println(s"x=$x, candidates $candidates, $count matches")
      count
    }
    values.sum
  }

  def solve4(array: Array[Long], k: Int): Int = {
    def hash(x: Long) = (x / k).toInt
    val bitSet = new mutable.BitSet(2 * k)
    val hashMap = array.groupBy(hash) withDefaultValue Array.empty[Long]
    for (x <- array; candidates <- Seq(-k - x, -x, k - x).map(hash).map(hashMap))
      candidates
        .filter(y => y != x)
        .map(y => x + y)
        .filter(t => (-k <= t) && (t <= k))
        .map(t => k + t.toInt)
        .foreach(t => bitSet += t)
    bitSet.size
  }

}
