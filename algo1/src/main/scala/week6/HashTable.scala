package scala.week6

import scala.collection.mutable.ListBuffer


class HashTable(b: Int) {
  private val buckets = Array.fill(b)(ListBuffer.empty[Int])
  private def h(n: Int): Int = Math.abs(n) % b
  def insert(n: Int): Unit = buckets(h(n)) += n
  def lookup(n: Int): Boolean = {
    def rec(xs: List[Int]): Boolean = xs match {
      case Nil => false
      case x :: xs => if (x == n) true else rec(xs)
    }
    rec(buckets(h(n)).toList)
  }
  def foreach(f: Int => Unit) = {

  }
}

object HashTable {
  def apply(buckets: Int) = {
    new HashTable(buckets)
  }
}
