package scala.week6

import scala.collection.mutable.{ListBuffer, PriorityQueue}

/**
 * Created by Nicolas on 13/06/2014.
 */
class MedianMaintenance {
  private val buffer = new ListBuffer[Int]
  // H-High = MaxHeap
  private val hHigh = new PriorityQueue[Int]()(Ordering[Int].reverse)
  // H-Low = MinHeap
  private val hLow = new PriorityQueue[Int]()(Ordering[Int])

  private def balanceHeaps(): Unit = {
    def balance = hLow.length - hHigh.length
    while (balance < 0) hLow enqueue hHigh.dequeue
    while (balance > 1) hHigh enqueue hLow.dequeue
  }

  private def pickMedian(): Int = hLow.head

  def submit(n: Int) = {
    if (hLow.isEmpty) hLow enqueue n
    else if (n <= hLow.head) hLow enqueue n
    else hHigh enqueue n
    balanceHeaps
    pickMedian +=: buffer
  }
  def get(k: Int): Int = buffer(k)
  def sum(): Long = buffer.sum
}


object MedianMaintenance {

  def solve(array: Array[Int], n: Int): Long = {
    val m = new MedianMaintenance
    array.take(n).foreach(m.submit)
    m.sum
  }

}
