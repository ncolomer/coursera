package utils

import scala.io.Source

/**
 * Created by Nicolas on 30/04/2014.
 */
object Tools {

  type AdjacencyList = Map[Int, List[Int]]
  def loadAdjacencyList(filename: String): AdjacencyList = {
    val resource = getClass.getClassLoader.getResource(filename)
    Source.fromURL(resource).getLines().map(s => {
      val split = s.split("\t").map(_.toInt).toList
      split.head -> split.tail
    }).toMap
  }

  def loadArray(filename: String): Array[Int] = {
    val source = Source.fromURL(getClass.getClassLoader.getResource(filename))
    source.getLines().map(_.toInt).toArray
  }

  def loadIndexedSeq(filename: String): Seq[Int] = {
    loadArray(filename).toIndexedSeq
  }

  def time[A](a: => A): (A, Long) = {
    val now = System.nanoTime
    (a, (System.nanoTime - now) / 1000)
  }

}
