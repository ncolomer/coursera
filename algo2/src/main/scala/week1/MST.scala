package week1

import utils.Tools._

import scala.collection.mutable


object MST {

  case class Edge(in: Int, out: Int, cost: Int)

  private def loadGraph(file: String): List[Edge] = {
    val lines = loadTextFile(file)
    val size = lines.next().toInt
    val edges = lines.toList
      .map {_.split("\\s").map {_.trim.toInt}}
      .collect {case Array(in, out, cost) => Edge(in, out, cost)}
    assume(edges.size == size)
    edges
  }

  /**
   * Prim's minimum spanning tree algorithm
   * @param file the file containing the graph (as adjacency list)
   * @return the minimum spanning tree
   */
  def mst(file: String): List[Edge] = {
    val minHeap = new mutable.PriorityQueue[Edge]()(Ordering.by(e => e.cost))
    ???
  }

}
