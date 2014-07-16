package week1

import utils.Tools._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object MST {

  case class Edge(in: Int, out: Int, cost: Int)

  private def loadGraph(file: String): (Set[Int], List[Edge]) = {
    val lines = loadTextFile(file)
    val (nodeSize, edgeSize) = lines.next().split("\\s").map{_.toInt} match {case Array(a, b) => (a, b)}
    val edges = lines.toList.map {_.split("\\s").map {_.trim.toInt}}
      .collect {case Array(in, out, cost) => Edge(in, out, cost)}
    val nodes = edges.flatMap {edge => List(edge.in, edge.out)}.toSet
    assume(edges.size == edgeSize)
    assume(nodes.size == nodeSize)
    (nodes, edges)
  }

  //val minHeap = new mutable.PriorityQueue[Edge]()(Ordering.by(e => e.cost))

  /**
   * Prim's minimum spanning tree algorithm
   * @param file the file containing the graph (as adjacency list)
   * @return the minimum spanning tree
   */
  def mst(file: String): List[Edge] = {
    val (nodes, edges) = loadGraph(file)
    val X = mutable.Set(nodes.head)
    val V = mutable.Set() ++ nodes.tail
    val T = ListBuffer[Edge]()
    while (X.size < nodes.size) {
      val edge = edges
        .filter{case Edge(in, out, _)=> X.contains(in) && V.contains(out) || X.contains(out) && V.contains(in)}
        .sortBy(e => e.cost).head
      X += edge.in += edge.out
      V -= edge.in -= edge.out
      T += edge
    }
    T.toList
  }

}
