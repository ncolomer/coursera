package scala.week3

import scala.util.Random


object MinCut {

  type Edge = Set[Int]
  type Edges = Array[Edge]

  class Graph(vertices: Set[Int], edges: Edges) {

    val n = vertices.size
    val m = edges.length

    def minCut: Int = {
      val random = new Random(System.nanoTime)
      def contract(that: Edges): Edges = {
        // Pick one edge, keep a reference I and remove it
        val I = that(random.nextInt(that.length))
        // Filter Edges following rules
        that
          // if E is OR equals I (ie. self loop), remove E
          .filterNot(E => (E eq I) || (E equals I))
          // if E intersects I, add I in E and keep E else just keep E
          .map(E => if ((E intersect I).size > 0) E ++ I else E)
      }
      def rec(iteration: Int, that: Edges): Int = {
        if (iteration == 0) that.length // Found a min cut, return count
        else rec(iteration - 1, contract(that))
      }
      rec(n - 2, edges)
    }

    def minCut(retry: Int): Int = {
      (0 to retry).foldLeft[Int](0)((min, i) => {
        println(s"Iteration #${i}, actual min is ${min}")
        if (i == 0) minCut else Math.min(min, minCut)
      })
    }

  }

  object Graph {
    type AdjacencyList = Map[Int, List[Int]]
    def apply(adjacencyList: AdjacencyList): Graph = {
      val vertices = adjacencyList.keySet
      val edges: Edges = adjacencyList.flatMap(entry => {
        val out = entry._1
        val ins = entry._2
        ins.map(in => Set(out, in))
      }).toSet.toArray
      new Graph(vertices, edges)
    }
  }

}
