package scala.week5

import scala.collection.mutable.BitSet
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.collection.mutable.PriorityQueue

/**
 * Strongly Connected Components
 */
object Dijkstra {

  type AdjacencyList = Map[Int, Map[Int, Int]]

  case class Graph(adjacencyList: AdjacencyList) {
    val size = adjacencyList.size
    def shortestPath(from: Int, to: Int): Int = shortestPath2(from)(to)
    def shortestPath1(from: Int): Map[Int, Int] = {
      val X = new HashSet[Int] += from
      val A = new HashMap[Int, Int] += from -> 0
      while (X.size < size) {
        val (out, length) = X.flatMap(in =>
          adjacencyList(in).filterNot({
            case (out, length) => X(out)
          }).map({
            case (out, length) => (out, A(in) + length)
          })
        ).minBy(_._2)
        X += out
        A += out -> length
      }
      A.toMap
    }
    def shortestPath2(from: Int): Map[Int, Int] = {
      val X = new BitSet += from
      val A = new HashMap[Int, Int] += from -> 0
      while (X.size < size) {
        val (out, length) = X.flatMap(in =>
          adjacencyList(in).filterNot({
            case (out, length) => X(out)
          }).map({
            case (out, length) => (out, A(in) + length)
          })
        ).minBy(_._2)
        X += out
        A += out -> length
      }
      A.toMap
    }
    def shortestPath3(from: Int): Map[Int, Int] = {
      case class Elem(item: Int, priority: Int)
      type MinHeap = PriorityQueue[Elem] (Ordering.by[Elem, Int](_.priority))
      type MaxHeap = PriorityQueue[Elem] (Ordering.by[Elem, Int](_.priority).reverse)
      val heap = new MinHeap
      Map()
    }
  }

  object Graph {
    private val Label = """^(\d+)\s+""".r("label")
    private val Edges = """(\d+),(\d+)""".r("out", "length")
    private def extract(line: String) = {
      val label = Label.findFirstMatchIn(line).get.group("label").toInt
      val edges = Edges.findAllMatchIn(line).map(m =>
        m.group("out").toInt -> m.group("length").toInt
      ).toMap
      label -> edges
    }
    def load(filename: String): Graph = {
      val resource = getClass.getClassLoader.getResource(filename)
      val adjacencyList = Source.fromURL(resource).getLines().map(extract).toMap
      new Graph(adjacencyList)
    }
  }

}