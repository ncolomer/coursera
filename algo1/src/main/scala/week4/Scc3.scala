package scala.week4

import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.Stack

/**
 * Strongly Connected Components
 */
object Scc3 {

  type AdjacencyList = Array[ListBuffer[Int]]
  type SCC = List[Int]
  type SCCs = List[SCC]
  val ASC = Ordering[Int].reverse

  class Graph(original: AdjacencyList, reversed: AdjacencyList) {
    assert(original.length == reversed.length)
    val length = original.length
    def stronglyConnectedComponents: SCCs = {
      class State() {
        private val visited = Array.ofDim[Boolean](length)
        def setVisited(i: Int): Int = { visited(i) = true; i }
        def isVisited(i: Int) = visited(i)
        def isNotVisited(i: Int) = !visited(i)
      }
      class Finished() {
        private val finished = Array.ofDim[Int](length)
        private val counter = new AtomicInteger(length)
        def add(i: Int) = finished(counter.decrementAndGet) = i
        def get = finished
        override def toString = finished.mkString("(", ",", ")")
      }
      def dfs(graph: AdjacencyList, state: State, sorted: Boolean = false)(start: Int, f: Int => Unit) = {
        val stack = Stack(start)
        val forks = Stack[Stack[Int]](Stack())
        state.setVisited(start)
        while (!stack.isEmpty) {
          val i = stack.pop
          forks.head push i
          val ins =
            if (sorted) graph(i).toList filterNot (state isVisited _) sorted(ASC)
            else graph(i).toList filterNot (state isVisited _)
          ins.foreach(j => {state setVisited j; stack push j})
          ins.size match {
            case 0 => forks.pop foreach (f(_)) // Sink node
            case _ => ins.tail foreach (ign => forks push Stack())
          }
        }
      }
      println("Start DFS Pass #1")
      val state1 = new State
      def dfsPass1 = dfs(reversed, state1, true) _
      val finished = new Finished
      for (i <- length - 1 to 0 by -1 if state1.isNotVisited(i)) {
        dfsPass1(i, j => finished.add(j))
      }
      println("Start DFS Pass #2")
      val state2 = new State
      def dfsPass2 = dfs(original, state2) _
      val sccs = ListBuffer.empty[SCC]
      for (i <- finished.get if state2.isNotVisited(i)) {
        val scc = ListBuffer.empty[Int]
        dfsPass2(i, j => scc += j)
        sccs += scc.toList
      }
      sccs.toList
    }
  }

  object Graph {
    private val Pattern = "(\\d+) (\\d+)".r("out", "in")
    private def extract(line: String) = Pattern.findFirstMatchIn(line) match {
      case Some(m) => (m.group("out").toInt, m.group("in").toInt)
      case None => throw new IllegalArgumentException("Malformed row")
    }
    def load(filename: String): Graph = {
      val resource = getClass.getClassLoader.getResource(filename)
      val size = Source.fromURL(resource).getLines().foldLeft(0)((max, line) => {
        Math.max(extract(line)._1, max)
      })
      val original = Array.fill[ListBuffer[Int]](size) {ListBuffer.empty}
      val reversed = Array.fill[ListBuffer[Int]](size) {ListBuffer.empty}
      Source.fromURL(resource).getLines().foreach(line => {
        val (out, in) = extract(line)
        original(out - 1) += (in - 1)
        reversed(in - 1) += (out - 1)
      })
      new Graph(original, reversed)
    }
  }

}