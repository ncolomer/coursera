import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.BitSet
import scala.io.Source

object Graph {
  case class Container(original: Graph, reversed: Graph)
  type Graph = Array[List[Int]]
  def load(filename: String): Container = {
    val resource = getClass.getClassLoader.getResource(filename)
    val size = Source.fromURL(resource).getLines().size
    val Pattern = "(\\d+) (\\d+)".r("out", "in")
    val original = Array.fill[List[Int]](size) {Nil}
    val reversed = Array.fill[List[Int]](size) {Nil}
    Source.fromURL(resource).getLines().foreach(line =>
      Pattern.findFirstMatchIn(line) match {
        case Some(m) => {
          val out = m.group("out").toInt
          val in = m.group("in").toInt
          original(out - 1) ::= in - 1
          reversed(in - 1) ::= out - 1
        }
        case None => throw new IllegalArgumentException
      })
    Container(original, reversed)
  }
}
val graph = Graph.load("week4/SCC.txt")
graph.original(0)
graph.reversed(0)
class SccState(size: Int) {
  private val counter = new AtomicInteger
  private val explored = new BitSet(size)
  // 0-indexed bit set
  private val finished = new Array[Int](size)
  private val leaders = new Array[Int](size)
  // 0-indexed array
  def clearExplored = explored.clear()
  def setExplored(node: Int) = explored += (node - 1)
  def isExplored(node: Int): Boolean = explored(node - 1)
  def setFinished(node: Int) = finished(counter.getAndIncrement) = node
  def getFinished = finished.toTraversable
  def setLeader(node: Int, leader: Int) = leaders(node - 1) = leader
  def getLeader(node: Int) = leaders(node - 1)
}
