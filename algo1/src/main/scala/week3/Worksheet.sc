import scala.util.Random


// with list
class Graph(vertices: Set[Int], edges: List[Set[Int]]) {

  type Edges = List[Set[Int]]

  val n = vertices.size
  val m = edges.size

  def minCut: Int = {
    val random = new Random(System.nanoTime)
    def contract(cEdges: Edges): Edges = {
      // Pick one edge, keep a reference I and remove it
      val index = random.nextInt(cEdges.length)
      val split = cEdges.splitAt(index)
      val I = split._2.head
      val list = split._1 ::: split._2.tail
      // Filter Edges following rules
      list.flatMap(E => {
        // if E contains/equals? I (ie. self loop), remove E
        if (E equals I) List()
        // else if E intersects I, add I in E and keep E
        else if ((E intersect I).size > 0) List(E ++ I)
        // else just keep E
        else List(E)
      })
    }
    def rec(iteration: Int, list: Edges): Int = {
      if (iteration == 0) list.length // Found a min cut, return count
      else rec(iteration - 1, contract(list))
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
    val edges: List[Set[Int]] = adjacencyList.flatMap(entry => {
      val out = entry._1
      val ins = entry._2
      ins.map(in => Set(out, in))
    }).toSet.toList
    new Graph(vertices, edges)
  }
}
