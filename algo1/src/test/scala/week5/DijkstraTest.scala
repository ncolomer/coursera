package week5

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DijkstraTest extends FunSuite {

  import scala.week5.Dijkstra._

  test("Adjacency list loading") {
    // Given
    val expected = Map(
      1 -> Map(2 -> 5, 3 -> 1),
      2 -> Map(4 -> 10, 1 -> 5),
      3 -> Map(4 -> 1, 1 -> 1),
      4 -> Map(2 -> 10, 3 -> 1)
    )
    // When
    val actual = Graph.load("week5/sample1.txt").adjacencyList
    // Then
    assert(actual === expected)
  }

  test("Shortest path on sample1 (small case)") {
    // Given
    val graph = Graph.load("week5/sample1.txt")
    // Then
    assert(graph.shortestPath(1, 4) === 2)
  }

  test("Shortest path on sample2 (medium case)") {
    // Given
    val graph = Graph.load("week5/sample2.txt")
    // Then
    assert(graph.shortestPath(1, 7) === 5)
  }

  test("Shortest path on sample3 (medium case)") {
    // Given
    val graph = Graph.load("week5/sample3.txt")
    // Then
    assert(graph.shortestPath(1, 6) === 11)
    assert(graph.shortestPath(1, 5) === 20)
    assert(graph.shortestPath(1, 4) === 20)
    assert(graph.shortestPath(1, 3) === 9)
    assert(graph.shortestPath(1, 2) === 7)
  }

  test("Shortest path on sample4 (large case)") {
    // Given
    val graph = Graph.load("week5/sample4.txt")
    // Then
    assert(graph.shortestPath(13, 5) === 26)
  }

  test("Shortest path on sample5 (big case)") {
    // Given
    val graph = Graph.load("week5/sample5.txt")
    // Then
    assert(graph.shortestPath(28, 6) === 9)
  }

  test("Shortest path on assignment") {
    // Given
    val graph = Graph.load("week5/dijkstraData.txt")
    // Then
    val lengthFrom1 = graph.shortestPath1(1)
    val seq = Seq(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
    info(s"Shortest-path distances from 1 to ${seq.mkString(",")} are respectively: "
      + seq.map(lengthFrom1).mkString(","))
  }

}
