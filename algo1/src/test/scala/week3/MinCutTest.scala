package week3

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.lang.Math.log
import utils.Tools.loadAdjacencyList

@RunWith(classOf[JUnitRunner])
class MinCutTest extends FunSuite {

  import scala.week3.MinCut._

  trait Sample {
    val minCut1 = Map(
      1 -> List(2, 3, 4),
      2 -> List(1),
      3 -> List(1),
      4 -> List(1)
    )
    val minCut2_1 = Map(
      1 -> List(2, 4),
      2 -> List(1, 3, 4),
      3 -> List(2, 4),
      4 -> List(1, 2, 3)
    )
    val minCut2_2 = Map(
      1 -> List(2, 4),
      2 -> List(1, 3, 5),
      3 -> List(2, 4),
      4 -> List(1, 3, 5),
      5 -> List(1, 2, 4)
    )
    val minCut3 = Map(
      1 -> List(2, 3, 4),
      2 -> List(1, 3, 4),
      3 -> List(1, 2, 4),
      4 -> List(1, 2, 3)
    )
  }

  test("min-cut 1") {
    new Sample {
      // Given
      val graph = Graph(minCut1)
      // When
      val retry = graph.n * log(graph.n).toInt
      val actual = graph.minCut(retry)
      // Then
      assert(actual === 1)
    }
  }

  test("min-cut 2 (1)") {
    new Sample {
      // Given
      val graph = Graph(minCut2_1)
      // When
      val retry = graph.n * log(graph.n).toInt
      val actual = graph.minCut(retry)
      // Then
      assert(actual === 2)
    }
  }

  test("min-cut 2 (2)") {
    new Sample {
      // Given
      val graph = Graph(minCut2_2)
      // When
      val retry = graph.n * log(graph.n).toInt
      val actual = graph.minCut(retry)
      // Then
      assert(actual === 2)
    }
  }

  test("min-cut 3") {
    new Sample {
      // Given
      val graph = Graph(minCut3)
      // When
      val retry = graph.n * log(graph.n).toInt 
      val actual = graph.minCut(retry)
      // Then
      assert(actual === 3)
    }
  }

  ignore("min-cut assignment") {
    // Given
    val graph = Graph(loadAdjacencyList("week3/kargerMinCut.txt"))
    // When
    val actual = graph.minCut(100)
    // Then
    info(s"Found min-cut ${actual}")
  }

}
