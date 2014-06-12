package week4

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SccTest extends FunSuite {

  import scala.week4.Scc3._

  def largest(s: SCCs): List[Int] = {
    s.map(_.size).sorted(Ordering[Int].reverse).take(5).toList
  }

  test("SCCs on sample1") {
    // Given
    val expected = List(List(3, 0, 6), List(5, 2, 8), List(1, 4, 7))
    // When
    val actual = Graph.load("week4/sample1.txt").stronglyConnectedComponents
    // Then
    assert(actual === expected)
    assert(largest(actual) === List(3, 3, 3))
  }

  test("SCCs on sample2") {
    // Given
    val expected = List(List(5, 4, 6), List(3), List(7, 8, 9, 10), List(1, 0, 2))
    // When
    val actual = Graph.load("week4/sample2.txt").stronglyConnectedComponents
    // Then
    assert(actual === expected)
    assert(largest(actual) === List(4, 3, 3, 1))
  }

  test("SCCs on test case 1") {
    // When
    val actual = Graph.load("week4/testCase1.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(3, 3, 3))
  }

  test("SCCs on test case 2") {
    // When
    val actual = Graph.load("week4/testCase2.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(3, 3, 2))
  }

  test("SCCs on test case 3") {
    // When
    val actual = Graph.load("week4/testCase3.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(3, 3, 1, 1))
  }

  test("SCCs on test case 4") {
    // When
    val actual = Graph.load("week4/testCase4.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(7, 1))
  }

  test("SCCs on test case 5") {
    // When
    val actual = Graph.load("week4/testCase5.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(6, 3, 2, 1))
  }

  test("SCCs on test case 6") {
    // When
    val actual = Graph.load("week4/testCase6.txt").stronglyConnectedComponents
    // Then
    assert(!(largest(actual) == List(6, 1, 1)))
  }

  test("SCCs on test case 7") {
    // When
    val actual = Graph.load("week4/testCase7.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(3, 2, 2, 2, 1))
  }

  test("SCCs on test case 8") {
    // When
    val actual = Graph.load("week4/testCase8.txt").stronglyConnectedComponents
    // Then
    assert(largest(actual) === List(6, 4, 3, 2, 1))
  }

  test("SCCs on assignment") {
    // When
    val actual = Graph.load("week4/SCC.txt").stronglyConnectedComponents
    // Then
    info("Sizes of the 5 largest SCCs in the given graph: " + largest(actual))
    assert(largest(actual) === List(434821, 968, 459, 313, 211))
  }

}
