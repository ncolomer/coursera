package week1

import org.scalatest.FunSuite

class MST$Test extends FunSuite {

  import week1.MST._

  def sumOfCost(edges: List[Edge]): Int = edges.map{_.cost}.sum

  test("sample1") {
    // When
    val actual = mst("week1/edges-sample1.txt")
    // Then
    assertResult(2624) {sumOfCost(actual)}
  }

  test("sample2") {
    // When
    val actual = mst("week1/edges-sample2.txt")
    // Then
    assertResult(-351) {sumOfCost(actual)}
  }

  test("sample3") {
    // When
    val actual = mst("week1/edges-sample3.txt")
    // Then
    assertResult(27) {sumOfCost(actual)}
  }

  test("sample4") {
    // When
    val actual = mst("week1/edges-sample4.txt")
    // Then
    assertResult(55) {sumOfCost(actual)}
  }

  test("sample5") {
    // When
    val actual = mst("week1/edges-sample5.txt")
    // Then
    assertResult(6) {sumOfCost(actual)}
  }

  test("assignment") {
    // When
    val actual = mst("week1/edges.txt")
    // Then
    info(s"MST edge cost sum is ${sumOfCost(actual)}")
  }

}
