package week6

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import utils.Tools._

@RunWith(classOf[JUnitRunner])
class HashTableTest extends FunSuite {

  import scala.week6.Sum2._

  test("2-SUM algorithm sum2sample1.txt") {
    // Given
    val array = loadLongArray("week6/sum2sample1.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 3)
  }

  test("2-SUM algorithm sum2sample2.txt") {
    // Given
    val array = loadLongArray("week6/sum2sample2.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 5)
  }

  test("2-SUM algorithm sum2tc_20.txt") {
    // Given
    val array = loadLongArray("week6/sum2tc_20.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 1)
  }

  test("2-SUM algorithm sum2tc_100.txt") {
    // Given
    val array = loadLongArray("week6/sum2tc_100.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 6)
  }

  test("2-SUM algorithm sum2tc_1000.txt") {
    // Given
    val array = loadLongArray("week6/sum2tc_1000.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 609)
  }

  test("2-SUM algorithm sum2tc_10000.txt") {
    // Given
    val array = loadLongArray("week6/sum2tc_10000.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 19017)
  }

  test("2-SUM algorithm sum2tc_100000.txt") {
    // Given
    val array = loadLongArray("week6/sum2tc_100000.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 20001)
  }

  test("2-SUM algorithm sum2tc_1000000.txt") {
    // Given
    val array = loadLongArray("week6/sum2tc_1000000.txt")
    // When
    val actual = solve(array)
    // Then
    assert(actual === 20001)
  }

  test("2-SUM algorithm assignment") {
    // Given
    val array = printTime("Reading array") {loadLongArray("week6/2sum.txt")}
    // When
    val sum = printTime("Running algorithm") {solve(array)}
    // Then
    info(s"Result is $sum")
  }

}
