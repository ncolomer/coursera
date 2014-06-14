package week6

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import utils.Tools._

@RunWith(classOf[JUnitRunner])
class MedianMaintenanceTest extends FunSuite {

  import scala.week6.MedianMaintenance._

  test("Median Maintenance algorithm sample 1") {
    // Given
    val array = loadIntArray("week6/mmsample1.txt")
    // When
    val actual = solve(array, 10)
    // Then
    assert(actual === 54)
  }

  test("Median Maintenance algorithm sample 2") {
    // Given
    val array = loadIntArray("week6/mmsample2.txt")
    // When
    val actual = solve(array, 7)
    // Then
    assert(actual === 23)
  }

  test("Median Maintenance algorithm sample 3") {
    // Given
    val array = loadIntArray("week6/mmsample3.txt")
    // When
    val actual = solve(array, 10)
    // Then
    assert(actual === 55)
  }

  test("Median Maintenance algorithm sample 4") {
    // Given
    val array = loadIntArray("week6/mmsample4.txt")
    // When
    val actual = solve(array, 20)
    // Then
    assert(actual === 148)
  }

  test("Median Maintenance algorithm sample 5") {
    // Given
    val array = loadIntArray("week6/mmsample5.txt")
    // When
    val actual = solve(array, 15)
    // Then
    assert(actual === 82)
  }

  test("Median Maintenance algorithm assignment") {
    // Given
    val array = loadIntArray("week6/Median.txt")
    // When
    val actual = solve(array, 10000)
    // Then
    val k = 10000
    info(s"Result is ${actual % k} ($actual mod $k)")
  }

}
