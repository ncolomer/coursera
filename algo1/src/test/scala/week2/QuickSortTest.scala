package week2

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import utils.Tools._
import scala.util.Random._

@RunWith(classOf[JUnitRunner])
class QuickSortTest extends FunSuite {

  import scala.week2.QuickSort._

  trait SampleData {
    val a1 = Array(3, 8, 2, 5, 1, 4, 7, 6)
    val a1Sorted = Array(1, 2, 3, 4, 5, 6, 7, 8)
    val a2 = Array(3, 8, 2, 5, 9, 1, 4, 7, 6)
    val a2Sorted = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  test("Sort a1 (even) with first element pivot picker") {
    new SampleData {
      // Given
      val expected = a1Sorted
      val actual = a1
      // When
      sort(array => 0)(actual)
      // Then
      assert(actual === expected)
    }
  }

  test("Sort a2 (odd) with first element pivot picker") {
    new SampleData {
      // Given
      val expected = a2Sorted
      val actual = a2
      // When
      sort(array => 0)(actual)
      // Then
      assert(actual === expected)
    }
  }

  test("Sort a1 (even) with random pivot picker") {
    new SampleData {
      // Given
      val expected = a1Sorted
      val actual = a1
      // When
      sort(array => nextInt(array.length))(actual)
      // Then
      assert(actual === expected)
    }
  }

  test("Sort a2 (odd) with random pivot picker") {
    new SampleData {
      // Given
      val expected = a2Sorted
      val actual = a2
      // When
      sort(array => nextInt(array.length))(actual)
      // Then
      assert(actual === expected)
    }
  }

  def checkSorted(array: Array[Int]): Boolean = {
    array.foldLeft[(Boolean, Int)]((true, 0)) {(b, i) => (b._1 & b._2 < i, i)}._1
  }

  test("Sort assignment with first element pivot picker") {
    new SampleData {
      // Given
      val actual = loadArray("week2/QuickSort.txt")
      val picker = (array: Array[Int]) => 0
      // When
      val comparisons = sort(picker)(actual)
      // Then
      assert(checkSorted(actual))
      info(s"# of comparisons: ${comparisons}")
    }
  }

  test("Sort assignment with last element pivot picker") {
    new SampleData {
      // Given
      val actual = loadArray("week2/QuickSort.txt")
      val picker = (array: Array[Int]) => array.length - 1
      // When
      val comparisons = sort(picker)(actual)
      // Then
      assert(checkSorted(actual))
      info(s"# of comparisons: ${comparisons}")
    }
  }

  val medianOfThreePicker = (array: Array[Int]) => {
    val first = (0, array.head) // Tuple(index, value)
    val middle = ((array.length - 1) / 2, array((array.length - 1) / 2))
    val last = (array.length - 1, array.last)
    List(first, middle, last).sortWith(_._2 < _._2)(1)._1
  }

  test("median-of-three pivot picker (even), peek first") {
    // Given
    val array = Array(4, 2, 8, 5, 7, 1)
    // When
    val actual = medianOfThreePicker(array)
    // Then
    assert(actual === 0)
  }

  test("median-of-three pivot picker (even), peek middle") {
    // Given
    val array = Array(8, 2, 4, 5, 7, 1)
    // When
    val actual = medianOfThreePicker(array)
    // Then
    assert(actual === 2)
  }

  test("median-of-three pivot picker (even), peek last") {
    // Given
    val array = Array(1, 2, 8, 5, 7, 6)
    // When
    val actual = medianOfThreePicker(array)
    // Then
    assert(actual === 5)
  }

  test("median-of-three pivot picker (odd), peek first") {
    // Given
    val array = Array(4, 2, 8, 5, 7, 3, 1)
    // When
    val actual = medianOfThreePicker(array)
    // Then
    assert(actual === 0)
  }

  test("median-of-three pivot picker (odd), peek middle") {
    // Given
    val array = Array(8, 2, 4, 5, 7, 3, 1)
    // When
    val actual = medianOfThreePicker(array)
    // Then
    assert(actual === 3)
  }

  test("median-of-three pivot picker (odd), peek last") {
    // Given
    val array = Array(1, 2, 8, 6, 7, 3, 5)
    // When
    val actual = medianOfThreePicker(array)
    // Then
    assert(actual === 6)
  }

  test("Sort assignment with median-of-three pivot picker") {
    new SampleData {
      // Given
      val actual = loadArray("week2/QuickSort.txt")
      val picker = medianOfThreePicker
      // When
      val comparisons = sort(picker)(actual)
      // Then
      assert(checkSorted(actual))
      info(s"# of comparisons: ${comparisons}")
    }
  }

}
