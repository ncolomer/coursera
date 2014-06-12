package week1

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StrassenMultiplicationTest extends FunSuite {

  import scala.week1.StrassenMultiplication._

  test("Matrix contains expected elements using get") {
    val m = new SimpleMatrix(2, 2).populate(1, 2, 3, 4)
    assert(m.get(0, 0) == 1)
    assert(m.get(0, 1) == 2)
    assert(m.get(1, 0) == 3)
    assert(m.get(1, 1) == 4)
  }

  test("Matrices are equals") {
    val m1 = new SimpleMatrix(2, 2).populate(1, 2, 3, 4)
    val m2 = new SimpleMatrix(2, 2).populate(1, 2, 3, 4)
    assert(m1 equals m2)
  }

  test("Matrix set runs as expected") {
    val m = new SimpleMatrix(2, 2)
    m.set(0, 1, 5)
    assert(m.get(0, 1) == 5)
  }

  test("sub on SquareMatrix returns expected matrix") {
    val m = new SquareMatrix(2)
    m.populate(1, 2, 3, 4)
    val actual: SquareMatrix = m.sub(0, 1, 1, 2)
    assert(actual.m == 1)
    assert(actual.n == 1)
    assert(actual.get(0, 0) == 2)
  }

  test("Sum of SquareMatrix") {
    val m1 = new SquareMatrix(2).populate(1, 2, 3, 4)
    val m2 = new SquareMatrix(2).populate(4, 3, 2, 1)
    val expected = new SquareMatrix(2).populate(5, 5, 5, 5)
    val actual: SquareMatrix = m1 + m2
    assert(expected == actual)
  }

  test("Subtraction of SquareMatrix") {
    val m1 = new SquareMatrix(2).populate(1, 2, 3, 4)
    val m2 = new SquareMatrix(2).populate(4, 3, 2, 1)
    val expected = new SquareMatrix(2).populate(-3, -1, 1, 3)
    val actual: SquareMatrix = m1 - m2
    assert(expected == actual)
  }

  test("Strassen multiplication with 2*2 matrix") {
    // {{1,2},{3,4}}*{{6,9},{2,5}}
    val m1 = new SquareMatrix(2).populate(1, 2, 3, 4)
    val m2 = new SquareMatrix(2).populate(6, 9, 2, 5)
    val expected = new SquareMatrix(2).populate(10, 19, 26, 47)
    val actual = m1 * m2
    assert(expected == actual)
  }

  test("Strassen multiplication with 4*4 matrix") {
    // {{1,2,4,2},{3,9,4,3},{2,5,6,4},{3,7,1,8}}*{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16}}
    val m1 = new SquareMatrix(4).populate(1, 2, 4, 2, 3, 9, 4, 3, 2, 5, 6, 4, 3, 7, 1, 8)
    val m2 = new SquareMatrix(4).populate(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val expected = new SquareMatrix(4).populate(73, 82, 91, 100, 123, 142, 161, 180, 133, 150, 167, 184, 151, 170, 189, 208)
    val actual = m1 * m2
    assert(expected == actual)
  }

}
