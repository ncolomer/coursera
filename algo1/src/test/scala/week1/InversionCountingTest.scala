package week1

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import utils.Tools._

@RunWith(classOf[JUnitRunner])
class InversionCountingTest extends FunSuite {

  import scala.week1.InversionCounting._

  def print(input: Array[Int], total: Long, timeUs: Long) {
    info("Input: Array(%d)".format(input.length))
    info("Result: %d".format(total))
    info("Took: %dµs".format(timeUs))
  }

  test("Simple test - sample1") {
    val input = Array(1, 3, 5, 2, 4, 6)
    val (total, timeUs) = time(countInversion(input))
    assert(total == 3l)
    print(input, total, timeUs)
  }

  test("Assignment test") {
    val input = loadIntArray("week1/IntegerArray.txt")
    val (total, timeUs) = time(countInversion(input))
    assert(total == 2407905288l)
    print(input, total, timeUs)
  }

}
