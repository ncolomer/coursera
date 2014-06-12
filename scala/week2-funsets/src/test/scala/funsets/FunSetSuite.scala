package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val negatives = {x: Int => x < 0}
    val positives = {x: Int => x > 0}
    val even = {x: Int => x % 2 == 0}
    val odd = {x: Int => x % 2 == 1}
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1")
      assert(contains(s2, 2), "Singleton 2")
      assert(contains(s3, 3), "Singleton 3")
    }
  }

  test("singletonSet(1) does not contain 2") {
    new TestSets {
      assert(!contains(s1, 2), "Singleton 1")
    }
  }

  test("union with itself contains all elements") {
    new TestSets {
      val s = union(s1, s1)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union with different contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection with itself contains all elements") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Intersection 1")
      assert(!contains(s, 2), "Intersection 2")
      assert(!contains(s, 3), "Intersection 3")
    }
  }

  test("intersection with different contains no element") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersection 1")
      assert(!contains(s, 2), "Intersection 2")
      assert(!contains(s, 3), "Intersection 3")
    }
  }

  test("diff with different contains all elements") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("diff with itself contains no element") {
    new TestSets {
      val s = diff(s1, s1)
      assert(!contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter always true") {
    new TestSets {
      val s = filter(s1, x => true)
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
    }
  }

  test("filter always false") {
    new TestSets {
      val s = filter(s1, x => false)
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
    }
  }

  test("filter after union") {
    new TestSets {
      val u = union(s1, s2)
      val s = filter(u, x => s2(x))
      assert(!contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
    }
  }

  test("filter pass through") {
    new TestSets {
      val s = filter(s3, x => s3(x))
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
    }
  }

  test("forall included singleton") {
    new TestSets {
      assert(forall(s1, positives), "Forall 1")
    }
  }

  test("forall not included singleton") {
    new TestSets {
      assert(!forall(s1, negatives), "Forall 1")
    }
  }

  test("forall included range") {
    new TestSets {
      assert(forall(positives, x => true), "Forall 1")
    }
  }

  test("forall included range itself") {
    new TestSets {
      assert(forall(positives, positives), "Forall 1")
    }
  }

  test("forall not included range") {
    new TestSets {
      assert(!forall(positives, x => false), "Forall 1")
    }
  }

  test("forall included discontinued range") {
    new TestSets {
      assert(forall(odd, x => true), "Forall 1")
    }
  }

  test("forall included discontinued range itself") {
    new TestSets {
      assert(forall(odd, odd), "Forall 1")
    }
  }

  test("forall not included discontinued range") {
    new TestSets {
      assert(!forall(odd, x => false), "Forall 1")
    }
  }

  test("exists included singleton") {
    new TestSets {
      assert(exists(s1, positives), "Exists 1")
    }
  }

  test("exists included singleton itself") {
    new TestSets {
      assert(exists(s1, s1), "Exists 1")
    }
  }

  test("exists not included singleton") {
    new TestSets {
      assert(!exists(s1, negatives), "Exists 1")
    }
  }

  test("exists included continued range") {
    new TestSets {
      assert(exists(positives, s1), "Exists 1")
    }
  }

  test("exists not included continued range") {
    new TestSets {
      assert(!exists(positives, negatives), "Exists 1")
    }
  }

  test("exists included discontinued range") {
    new TestSets {
      assert(exists(odd, s1), "Exists 1")
    }
  }

  test("exists not included discontinued range") {
    new TestSets {
      assert(!exists(even, s1), "Exists 1")
    }
  }

  test("map singleton 1 to singleton 2") {
    new TestSets {
      val s = map(s1, x => x + 1)
      assert(forall(s, s2), "Map 1")
    }
  }

  test("map positives to singleton 1") {
    new TestSets {
      val s = map(positives, x => 1)
      assert(forall(s, s1), "Map 1")
    }
  }

  test("map negatives to singleton 1") {
    new TestSets {
      val s = map(negatives, x => 1)
      assert(forall(s, s1), "Map 1")
    }
  }

  test("map odd to even") {
    new TestSets {
      val s = map(odd, x => x + 1)
      assert(forall(s, even), "Map 1")
      assert(!forall(s, odd), "Map 2")
    }
  }

  test("map positives to negatives") {
    new TestSets {
      val s = map(positives, x => -x)
      assert(forall(s, negatives), "Map 1")
      assert(!forall(s, positives), "Map 2")
    }
  }

}
