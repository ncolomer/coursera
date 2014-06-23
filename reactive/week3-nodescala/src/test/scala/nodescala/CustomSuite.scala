package nodescala

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random.nextInt

@RunWith(classOf[JUnitRunner])
class CustomSuite extends FlatSpec with ScalaFutures {

  class TestException extends Exception

  "all" should "evaluate all futures" in {
    // Given
    val fs = List(
      Future {Thread.sleep(nextInt(100)); 1},
      Future {Thread.sleep(nextInt(100)); 2},
      Future {Thread.sleep(nextInt(100)); 3}
    )
    // When
    val all = Future.all(fs)
    // Then
    whenReady(all, Interval(500 millis)) { result =>
      assert(result.sum === 6)
    }
  }

  "all" should "return Nil if List is empty" in {
    // When
    val all = Future.all(Nil)
    // Then
    whenReady(all) { result =>
      assert(result === Nil)
    }
  }

  "all" should "return List with one future" in {
    // When
    val all = Future.all(List(Future {1}))
    // Then
    whenReady(all) { result =>
      assert(result === List(1))
    }
  }

  "all" should "return List with two futures" in {
    // When
    val all = Future.all(List(Future {1}, Future {2}))
    // Then
    whenReady(all) { result =>
      assert(result === List(1, 2))
    }
  }

  "all" should "fail with one Exception thrown" in {
    // When
    val all = Future.all(List(Future {throw new TestException}))
    // Then
    whenReady(all.failed) { result =>
      assert(result.isInstanceOf[TestException])
    }
  }

  "all" should "fail with one Future failing" in {
    // When
    val all = Future.all(List(Future {1}, Future {throw new TestException}))
    // Then
    whenReady(all.failed) { result =>
      assert(result.isInstanceOf[TestException])
    }
  }

  "all" should "fail with other Future failing" in {
    // When
    val all = Future.all(List(Future {throw new TestException}, Future {1}))
    // Then
    whenReady(all.failed) { result =>
      assert(result.isInstanceOf[TestException])
    }
  }

  "all" should "fail with all Future failing" in {
    // When
    val all = Future.all(List(Future {throw new TestException}, Future {throw new TestException}))
    // Then
    whenReady(all.failed) { result =>
      assert(result.isInstanceOf[TestException])
    }
  }

  "any" should "return first result" in {
    // Given
    val f1 = Future {Thread.sleep(10); 1}
    val f2 = Future.all(List(f1, Future {2}))
    val fs = List(f1, f2)
    // When
    val any = Future.any(fs)
    // Then
    whenReady(any, Timeout(100 millis)) { result =>
      assert(result === 1)
    }
  }

  "any" should "return second result" in {
    // Given
    val f1 = Future {Thread.sleep(10); 1}
    val f2 = Future.all(List(f1, Future {2}))
    val fs = List(f2, f1)
    // When
    val any = Future.any(fs)
    // Then
    whenReady(any, Timeout(100 millis)) { result =>
      assert(result === 1)
    }
  }

  "any" should "fail with Exception thrown first" in {
    // Given
    val f1 = Future {Thread.sleep(10); throw new TestException}
    val f2 = Future.all(List(f1, Future {1}))
    val fs = List(f1, f2)
    // When
    val any = Future.any(fs)
    // Then
    whenReady(any.failed, Timeout(100 millis)) { result =>
      assert(result.isInstanceOf[TestException])
    }
  }

  "any" should "return first result with Exception" in {
    // Given
    val f1 = Future {Thread.sleep(10); 1}
    val f2 = Future.all(List(f1, Future {throw new TestException}))
    val fs = List(f1, f2)
    // When
    val any = Future.any(fs)
    // Then
    whenReady(any, Timeout(100 millis)) { result =>
      assert(result === 1)
    }
  }

  "delay" should "wait the expected amount of time before complete" in {
    // When
    val stamp = System.currentTimeMillis
    val delay = Future.delay(100 millis)
    // Then
    whenReady(delay, Timeout(200 millis), Interval(1 millis)) { result =>
      val duration = System.currentTimeMillis - stamp
      assert(duration >= 90 && duration <= 110)
    }
  }

  "now" should "throw NoSuchElementException with None value" in {
    // When
    val stamp = Promise().future
    // Then
    intercept[NoSuchElementException](stamp.now)
  }

  "now" should "throw NoSuchElementException with Failure" in {
    // When
    val future = Future {throw new TestException}
    // Then
    intercept[NoSuchElementException](future.now)
  }

  "now" should "return value" in {
    // When
    val future = Future.successful(1)
    // Then
    assert(future.now === 1)
  }

}




