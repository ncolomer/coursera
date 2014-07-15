package week1

import org.scalatest.FunSuite

class Scheduling$Test extends FunSuite {

  import week1.Scheduling._

  def sumOfWeightedCompletionTime(jobs:List[Job]): Long =
    jobs.foldLeft((0l,0l)) {case ((acc, time), job) => {
      val completionTime = time + job.length
      (acc + job.weight * completionTime, completionTime)
    }}._1

  test("sample1") {
    // When
    val actual1 = schedule1("week1/jobs-sample1.txt")
    val actual2 = schedule2("week1/jobs-sample1.txt")
    // Then
    assertResult(23) {sumOfWeightedCompletionTime(actual1)}
    assertResult(22) {sumOfWeightedCompletionTime(actual2)}
  }

  test("sample2") {
    // When
    val actual1 = schedule1("week1/jobs-sample2.txt")
    val actual2 = schedule2("week1/jobs-sample2.txt")
    // Then
    assertResult(11336) {sumOfWeightedCompletionTime(actual1)}
    assertResult(10548) {sumOfWeightedCompletionTime(actual2)}
  }

  test("sample3") {
    // When
    val actual1 = schedule1("week1/jobs-sample3.txt")
    val actual2 = schedule2("week1/jobs-sample3.txt")
    // Then
    assertResult(145924) {sumOfWeightedCompletionTime(actual1)}
    assertResult(138232) {sumOfWeightedCompletionTime(actual2)}
  }

  test("assignment") {
    // When
    val actual1 = schedule1("week1/jobs.txt")
    val actual2 = schedule2("week1/jobs.txt")
    // Then
    info(s"schedule1 weighted completion time sum is ${sumOfWeightedCompletionTime(actual1)}")
    info(s"schedule2 weighted completion time sum is ${sumOfWeightedCompletionTime(actual2)}")
  }

}
