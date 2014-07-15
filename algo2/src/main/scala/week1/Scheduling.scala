package week1

import utils.Tools._

object Scheduling {

  case class Job(weight:Int, length:Int)

  private def loadJobs(file: String): List[Job] = {
    val lines = loadTextFile(file)
    val size = lines.next().toInt
    val jobs = lines.toList
      .map {_.split("\\s").map {_.trim.toInt}}
      .collect {case Array(w,l) => Job(w,l)}
    assume(jobs.size == size)
    jobs
  }

  private def schedule(score: Job => Float)(file: String): List[Job] = {
    val jobs = loadJobs(file)
    jobs.sortWith((job1, job2) => {
      val (score1, score2) = (score(job1), score(job2))
      if (score1 != score2) score1 > score2
      else job1.weight > job2.weight // Tie fallback
    })
  }

  def schedule1 = schedule(job => job.weight - job.length) _
  def schedule2 = schedule(job => job.weight.toFloat / job.length) _

}
