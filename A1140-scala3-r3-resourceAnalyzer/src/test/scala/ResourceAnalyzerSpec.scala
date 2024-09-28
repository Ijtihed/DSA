// Scala 3 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University
 * This file is only for your personal use in the Aalto course CS-A1140.
 * Distribution of any parts of this file in any form,
 * including posting to public or shared forums,
 * storing in public or shared repositories,
 * is *not allowed*,
 * and constitutes a violation of the code of conduct of the course.
 * In addition, entering any parts of this file or the assignment instructions
 * into an AI tool, or to otherwise distributing them to external parties,
 * is *not allowed*.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import Ordering.Double.TotalOrdering

import timer._
import resourceAnalyzer._

class ResourceAnalyzerSpec extends AnyFlatSpec with Matchers:
  val rand = new scala.util.Random(2024)

  val simpleTestInstances = Seq(
    (List(Task(1000,1000),Task(1010,1000),Task(1999,1000)), 3)
    ,(List(Task(1000,1000),Task(1010,1000),Task(2000,1000)), 2)
    ,(List(Task(1,2),Task(1,2),Task(3,2),Task(3,1),Task(3,3)), 3)
    ,(List(Task(1,2),Task(1,2),Task(1,3),Task(3,1),Task(5,3)), 3)
    ,(List(Task(154,437),Task(233,409),Task(872,378)), 2)
    ,(List(Task(415,433),Task(468,85),Task(682,903)), 2)
    ,(List(Task(415,433),Task(468,214),Task(682,903)), 2)
    ,(List(Task(415,433),Task(468,215),Task(682,903)), 3)
  )

  "The class ResourceAnalyzerSlow" should "work correctly" in {
    for (instance, solution) <- simpleTestInstances do
      withClue(s"On instance $instance") {
        val analyzer = new ResourceAnalyzerSlow()
        for task <- instance do
          analyzer.newTask(task)
          analyzer.minResources
        analyzer.minResources should be (solution)
      }
  }
  
  "The class ResourceAnalyzerFast" should "work correctly" in {
    for (instance, solution) <- simpleTestInstances do
      withClue(s"On instance $instance") {
        val analyzer = new ResourceAnalyzerFast()
        for task <- instance do
          analyzer.newTask(task)
          analyzer.minResources
        analyzer.minResources should be (solution)
      }
  }

  it should "be significantly faster than ResourceAnalyzerSlow on inputs 60000 tasks spanning 10000 time units" in {
    val requiredSpeedup = 5.0
    val nofTasks = 60000
    val timeSpan = 10000
    val nofTests = 15
    var timeCumu = 0.0
    var timeSlowCumu = 0.0
    println("Doing performance tests")
    for testNum <- 1 to nofTests do
      println(s"Test $testNum")
      // Generate a random instance
      val instance = Seq.fill[Task](nofTasks)({
        val start = rand.nextInt(timeSpan-500)
        val duration = rand.nextInt(500)+1
        Task(start, duration)
      }).sortBy(_.start)
      val (solutionSlow, timeSlow) = measureWallClockTime {
        val analyzer = new ResourceAnalyzerSlow()
        for task <- instance do {
          analyzer.newTask(task)
          analyzer.minResources
        }
        analyzer.minResources
      }
      val (solution, time) = measureWallClockTime {
        val analyzer = new ResourceAnalyzerFast()
        for task <- instance do {
          analyzer.newTask(task)
          analyzer.minResources
        }
        analyzer.minResources
      }
      //println(f" time vs timeSlow: $time%.6f vs $timeSlow%.6f")
      println(f" Slow: $timeSlow%.6f")
      println(f" Fast: $time%.6f")
      solution should be (solutionSlow)
      timeCumu += time
      timeSlowCumu += timeSlow
    end for

    val timeAvg = timeCumu / nofTests
    val timeSlowAvg = timeSlowCumu / nofTests
    println("Average times:")
    println(f" Slow: $timeSlowAvg%.6f")
    println(f" Fast: $timeAvg%.6f")
    //  println(f" timeAvg vs timeSlowAvg: $timeAvg%.6f vs $timeSlowAvg%.6f")
    val speedup = timeSlowAvg / timeAvg
    println(f" Speedup: $speedup%.2f")
    speedup should be >= (requiredSpeedup)
  }
end ResourceAnalyzerSpec
