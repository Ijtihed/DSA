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

import nutsAndBolts._
import timer.measureWallClockTime

class MatcherSpec extends AnyFlatSpec with Matchers:
  var nofCompares = 0

  // A concrete class of nuts for testing
  // The number of comparisons is recorded for evaluation purposes
  protected case class EuroNut(name: String, size: Int) extends Nut(name):
    override def compare(bolt: Bolt): Int =
      nofCompares += 1
      bolt match
        case EuroBolt(boltName, boltSize) =>
          if size < boltSize then
            -1
          else if size == boltSize then
            0
          else
            1
        case _ =>
          assert(false, "can only compare EuroNuts to EuroBolts")
          0 // Just for type checking
    end compare
  end EuroNut

  protected case class EuroBolt(name: String, size: Int) extends Bolt(name) {}

  val rand = util.Random()

  /* A small helper for generating random Euro nuts and bolts */
  def randomNutsAndBolts(n: Int, maxSize: Int): (Vector[EuroNut],Vector[EuroBolt]) =
    val sizes = Array.fill[Int](n)(rand.nextInt(maxSize))
    val nuts = rand.shuffle(sizes).zipWithIndex.map({case (s,i) => EuroNut("n"+i, s)}).toVector
    val bolts = rand.shuffle(sizes).zipWithIndex.map({case (s,i) => EuroBolt("b"+i, s)}).toVector
    (nuts, bolts)
  end randomNutsAndBolts


  "The slow matcher" should "work correctly" in {
    val nofTests = 1000
    for test <- 1 to nofTests do
      val n = 5 + rand.nextInt(20)
      val maxSize = 5 + rand.nextInt(n)
      val (nuts, bolts) = randomNutsAndBolts(n, maxSize)
      val result = matcher.slow(nuts, bolts)
      withClue(s"When nuts=$nuts, bolts=$bolts, and result=$result:") {
        result.map(_._1).toSet should be (nuts.toSet)
        result.map(_._2).toSet should be (bolts.toSet)  
        result.foreach(p => p._1.compare(p._2) should be (0))
      }
    end for
  }

  "The fast matcher" should "work correctly" in {
    val nofTests = 1000
    for test <- 1 to nofTests do
      val n = 5 + rand.nextInt(20)
      val maxSize = 5 + rand.nextInt(n)
      val (nuts, bolts) = randomNutsAndBolts(n, maxSize)
      val result = matcher.fast(nuts, bolts)
      withClue(s"When nuts=$nuts, bolts=$bolts, and result=$result:") {
        // The length should match
        result.length should be (nuts.length)
        // All the nuts should be included
        result.map(_._1).toSet should be (nuts.toSet)
        // All the bolts should be included
        result.map(_._2).toSet should be (bolts.toSet)  
        // The pairs should be matching
        result.foreach(p => p._1.compare(p._2) should be (0))
      }
    end for
  }

  it should "be significantly faster than the quadratic time algorithm on largish instances and only use at most 4*n*log(n) comparisons" in {
    val nofRuns = 10
    val n = 50000
    val requiredSpeedup = 2.0
    val maxSize = 100000
    var slowCumuTime = 0.0
    var fastCumuTime = 0.0
    var fastCompares = 0
    for run <- 1 to nofRuns do
      println(s"Run $run")
      // Build the test instance
      val (nuts, bolts) = randomNutsAndBolts(n, maxSize)
      // Execute the slow algorithm
      nofCompares = 0
      val (slowResult, slowTime) = measureWallClockTime { matcher.slow(nuts, bolts) }
      println(f" Slow algorithm time: $slowTime%.3f")
      println(f" Slow algorithm comparisons: $nofCompares")
      slowCumuTime += slowTime
      // Execute the fast algorithm
      nofCompares = 0
      val (fastResult, fastTime) = measureWallClockTime { matcher.fast(nuts, bolts) }
      println(f" Fast algorithm time: $fastTime%.3f")
      fastCumuTime += fastTime
      val maxComparisons = (4*n*math.log(n)/math.log(2)).toInt
      println(f" Fast algorithm comparisons: $nofCompares (at most $maxComparisons allowed)")
      // Check correctness
      nofCompares should be <= (maxComparisons)
      fastCompares += nofCompares
      fastResult.length should be (nuts.length)
      fastResult.map(_._1).toSet should be (nuts.toSet)
      fastResult.map(_._2).toSet should be (bolts.toSet)    
      fastResult.foreach(p => p._1.compare(p._2) should be (0))
    end for
    println("Cumulative running times:")
    println(f" Slow algorithm: $slowCumuTime%.3f")
    println(f" Fast algorithm: $fastCumuTime%.3f")
    val speedup = slowCumuTime / fastCumuTime
    println(f"Speedup: $speedup%.2f")
    speedup should be >= (requiredSpeedup)
  }

end MatcherSpec
