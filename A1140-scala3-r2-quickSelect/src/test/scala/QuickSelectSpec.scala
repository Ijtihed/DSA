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

import util.Random
import timer.measureWallClockTime

class QuickSelectSpec extends AnyFlatSpec with Matchers:
  val rand = Random()

  "Your quickselect implementation" should "compute correct results on some selected inputs" in {
    val tests = List(Vector(1)
                     ,Vector(1,1,1,1)
                     ,Vector(1,2,3)
                     ,Vector(3,2,1)
                   )
    for test <- tests; k <- 0 until test.length do
      val result = quickSelect.find(test, k)
      val correct = test.sorted.apply(k)
      withClue(s"On quickSelect.find($test, $k): ") {
        result should be (correct)
      }
  }


  it should "compute correct results on some random inputs" in {
    val maxValue = 100
    val nofTests = 1000
    for testNum <- 1 to nofTests do
      val n = testNum
      val test = Vector.fill[Int](n)(rand.nextInt(maxValue))
      val k = rand.nextInt(n)
      val correct = test.sorted.apply(k)
      val result = quickSelect.find(test, k)
      withClue(s"On quickSelect.find($test, $k): ") {
        result should be (correct)
      }
  }

  /**
   * A helper function to compare the performance of quickSelect.find and
   * a sorting-based approach using java.util.Arrays.sort.
   */
  def runPerformanceTests(n: Int, nofTests: Int, createInput: Int => collection.immutable.IndexedSeq[Int]): Double = {
    require(n >= 2)
    require(nofTests >= 1)
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    for testNum <- 1 to nofTests do
      println(s" Test $testNum")
      val test = createInput(n)
      val k = rand.nextInt(n)
      val (valueArraysSort, timeArraysSort) = measureWallClockTime {
        val tmp = test.toArray
        java.util.Arrays.sort(tmp)
        tmp(k)
      }
      val (valueQuickSelect,timeQuickSelect) = measureWallClockTime {
        quickSelect.find(test, k)
      }
      valueQuickSelect should be (valueArraysSort)
      println(f"  quickSelect: $timeQuickSelect%.6f")
      println(f"  Arrays.sort: $timeArraysSort%.6f")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    end for

    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average:")
    println(f"  quickSelect: $avgQuickSelect%.6f")
    println(f"  Arrays.sort: $avgArraysSort%.6f")
    val speedup = avgArraysSort / avgQuickSelect
    println(f" Speedup: $speedup%.2f")
    speedup
  }

  it should "be somewhat faster than the approach based on applying java.util.Arrays.sort on longish sequences of integers" in {
    def createInput(n: Int) =
      Vector.fill[Int](n)(rand.nextInt())
    val n = 1000000
    println(f"Running performance tests on sequences of $n random integers")
    val speedup = runPerformanceTests(n, 10, createInput)
    speedup should be >= (1.5)
  }


  it should "be as fast as the approach based on applying java.util.Arrays.sort on longish almost-sorted sequences of integers" in {
    def createInput(n: Int) =
      val data = Array.fill[Int](n)(rand.nextInt(Int.MaxValue))
      java.util.Arrays.sort(data)
      def swap(i: Int, j: Int): Unit =
        val tmp = data(i)
        data(i) = data(j)
        data(j) = tmp      
      // Swap some random pairs
      for f <- 1 to n/100 do
        swap(rand.nextInt(n), rand.nextInt(n))
      data.toIndexedSeq
    end createInput
    val n = 1000000
    println("Running performance tests on almost-sorted sequences")
    val speedup = runPerformanceTests(n, 10, createInput)
    speedup should be >= (1.0)
  }


  it should "be as fast as the approach based on applying java.util.Arrays.sort on sequences of small integers (=> lots of equal values in the array)" in {
    val maxValue = 21
    def createInput(n: Int) =
      Vector.fill[Int](n)(rand.nextInt(maxValue))
    val n = 1000000
    println("Running performance tests on sequences with few values only")
    val speedup = runPerformanceTests(n, 10, createInput)
    speedup should be >= (1.0)
  }

end QuickSelectSpec
