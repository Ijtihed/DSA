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

import collection.mutable.ArrayBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import Ordering.Double.TotalOrdering

import radixSort._
import timer.measureWallClockTime

class RadixSortSpec extends AnyFlatSpec with Matchers:
  val rand = util.Random()

  /**
   * A helper for comparing java.util.Arrays.sort and radix sort.
   * Returns the speedup of the average running time of radix sort over java.util.Arrays.sort
   * on nofRuns runs on input arrays created with createInput(n).
   */
  private def compareSortAndRadix(nofRuns: Int, n: Int, createInput: Int => Array[Int], indent: String = " ") =
    require(nofRuns > 0)
    require(n > 0)
    val sortTimes = ArrayBuffer[Double]()
    val radixTimes = ArrayBuffer[Double]()
    for run <- 1 to nofRuns do
      val input = createInput(n)
      // Execute java.util.Arrays.sort
      val sortData = input.clone()
      val (_, sortTime) = measureWallClockTime { java.util.Arrays.sort(sortData) }
      // Execute radix sort
      val radixData = input.clone()
      val (_, radixTime) = measureWallClockTime { lsdRadixSort(radixData) }
      // Check correctness
      for i <- 0 until n do
        withClue(s"At index $i: ") {
          radixData(i) shouldBe sortData(i)
        }
      sortTimes += sortTime
      radixTimes += radixTime
    end for
    val sortAvg = sortTimes.sum / nofRuns
    val radixAvg = radixTimes.sum / nofRuns
    val speedup = sortAvg / radixAvg
    println(f"${indent}Average running times:")
    println(f"${indent} java.util.Arrays.sort: $sortAvg%.6f")
    println(f"${indent} radix sort:            $radixAvg%.6f")
    println(f"${indent} speedup: $speedup%.2f")
    speedup
  end compareSortAndRadix


  /*
   * The tests relevant for the grading process.
   */
  "The lsdRadixSort method" should "work correctly on arrays of non-negative integers" in {
    val nofTests = 200
    for testNum <- 1 to nofTests do
      // The number of elements in the test array
      val n = testNum
      // The test array of non-negative integers
      val a = Array.fill[Int](n)(rand.nextInt(Int.MaxValue))
      // Reference: array sorted with the "sorted" method
      val correct = a.sorted
      // Apply radix sort to the test array
      lsdRadixSort(a)
      // Check that the result corresponds to the reference
      for i <- 0 until n do
        withClue(s"At index $i: ") {
          a(i) shouldBe correct(i)
        }
  }


  it should "be faster than java.util.Arrays.sort on large arrays of non-negative integers" in {
    val nofTests = 10
    val n = 5000000
    println(s"Comparing to 'java.util.Arrays.sort' on $nofTests arrays of $n non-negative integers")
    def createInput(n: Int) = Array.fill[Int](n)(rand.nextInt(Int.MaxValue))
    val speedup = compareSortAndRadix(nofTests, n, createInput)
    speedup should be > (1.0)
  }


  it should "work correctly on arrays of arbitrary integers" in {
    val nofTests = 200
    for testNum <- 1 to nofTests do
      val n = testNum
      val a = Array.fill[Int](n)(rand.nextInt())
      val correct = a.sorted
      lsdRadixSort(a)
      //println(a.map(v => f"$v%08x").toList)
      for i <- 0 until n do
        withClue(s"At index $i: ") {
          a(i) shouldBe correct(i)
        }
  }


  it should "be faster than java.util.Arrays.sort on large arrays of arbitrary integers" in {
    val nofTests = 10
    val n = 5000000
    println(s"Comparing to 'java.util.Arrays.sort' on $nofTests arrays of $n arbitrary integers")
    def createInput(n: Int) = Array.fill[Int](n)(rand.nextInt())
    val speedup = compareSortAndRadix(nofTests, n, createInput)
    speedup should be > (1.0)
  }



  /**
   * Some additional tests for radix sort performance
   * on different kinds of input arrays.
   * These are not included in the grader.
   */


  it should "be at most 3 times slower than java.util.Arrays.sort on large arrays of non-negative integers drawn from a small set (not included in the grader)" in {
    val nofTests = 10
    val n = 5000000
    println(s"Comparing to 'java.util.Arrays.sort' on $nofTests arrays of $n non-negative integers drawn from a small set")
    val m = 21
    val values = IndexedSeq.fill[Int](m)(rand.nextInt(Int.MaxValue))
    def createInput(n: Int) = Array.fill[Int](n)(values(rand.nextInt(values.size)))
    val speedup = compareSortAndRadix(nofTests, n, createInput)
    speedup should be > (1.0 / 3.0)
  }

  it should "be faster than java.util.Arrays.sort on large almost-sorted arrays of non-negative integers (not included in the grader)" in {
    val nofTests = 10
    val n = 5000000
    println(s"Comparing to 'java.util.Arrays.sort' on $nofTests almost-sorted arrays of $n non-negative integers")
    def createInput(n: Int) =
      val data = Array.fill[Int](n)(rand.nextInt(Int.MaxValue))
      java.util.Arrays.sort(data)
      for _ <- 1 to n/1000 do
        val s = rand.nextInt(n)
        val t = rand.nextInt(n)
        val tmp = data(s)
        data(s) = data(t)
        data(t) = tmp
      data
    val speedup = compareSortAndRadix(nofTests, n, createInput)
    speedup should be > (1.0)
  }


end RadixSortSpec
