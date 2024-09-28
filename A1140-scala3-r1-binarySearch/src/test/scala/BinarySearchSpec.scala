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

import binarySearch._

class BinarySearchSpec extends AnyFlatSpec with Matchers:
  /**
   * Compute the ceiling of log2(x).
   * Due to rounding errors, math.ceil(math.log(x)/math.log(2))
   * would report a wrong answer when x = 536870912.
   */
  def log2ceil(x: Int): Int =
    require(x > 0)
    32 - Integer.numberOfLeadingZeros(x-1)

  private class IntWrapper(private val v: Int):
    def cmp(that: IntWrapper): Int = v.compare(that.v)

    override def equals(that: Any): Boolean =
      assert(false, "You should not use the == operator to compare array elements in your solution, use ord.compare instead.")
      false

    override def toString: String =
      assert(false, "You should not use the toString method to access the array element contents in your solution.")
      ""

    def asString: String = v.toString
  end IntWrapper

  /*
   * A wrapper for Int Ordering that also counts the number of comparisons.
   */
  var nofComparisons = 0
  private object OrderingWithCount extends Ordering[IntWrapper]:
    def compare(x: IntWrapper, y: IntWrapper): Int =
      nofComparisons += 1
      x.cmp(y)

  private object IntOrderingWithCount extends Ordering[Int]:
    def compare(x: Int, y: Int): Int =
      nofComparisons += 1
      x.compare(y)


  "The searchLow method" should "work correctly 1" in {
    val tests = Array[(Array[Int],Int)](
      (Array(1,2,3,4), 0),
      (Array(1,2,3,4), 1),
      (Array(1,2,3,4), 2),
      (Array(1,2,3,4), 3),
      (Array(1,2,3,4), 4),
      (Array(1,2,3,4), 5)
    )
    for (data, low) <- tests do
      val n = data.length
      // Reset the comparisons counter
      nofComparisons = 0
      // Call the search routine, count the comparisons
      val result = searchLow(data, low)(using IntOrderingWithCount)
      val comparisonsUsed = nofComparisons
      // Check whether everything is okay
      withClue(s"""searchLow(Array(${data.mkString(",")}),$low) = $result: """) {
        if data(n-1) < low then
          withClue(s"Expected None but got $result: ") {
            result should be (None)
          }
        else
          withClue("Got None instead of the correct index: ") {
            result should not be (None)
          }
          val lowIndex = result.get
          assert(0 <= lowIndex)
          assert(lowIndex < n)
          data(lowIndex) should be >= (low)
          if 0 < lowIndex then
            data(lowIndex - 1) should be < (low)

        val nofAllowedComparisons = log2ceil(n)+1
        withClue("Too many comparisons: ") {
          comparisonsUsed should be <= (nofAllowedComparisons)
        }
      }
    end for
  }

  it should "work correctly 2" in {
    val seed = 2021
    val rand = new scala.util.Random(seed)
    val nofTests = 1000
    for test <- 1 to nofTests do
      val n = test * 10
      val offset = rand.nextInt(2*n-n)
      val range = 2*n
      val data = Array.fill[IntWrapper](n)(new IntWrapper(rand.nextInt(range)-offset)).sorted(OrderingWithCount)
      val low = new IntWrapper(rand.nextInt(range+n/3)-offset)
      // Reset the comparisons counter
      nofComparisons = 0
      // Call the search routine, count the comparisons
      val result = searchLow(data, low)(using OrderingWithCount)
      // Save the number of comparisons used (will be incremented by the comparisons below)
      val comparisonsUsed = nofComparisons
      // Check whether everything is okay
      withClue(s"""searchLow(Array(${data.map(_.asString).mkString(",")}),${low.asString}) returned $result: """) {
        if OrderingWithCount.compare(data(n-1), low) < 0 then {
          withClue(s"Expected None but got $result: ") {
            result shouldBe None
          }
        }
        else {
          withClue("Got None instead of the correct index: ") {
            result should not be None
          }
          val lowIndex = result.get
          assert(0 <= lowIndex)
          assert(lowIndex < n)
          OrderingWithCount.compare(data(lowIndex), low) should be >= 0
          if 0 < lowIndex then
            OrderingWithCount.compare(data(lowIndex - 1), low) should be < 0
        }
        val nofAllowedComparisons = log2ceil(n)+1
        withClue("Too many comparisons: ") {
          comparisonsUsed should be <= (nofAllowedComparisons)
        }
      }
    end for
  }
end BinarySearchSpec
