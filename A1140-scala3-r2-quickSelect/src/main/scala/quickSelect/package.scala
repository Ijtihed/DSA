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

package quickSelect


// A pseudo-random number generator with a fixed seed
// so that error situations can be reproduced easier
val rand = util.Random(21)

/**
 * Find the k:th smallest (0 for the smallest) element in
 * the integer sequence seq by using the quickselect algorithm.
 */
def find(seq: collection.immutable.Seq[Int], k: Int): Int =
  require(0 <= k && k < seq.length)
  // Make a working copy of the sequence as we cannot modify the argument sequence
  val a: Array[Int] = seq.toArray

  def partition3Way(l: Int, r: Int, pivotIndex: Int): (Int, Int) = {
    val pivotValue = a(pivotIndex)
    a(pivotIndex) = a(r)
    a(r) = pivotValue
    
    var lt = l
    var gt = r
    var i = l
    
    while (i <= gt) {
      if (a(i) < pivotValue) {
        val tmp = a(i)
        a(i) = a(lt)
        a(lt) = tmp
        lt += 1
        i += 1
      } else if (a(i) > pivotValue) {
        val tmp = a(i)
        a(i) = a(gt)
        a(gt) = tmp
        gt -= 1
      } else {
        i += 1
      }
    }
    (lt, gt)
  }

  def quickSelect(l: Int, r: Int, k: Int): Int = {
    if (l == r) return a(l)
    val pivotIndex = l + rand.nextInt(r - l + 1)
    val (lt, gt) = partition3Way(l, r, pivotIndex)
    
    if (k < lt) {
      quickSelect(l, lt - 1, k)
    } else if (k > gt) {
      quickSelect(gt + 1, r, k)
    } else {
      a(k)
    }
  }

  quickSelect(0, a.length - 1, k)
end find

