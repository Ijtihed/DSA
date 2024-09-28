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

package radixSort


/**
 * Least-significant-digit-first radix sort for integer arrays.
 * Sorts the argument array in ascending order.
 * In the basic version (for 80 percent of the points),
 * you may assumes that all the integers are non-negative.
 * Interpret 'digit' as 'byte' (8 bits) here and
 * use bit-level operations such as
 * shifting (>> and <<) and masking (&) to extract 'digits'.
 */

def lsdRadixSort(a: Array[Int]): Unit = {
  val n = a.length
  if n <= 1 then return

  val radix = 256
  var aux = a
  val aux2 = new Array[Int](n)

  var shift = 0
  while shift < 32 do {
    val count = Array.fill[Int](radix)(0)

    var index1 = 0
    while (index1 < n) do {
      val byteValue = (aux(index1) >> shift) & 0xFF
      count(byteValue) += 1
      index1 += 1
    }

    var index2 = 1
    while (index2 < radix) do {
      count(index2) += count(index2 - 1)
      index2 += 1
    }
    
    var index3 = n - 1
    while (index3 >= 0) do {
      val byteValue = (aux(index3) >> shift) & 0xFF
      aux2(count(byteValue) - 1) = aux(index3)
      count(byteValue) -= 1
      index3 -= 1
    }

    aux = aux2.clone()


    shift += 8
  }

  Array.copy(aux, 0, a, 0, n)
}


