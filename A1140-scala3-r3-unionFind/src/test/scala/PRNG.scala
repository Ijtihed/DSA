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

/*
 * A fixed pseudorandom number generator for the sake of reproducibility
 * (in case there are several JDKs out there with different implementations).
 */
class PRNG(initialSeed: Int):
  private val multiplier: Long = 0x5DEECE66DL
  private val addend: Long = 0xBL
  private val mask: Long = (1L << 48) - 1
  private var seed: Long = (initialSeed ^ multiplier) & mask

  private def next(bits: Int): Int =
    seed = (seed * multiplier + addend) & mask
    (seed >>> (48 - bits)).toInt

  def nextInt(bound: Int): Int =
    require(bound > 0)
    // Bound is a power of 2?
    if (bound & -bound) == bound then
      return ((bound * next(31).toLong) >> 31).toInt
    var bits = 0
    var v = 0
    while
      bits = next(31)
      v = bits % bound
      (bits - v + (bound-1) < 0)
    do ()
    return v
  end nextInt

end PRNG
