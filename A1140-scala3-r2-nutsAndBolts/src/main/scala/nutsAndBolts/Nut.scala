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

package nutsAndBolts

/**
 * An abstract base class for nuts.
 */
abstract class Nut(name: String):
  /**
   * Compare this nut to a bolt.
   * Returns -1 if this nut is smaller than, 0 if the same size as,
   * and 1 if larger than the bolt.
   */
  def compare(bolt: Bolt): Int
end Nut
