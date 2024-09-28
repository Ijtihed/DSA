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

import unionFind._

class UnionFindSpec extends AnyFlatSpec with Matchers:
  /*
   * A helper method for generating pseudorandom edge-weighted graphs.
   */
  def randomEdges(nofVertices: Int, nofEdges: Int, minW: Int, maxW: Int,
                  seed: Int): collection.Seq[(Int, Int, Int)] =
    require(minW <= maxW)
    val rand = new PRNG(seed)
    val edges = new scala.collection.mutable.ArrayBuffer[(Int, Int, Int)]()
    while edges.length < nofEdges do {
      val vertex1 = rand.nextInt(nofVertices)
      val vertex2 = rand.nextInt(nofVertices)
      if vertex1 != vertex2 then {
        val w = rand.nextInt(maxW - minW + 1) + minW
        edges += ((vertex1, w, vertex2))
      }
    }
    edges.toSeq
  end randomEdges


  "The solve method" should "be correct" in {
    val tests = List((6, List((0, 1, 1), (0, 5, 2), (1, 7, 2), (2, 4, 3), (3, 10, 4), (3, 12, 5), (4, 2, 5)), Some(4))
                     ,(6, List((0, 1, 1), (0, 5, 2), (1, 7, 2), (3, 10, 4), (3, 12, 5), (4, 2, 5)), None)
                     ,(6, List((0, 3, 1), (1, 8, 2), (2, 9, 3), (3, 4, 4), (4, 17, 5), (5, 6, 0), (5, 5, 2)), Some(5))
                   )
    for (nofVertices, edges, correct) <- tests do
      val result = solver.solve(nofVertices, edges)
      result should be (correct)
  }

  it should "correct and efficient" in {
    val tests = List((100000, 1000000, 1, 1000, 1, Some(403))
                     ,(100000, 1000000, 1, 1000, 2, Some(339))
                     ,(100000, 1000000, 1, 1000, 3, Some(315))
                     ,(100000, 1000000, 1, 1000, 4, Some(440))
                     ,(100000, 1000000, 1, 1000, 5, Some(449))
                     ,(100000, 1000000, 1, 1000, 6, Some(327))
                     ,(100000, 1000000, 1, 1000, 7, Some(474))
                     ,(100000, 1000000, 1, 1000, 8, Some(277))
                     ,(100000, 1000000, 1, 1000, 9, Some(273))
                     ,(100000, 1000000, 1, 1000, 10, Some(436))
                   )
    for (nofVertices, nofEdges, minCap, maxCap, seed, correct) <- tests do
      println(s" Testing with edges = randomEdges($nofVertices, $nofEdges, $minCap, $maxCap, $seed)")
      val edges = randomEdges(nofVertices, nofEdges, minCap, maxCap, seed)
      val (result, time) = timer.measureWallClockTime {solver.solve(nofVertices, edges) }
      result should be (correct)
      println(s"  Time: $time")
      time should be <= (3.0)
  }

end UnionFindSpec
