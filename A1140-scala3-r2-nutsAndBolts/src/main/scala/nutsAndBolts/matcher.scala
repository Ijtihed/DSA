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

import collection.mutable.ArrayBuffer
import reflect.ClassTag

object matcher:
  /**
   * Find matching pairs for the nuts and bolts.
   * A straightforward, quadratic-time algorithm given
   * for reference when measuring performance.
   */
  def slow[NutType <: Nut : ClassTag, BoltType <: Bolt : ClassTag](nuts: Vector[NutType], bolts: Vector[BoltType]): Vector[(NutType,BoltType)] =
    require(nuts.length == bolts.length)
    val result = ArrayBuffer[(NutType,BoltType)]()
    val unmatchedBolts = bolts.to(ArrayBuffer)
    for nut <- nuts do
      val index = unmatchedBolts.indexWhere(_.compare(nut) == 0)
      assert(index != -1)
      // Found a matching bolt, append the pair to the result
      result += ((nut, unmatchedBolts(index)))
      // Remove the bolt from the unmatched array buffer by
      // moving it to the end of the current array buffer and
      // shrinking the array buffer by one
      unmatchedBolts(index) = unmatchedBolts.last
      unmatchedBolts.dropRightInPlace(1)
    end for
    result.toVector
  end slow

  /**
   * The faster algorithm based on a variant of quicksort.
   */
  def fast[NutType <: Nut : ClassTag, BoltType <: Bolt : ClassTag](nuts: Vector[NutType], bolts: Vector[BoltType]): Vector[(NutType,BoltType)] =
    require(nuts.length == bolts.length)

    val nutList = nuts.to(ArrayBuffer)
    val boltList = bolts.to(ArrayBuffer)
    val pairings = ArrayBuffer[(NutType,BoltType)]()


    def quickPartition(low: Int, high: Int): Unit =

      if high < low then return
      if high == low then
        pairings += ((nutList(low), boltList(low)))
        return

      val pivotIdx = util.Random.between(low,  high + 1)
      val pivotNut = nutList(pivotIdx)
      var index = low
      var rightBolt = high
      var leftBolt = low
      while index <= rightBolt do
        val comparison = boltList(index).compare(pivotNut)
        if comparison == -1 then
          val temp = boltList(leftBolt)
          boltList(leftBolt) = boltList(index)
          boltList(index)=temp
                    index += 1
          leftBolt += 1
        else if comparison == 1 then
          val temp =boltList(rightBolt)
          boltList(rightBolt) = boltList(index)
          rightBolt -= 1
          boltList(index) =temp
        else
          index += 1

      val pivotBolt = boltList(leftBolt)
      var rightNut = high
      var leftNut = low
      index = low

      while index <=rightNut do
        val comparison = nutList(index).compare(pivotBolt)
        if comparison == -1 then
          val temp = nutList(leftNut)
          nutList(leftNut) = nutList(index)
          nutList(index) =temp
          leftNut += 1
          index += 1
        else if comparison == 1 then
          val temp = nutList(rightNut)
          nutList(rightNut) = nutList(index)
          nutList(index) =temp
          rightNut -= 1
        else
          index += 1
      for j <- leftNut to rightNut do
        pairings += ((nutList(j), boltList(j)))

      quickPartition(low, leftBolt - 1)
      quickPartition(rightBolt + 1, high)

    quickPartition(0, nuts.length - 1)
    pairings.toVector
  end fast

end matcher
