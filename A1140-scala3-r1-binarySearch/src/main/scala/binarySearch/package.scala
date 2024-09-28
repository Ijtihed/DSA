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

package binarySearch

/**
 * The classic recursive binary search, given here as a reference.
 * Returns true if and only if 'e' appears in the ordered array 'data'.
 */
def search[A](data: Array[A], e: A)(using ord: Ordering[A]): Boolean =
  /* The recursive inner function,
   * searching e in the sub-array data[from..to].
   */
  def inner(from: Int, to: Int): Boolean =
    if from <= to then
      // The sub-array data[from,to] contains at least one element.
      // Compute the mid-point.
      val mid = from + (to - from) / 2
      // Compare e to the element at the mid-point,
      // reuse the result multiple times below
      val cmp = ord.compare(e, data(mid))
      if cmp == 0 then
        // e == data(mid)
        // Found e, return true
        true
      else if cmp < 0 then
        // e < data(mid)
        // If e is in data, it is the "left" sub-array data[from..mid-1]
        inner(from, mid-1)
      else
        // e > data(mid)
        // If e is in data, it is the "right" sub-array data[mid+1..to]
        inner(mid+1, to)
    else
      // The sub-array data[from..to] is empty, e cannot be in it
      false
  end inner

  inner(0, data.length-1)
end search


/**
 * Returns the smallest index i in the ordered array data
 * such that low <= data(i), or
 * None if all the elements in data are smaller than low.
 * 
 * As in the "search" method above, use ord.compare(x,y) to compare x and y.
 * Do NOT use == to compare two elements of the type A:
 * they can be objects, and then == evaluates to true if they are
 * the same object, not if their values are equal in the ordering "ord".
 *
 * The method should only perform at most ceil(log2(n))+1 comparisons,
 * where n is the number of elements in data.
 */
def searchLow[A](data: Array[A], low: A)(using ord: Ordering[A]): Option[Int] =
  // Use of recursion is recommended,
  // but you can also implement an iterative version if you wish.
  def inner(from: Int, to: Int): Option[Int] =
    if from <= to then
      val mid = from + (to - from) / 2
      val cmp = ord.compare(low, data(mid))
      if cmp <= 0 then
        inner(from, mid - 1).orElse(Some(mid))
      else
        inner(mid + 1, to)
    else
      None
  end inner

  inner(0, data.length - 1)
end searchLow


