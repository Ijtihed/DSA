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

package resourceAnalyzer

/**
 * A simple immutable class for task information.
 * A task starts at the beginning of the time unit "start" and
 * runs "duration" time units.
 * Thus it is running during the half-closed interval [start,start+duration).
 */
case class Task(val start: Long, val duration: Long):
  require(start >= 0, "The starting time of the task should be non-negative")
  require(duration > 0, "The duration of the task should be positive")
  def end: Long = start + duration
  override def toString: String = s"Task[$start,$end)"
end Task
