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
 * The abstract base class for different resource analyzers.
 * Do not modify.
 */
abstract class ResourceAnalyzer:
  /*
   * Introduce a new Task "task" running in the half-closed time interval
   * [task.start,task.end).
   * The tasks shall be introduced in non-decreasing order
   * with respect to their start times.
   */
  def newTask(task: Task): Unit

  /*
   * Return the minimum amount of resources required by
   * the input sequence so far.
   * This should be a really fast constant time operation.
   * */
  def minResources: Int
end ResourceAnalyzer



/*
 * A slow reference implementation using an ArrayBuffer and filtering.
 */
class ResourceAnalyzerSlow extends ResourceAnalyzer:
  // For input validation only
  protected var prevStart = -1L
  // Stores the end times of the tasks that are currently "live"
  protected var liveTasksEndTime = collection.mutable.ArrayBuffer[Long]()
  // The maximum load seen so far
  protected var minResources_ = 0

  def newTask(task: Task): Unit =
    // Input validation: the tasks are given in the order of start time
    require(prevStart <= task.start)
    prevStart = task.start
    // Remove tasks that end before the new start time
    liveTasksEndTime = liveTasksEndTime.filter(_ > task.start)
    // Add the new task end time to the buffer
    liveTasksEndTime.append(task.end)
    // Update the maximum load variable
    minResources_ = minResources_ max liveTasksEndTime.length
  end newTask

  def minResources: Int = minResources_
end ResourceAnalyzerSlow


/*
 * A faster implementation using collection.mutable.PriorityQueue.
 */
class ResourceAnalyzerFast extends ResourceAnalyzer:
  // For input validation only
  protected var prevStart = -1L
  
  protected val liveTasksEndTime = collection.mutable.PriorityQueue[Long]()(Ordering.Long.reverse)
  protected var minResources_ = 0

  def newTask(task: Task) =
    require(prevStart <= task.start)
    prevStart = task.start
    
    while liveTasksEndTime.nonEmpty && liveTasksEndTime.head <= task.start do
      liveTasksEndTime.dequeue()
    liveTasksEndTime.enqueue(task.end)
    
    minResources_ = minResources_ max liveTasksEndTime.size
  end newTask

  def minResources: Int = minResources_
end ResourceAnalyzerFast
