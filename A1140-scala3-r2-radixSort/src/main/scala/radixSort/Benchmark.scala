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

import collection.mutable.ArrayBuffer
import timer.measureWallClockTime

object Benchmark extends plotters.Plotters:
  val title = "Sorting arrays of random integers"
  val filenameBase = "radixsort_benchmark"
  val tools: List[(String, Array[Int] => Unit)] = List(
    ("java.util.Arrays.sort", java.util.Arrays.sort(_)),
    ("radixSort.lsdRadixSort", radixSort.lsdRadixSort(_))
  )

  val nofElements = 10000000
  val sizes = Range.inclusive(50000, 1000000, 50000)
  val rand = util.Random()
  def createInputs(n: Int): Vector[Array[Int]] =
    Vector.fill[Array[Int]](nofElements / n max 1)(Array.fill[Int](n)(rand.nextInt))

  def run(): Unit =
    val allResults = ArrayBuffer[Results]()
    for (name, sort) <- tools do
      println(s"$name: warming up...")
      for input <- createInputs(sizes.last) do sort(input)
      println(s"$name: measuring...")
      val points = ArrayBuffer[(Int, Double)]()
      for n <- sizes do
        val inputs = createInputs(n)
        val (_, cumuTime) = measureWallClockTime {
          for input <- inputs do sort(input)
        }
        val time = cumuTime / inputs.length
        println(f"$n: $time%.9f")
        points += ((n, time))
      allResults += Results(name, points)
    writeHTML(title, filenameBase + ".html", allResults.toSeq)

@main
def runBenchmark(): Unit = Benchmark.run()
