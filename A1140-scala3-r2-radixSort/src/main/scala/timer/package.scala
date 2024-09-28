/**
 * A small package for measuring time usage of functions.
 * @author Tommi Junttila
 */
package timer

val minTime = 1e-9 // A safeguad to prevent 0.0 running times.

/**
 * Run the argument function f and
 * measure the wall clock time spent in it in seconds.
 * As wall clock time is used, the other processes running in the same machine
 * can effect the returned run-time quite a lot.
 * But due to Windows system clock resolution limits,
 * using wallclock time seems to be better than CPU time on small running times.
 */
def measureWallClockTime[T](f: => T): (T, Double) =
  val start: Long = System.nanoTime
  val r = f
  val end: Long = System.nanoTime
  val t = minTime max (end - start) / 1e9
  (r, t)
