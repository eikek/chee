package chee

import java.time.Duration
import java.time.Instant

object Timing {

  def timed[A](f: (A, Duration) => Unit)(body: => A): A = {
    val (r, d) = timedResult(body)
    f(r, d)
    r
  }

  def timedResult[A](body: => A): (A, Duration) = {
    val start = System.nanoTime
    val r = body
    (r, Duration.ofNanos(System.nanoTime - start))
  }

  def format(millis: Long): String = {
    val s = millis / 1000
    "%d:%02d:%02d.%d".format(s / 3600, (s % 3600) / 60, (s % 60), millis - (s * 1000))
  }

  def format(d: Duration): String =
    format(d.toMillis)

  def printDuration(d: Duration): Unit =
    println(s"Time: ${format(d)}")
}
