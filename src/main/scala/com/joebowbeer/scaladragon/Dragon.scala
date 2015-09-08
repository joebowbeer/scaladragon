package com.joebowbeer.scaladragon

import java.io.PrintStream
import java.lang.System
import scala.collection.mutable
import scala.io.Source

/**
 * Solves the Dragon challenge.
 */
object Dragon {

  val Out = "out"
  val Failure = "failure"

  /**
   * Reads input from System.in and writes solution to System.out.
   *
   * @param args the unused command line arguments
   */
  def main(args: Array[String]) {
    solve(Source.fromInputStream(System.in), System.out)
  }

  /**
   * Reads input from given input stream and writes solution to given print stream.
   *
   * @param args the unused command line arguments
   */
  def solve(in: Source, out: PrintStream) {
    out.println(try { format(solve(parse(in))) } catch { case _: Exception => Failure })
  }

  /**
   * Returns array of numbers read from the given input stream.
   */
  def parse(in: Source): Array[Int] = in.getLines map (_.toInt) toArray

  /**
   * Returns string representation for the given solution.
   */
  def format(indices: Array[Int]): String = indices match {
    case Array() => Failure
    case _ => indices.mkString("", ", ", ", " + Out)
  }

  /**
   * Searches for a shortest solution.
   *
   * @param canyon array of non-negative numbers representing dragons and maximum flight lengths
   * @return shortest solution found, or an empty array if no solution found
   */
  def solve(canyon: Array[Int]): Array[Int] = {
    require(canyon forall (_ >= 0))
    // Queue of active traversals. A traversal is a list of visited indices in reverse order.
    val queue = mutable.Queue[List[Int]]()
    val visited = new Array[Boolean](canyon.length)
    if (canyon.length > 0) {
      // visit first element
      queue += List(0)
      visited(0) = true
    }
    // Breadth-first search to find a traveral with the smallest number of flights.
    for (flights <- Iterator.from(1).takeWhile(_ => queue nonEmpty)) {
      for (traversals <- queue.size - 1 to 0 by -1) {
        val traversal = queue.dequeue
        val lastIndex = traversal.head
        val longestFlight = canyon(lastIndex)
        // If longestFlight is 0, a dragon be there and the loop below does nothing.
        for (flight <- longestFlight to 1 by -1) {
          val nextIndex = lastIndex + flight
          if (nextIndex >= canyon.length) {
            // Canyon traversed! Return array of canyon indices.
            val result = new Array[Int](flights)
            for ((canyonIndex, traversalIndex) <- traversal.zipWithIndex) {
              result(flights - traversalIndex - 1) = canyonIndex
            }
            return result
          }
          // If we jumped to new element, enqueue new traversal for future exploration.
          if (!visited(nextIndex)) {
            queue += nextIndex :: traversal
            visited(nextIndex) = true
          }
        }
      }
    }
    // no solution
    Array()
  }
}
