package com.joebowbeer.scaladragon

import java.io.{ ByteArrayOutputStream, PrintStream }
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.scalatest.Matchers
import scala.io.Source
import scala.util.Random

class DragonSuite extends JUnitSuite with Matchers {

  @Test def solvesOne() {
    val canyon = Array(1)
    val solution = Array(0)
    checkSolution(canyon, solution)
    Dragon solve (canyon) should contain(solution)
  }

  @Test def solvesSample() {
    val canyon = Array(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)
    val solution = Array(0, 5, 9)
    checkSolution(canyon, solution)
    Dragon solve (canyon) should contain(solution)
  }

  @Test def parsesEmpty() {
    Dragon parse (source()) should be(Array())
  }

  @Test def parsesOne() {
    Dragon parse (source(1)) should be(Array(1))
  }

  @Test def parsesSampleInput() {
    Dragon parse (source(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)) should be(
      Array(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)
    )
  }

  @Test def formatsZero() {
    Dragon format (Array(0)) should be("0, out")
  }

  @Test def formatsSampleSolution() {
    Dragon format (Array(0, 5, 9)) should be("0, 5, 9, out")
  }

  @Test def failsEmptyInput() {
    solutionOf() should be("failure")
  }

  @Test def solvesOneInput() {
    solutionOf(1) should be("0, out")
  }

  @Test def solvesSampleInput() {
    solutionOf(5, 6, 0, 4, 2, 4, 1, 0, 0, 4) should be("0, 5, 9, out")
  }

  @Test def failsInitialDragon() {
    solutionOf(0) should be("failure")
  }

  @Test def failsNegativeInput() {
    solutionOf(-1) should be("failure")
  }

  @Test def solvesRandomCanyons() {
    val canyonLength = 1000000
    val longestFlight = 50
    val dragonCount = 10000
    for (trial <- 0 until 10) {
      val canyon = randomCanyon(canyonLength, longestFlight, dragonCount)
      Dragon solve (canyon) match {
        case Some(traversal) => {
          printf("Trial %2d: traversal length %d%n", trial, traversal length)
          checkSolution(canyon, traversal)
        }
        case None => printf("Trial %2d: No traversal!%n", trial)
      }
    }
  }

  def source(values: Int*): Source = {
    Source fromString (if (values nonEmpty) values mkString ("", "\n", "\n") else "")
  }

  def solutionOf(values: Int*): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    try {
      Dragon solve (source(values: _*), ps)
    } finally {
      ps close ()
    }
    Source.fromString(baos toString).getLines.next
  }

  def randomCanyon(canyonLength: Int, longestFlight: Int, dragonCount: Int): Array[Int] = {
    require(dragonCount < canyonLength)
    val canyon = new Array[Int](canyonLength)
    val random = new Random()
    // The canyon starts with the clan of dragons on the right, with remaining
    // elements initialized to random lengths.
    for (index <- canyonLength - dragonCount - 1 to 0 by -1) {
      canyon(index) = 1 + random.nextInt(longestFlight)
    }
    // Shuffle the array; except first element, for no dragon should be there.
    for (index <- 1 until canyonLength) {
      val swapIndex = index + random.nextInt(canyonLength - index)
      if (swapIndex != index) {
        val swapValue = canyon(swapIndex)
        canyon(swapIndex) = canyon(index)
        canyon(index) = swapValue
      }
    }
    canyon
  }

  /** Validates traversal. */
  def checkSolution(canyon: Array[Int], traversal: Array[Int]) = {
    // Verify that 0 is visited first.
    assert(traversal(0) == 0)
    // Verify each cell is reachable from its predecessor and that final flight leaves the canyon.
    traversal.foldRight(canyon length) { (index, nextIndex) =>
      assert(index + canyon(index) >= nextIndex)
      index
    }
  }
}
