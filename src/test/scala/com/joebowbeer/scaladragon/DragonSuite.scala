package com.joebowbeer.scaladragon

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import scala.io.Source
import scala.util.Random

class DragonSuite extends JUnitSuite {

  @Test def solvesEmpty() {
    val canyon = Array[Int]()
    val solution = Array[Int]()
    checkSolution(canyon, solution)
    assertResult(solution) { Dragon.solve(canyon) }
  }

  @Test def solvesOne() {
    val canyon = Array(1)
    val solution = Array(0)
    checkSolution(canyon, solution)
    assertResult(solution) { Dragon.solve(canyon) }
  }

  @Test def solvesSample() {
    val canyon = Array(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)
    val solution = Array(0, 5, 9)
    checkSolution(canyon, solution)
    assertResult(solution) { Dragon.solve(canyon) }
  }

  @Test def failsDragon() {
    assertResult(Array[Int]()) { Dragon.solve(Array(0)) }
  }

  @Test def parsesEmpty() {
    assertResult(Array[Int]()) { Dragon.parse(source()) }
  }

  @Test def parsesOne() {
    assertResult(Array(1)) { Dragon.parse(source(1)) }
  }

  @Test def parsesSampleInput() {
    assertResult(Array(5, 6, 0, 4, 2, 4, 1, 0, 0, 4)) {
      Dragon.parse(source(5, 6, 0, 4, 2, 4, 1, 0, 0, 4))
    }
  }

  @Test def formatsEmpty() {
    assertResult("failure") { Dragon.format(Array()) }
  }

  @Test def formatsZero() {
    assertResult("0, out") { Dragon.format(Array(0)) }
  }

  @Test def formatsSampleSolution() {
    assertResult("0, 5, 9, out") { Dragon.format(Array(0, 5, 9)) }
  }

  @Test def failsEmptyInput() {
    assertResult("failure") { solutionOf() }
  }

  @Test def solvesOneInput() {
    assertResult("0, out") { solutionOf(1) }
  }

  @Test def solvesSampleInput() {
    assertResult("0, 5, 9, out") { solutionOf(5, 6, 0, 4, 2, 4, 1, 0, 0, 4) }
  }

  @Test def failsNegativeInput() {
    assertResult("failure") { solutionOf(-1) }
  }

  @Test def solvesRandomCanyons() {
    val canyonLength = 1000000
    val longestFlight = 50
    val dragonCount = 10000
    for (trial <- 0 until 10) {
      val canyon = randomCanyon(canyonLength, longestFlight, dragonCount)
      val traversal = Dragon.solve(canyon)
      printf("Trial %2d: traversal length %d%n", trial, traversal.length)
      checkSolution(canyon, traversal)
    }
  }

  def source(values: Int*): Source = {
    Source.fromString(if (values nonEmpty) values.mkString("", "\n", "\n") else "")
  }

  def solutionOf(values: Int*): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    try {
      Dragon.solve(source(values: _*), ps)
    } finally {
      ps.close()
    }
    Source.fromString(baos.toString).getLines.next
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
  def checkSolution(canyon: Array[Int], traversal: Array[Int]) {
    if (traversal nonEmpty) {
      assertResult(0) { traversal(0) }
      val lastIndex = traversal.reduceLeft { (prevIndex, index) =>
        assert(prevIndex + canyon(prevIndex) >= index)
        index
      }
      assert(lastIndex + canyon(lastIndex) >= canyon.length)
    }
  }
}
