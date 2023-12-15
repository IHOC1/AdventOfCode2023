package aoc2023.day10

import aoc2023.day10.PipeMaze.furthestPoint
import org.scalatest.flatspec.AnyFlatSpec

class PipeMazeTest extends AnyFlatSpec {

  "Test pipe maze" should "give furthest point of 4" in {
    assert(furthestPoint("Day10TestPipeMaze.txt") === 4L)
  }

  "Test pipe maze 2" should "give furthest point of 4" in {
    assert(furthestPoint("Day10TestPipeMaze2.txt") === 4L)
  }
}
