package aoc2023.day10

import aoc2023.day10.PipeMaze.{furthestPoint, numInternalTiles}
import org.scalatest.flatspec.AnyFlatSpec

class PipeMazeTest extends AnyFlatSpec {

  "Test pipe maze" should "give furthest point of 4" in {
    assert(furthestPoint("Day10TestPipeMaze.txt") === 4L)
  }

  "Test pipe maze 2" should "give furthest point of 4" in {
    assert(furthestPoint("Day10TestPipeMaze2.txt") === 4L)
  }

  "Test pipe maze 3" should "give furthest point of 8" in {
    assert(furthestPoint("Day10TestPipeMaze3.txt") === 8L)
  }

  "Pipe maze" should "give furthest point of 7173" in {
    assert(furthestPoint("Day10PipeMaze.txt") === 7173L)
  }

  // Part 2

  "Pipe maze 1" should "give num internal tiles as 4" in {
    assert(numInternalTiles("Day10TestPipeMaze1Part2.txt") === 4L)
  }

  "Pipe maze 2" should "give num internal tiles as 8" in {
    assert(numInternalTiles("Day10TestPipeMaze2Part2.txt") === 8L)
  }

  "Pipe maze" should "give num internal tiles as ???" in {
    assert(numInternalTiles("Day10PipeMaze.txt") === 291L)
  }
}
