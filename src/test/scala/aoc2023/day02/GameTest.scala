package aoc2023.day02

import org.scalatest.flatspec.AnyFlatSpec

class GameTest extends AnyFlatSpec {

  "A Game is possible" should "all coloured cubes be less than the limit" in {
    assert(new Game(2, 2, 2).isPossible(new Game(3, 3, 3)))
  }

  "A Game is not possible" should "the red cubes be greater than the limit" in {
    assert(new Game(2, 2, 2).isPossible(new Game(1, 3, 3)) === false)
  }

  "A Game is not possible" should "the green cubes be greater than the limit" in {
    assert(new Game(2, 4, 2).isPossible(new Game(3, 3, 3)) === false)
  }

  "A Game is not possible" should "the blue cubes be greater than the limit" in {
    assert(new Game(2, 2, 5).isPossible(new Game(3, 3, 4)) === false)
  }

}
