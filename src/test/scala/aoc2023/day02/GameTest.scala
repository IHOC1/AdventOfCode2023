package aoc2023.day02

import org.scalatest.flatspec.AnyFlatSpec

class GameTest extends AnyFlatSpec {

  "A Game is possible" should "all coloured cubes be less than the limit" in {
    assert(new Game(2, 2, 2).isPossible(new Game(3, 3, 3)))
  }

}
