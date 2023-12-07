package aoc2023.day06

import org.scalatest.flatspec.AnyFlatSpec

class BoatRaceTest extends AnyFlatSpec {

  "A Boat" should "travels at the speed for which the button is held down" in {
    assert(Boat(7).distance(0) === 0)
  }

}
