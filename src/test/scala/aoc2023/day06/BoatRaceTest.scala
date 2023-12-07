package aoc2023.day06

import org.scalatest.flatspec.AnyFlatSpec

class BoatRaceTest extends AnyFlatSpec {

  "A Boat" should "travels at the speed for which the button is held down" in {
    assert(Boat(7).distance(0) === 0)
    assert(Boat(7).distance(1) === 6)
    assert(Boat(7).distance(2) === 10)
    assert(Boat(7).distance(3) === 12)
    assert(Boat(7).distance(4) === 12)
    assert(Boat(7).distance(5) === 10)
    assert(Boat(7).distance(6) === 6)
    assert(Boat(7).distance(7) === 0)
  }

}
