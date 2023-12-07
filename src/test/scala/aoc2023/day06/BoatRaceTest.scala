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

  "A BoatRace" should "should give the number of ways the record can be broken" in {
    assert(BoatRace(duration = 7, record = 9).winningButtonPresses() === 4)
    assert(BoatRace(duration = 15, record = 40).winningButtonPresses() === 8)
    assert(BoatRace(duration = 30, record = 200).winningButtonPresses() === 9)
  }

  "Part 1" should "should give the correct answer" in {
    println(
    BoatRace(duration = 59, record = 543).winningButtonPresses() *
    BoatRace(duration = 68, record = 1020).winningButtonPresses() *
    BoatRace(duration = 82, record = 1664).winningButtonPresses() *
    BoatRace(duration = 74, record = 1022).winningButtonPresses())
  }

  "Part 2 example" should "should give the correct answer" in {
    println(
    BoatRace(duration = 71530, record = 940200).winningButtonPresses()
    )
  }

  "Part 2" should "should give the correct answer" in {
    println(
    BoatRace(duration = 59688274, record = 543102016641022L).winningButtonPresses()
    )
  }

}
