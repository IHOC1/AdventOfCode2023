package aoc2023.day08

import org.scalatest.flatspec.AnyFlatSpec

class HauntedWastelandTest extends AnyFlatSpec {

  "Test map 1" should "reach ZZZ in 2 steps" in {
    assert(HauntedWasteland.parse("Day08HauntedWastelandTestData1.txt").numberOfStepsToZZZ("AAA", 0) === 2)
  }
}
