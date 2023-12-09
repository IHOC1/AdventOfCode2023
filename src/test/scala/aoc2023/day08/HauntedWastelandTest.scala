package aoc2023.day08

import org.scalatest.flatspec.AnyFlatSpec

class HauntedWastelandTest extends AnyFlatSpec {

  "Test map 1" should "reach ZZZ in 2 steps" in {
    assert(HauntedWasteland.parse("Day08HauntedWastelandTestData1.txt").numberOfStepsToZZZ("AAA", 0, 0) === 2)
  }

  "Test map 2" should "reach ZZZ in 6 steps" in {
    assert(HauntedWasteland.parse("Day08HauntedWastelandTestData2.txt").numberOfStepsToZZZ("AAA", 0, 0) === 6)
  }

  "Full map" should "reach ZZZ in 20093 steps" in {
    assert(HauntedWasteland.parse("Day08HauntedWastelandData.txt").numberOfStepsToZZZ("AAA", 0, 0) === 20093)
  }

  // Part 2

  "Test map" should "reach ZZZ in 6 steps" in {
    assert(HauntedWasteland.parse("Day08HauntedWastelandPart2TestData1.txt").numberOfStepsToAllEndingInZ() === 6)
  }

}
