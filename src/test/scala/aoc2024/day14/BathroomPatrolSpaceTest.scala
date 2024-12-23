package aoc2024.day14

import org.scalatest.flatspec.AnyFlatSpec

class BathroomPatrolSpaceTest extends AnyFlatSpec {

  "Test bathroom security robots" should "have safety factor of 12" in {
    assert(new BathroomPatrolSpace(width = 11, height = 7).safetyFactor("aoc2024/day14/TestBathroomSecurityRobots.txt") == 12)
  }

  "Full bathroom security robots" should "have safety factor of 224357412" in {
    assert(new BathroomPatrolSpace(width = 101, height = 103).safetyFactor("aoc2024/day14/FullBathroomSecurityRobots.txt") == 224357412)
  }

  "Full bathroom security robots" should "first make a christmas tree after 7083 moves" in {
    assert(new BathroomPatrolSpace(width = 101, height = 103).numMovesUntilTreeFound("aoc2024/day14/FullBathroomSecurityRobots.txt") == 7083)
  }

}
