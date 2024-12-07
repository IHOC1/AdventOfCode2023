package aoc2024.day06

import org.scalatest.flatspec.AnyFlatSpec

class GuardPatrolPathTest extends AnyFlatSpec {

  "Example Guard Patrol" should "visit 41 distinct positions" in {
    assert(new GuardPatrolPath().numDistinctPositionsVisited("aoc2024/day06/ExampleGuardPatrol.txt") == 41)
  }

  "Full Guard Patrol" should "visit 5409 distinct positions" in {
    assert(new GuardPatrolPath().numDistinctPositionsVisited("aoc2024/day06/FullGuardPatrol.txt") == 5409)
  }

  "Example Guard Patrol" should "take 6 different obstructions to cause a loop" in {
    assert(new GuardPatrolPath().numPositionsToCauseALoop("aoc2024/day06/ExampleGuardPatrol.txt") == 6)
  }

  "Full Guard Patrol" should "take 2022 different obstructions to cause a loop" in {
    assert(new GuardPatrolPath().numPositionsToCauseALoop("aoc2024/day06/FullGuardPatrol.txt") == 2022)
  }

}
