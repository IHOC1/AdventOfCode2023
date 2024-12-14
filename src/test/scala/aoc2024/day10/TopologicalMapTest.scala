package aoc2024.day10

import org.scalatest.flatspec.AnyFlatSpec

class TopologicalMapTest extends AnyFlatSpec {

  "Simplest Topological Map" should "sum trailhead scores to 2" in {
    assert(new TopologicalMapPart1().sumTrailheadScores("aoc2024/day10/SimplestTopologicalMap.txt") == 2)
  }

  "Simplest Topological Map 2" should "sum trailhead scores to 4" in {
    assert(new TopologicalMapPart1().sumTrailheadScores("aoc2024/day10/SimplestTopologicalMap2.txt") == 4)
  }

  "Simple Topological Map" should "sum trailhead scores to 36" in {
    assert(new TopologicalMapPart1().sumTrailheadScores("aoc2024/day10/SimpleTopologicalMap.txt") == 36)
  }

  "Full Topological Map" should "sum trailhead scores to 607" in {
    assert(new TopologicalMapPart1().sumTrailheadScores("aoc2024/day10/FullTopologicalMap.txt") == 607)
  }

  "Simple Topological Map" should "sum trailhead scores to 81" in {
    assert(new TopologicalMapPart2().sumTrailheadScores("aoc2024/day10/SimpleTopologicalMap.txt") == 81)
  }

  "Full Topological Map" should "sum trailhead scores to 1384" in {
    assert(new TopologicalMapPart2().sumTrailheadScores("aoc2024/day10/FullTopologicalMap.txt") == 1384)
  }

}
