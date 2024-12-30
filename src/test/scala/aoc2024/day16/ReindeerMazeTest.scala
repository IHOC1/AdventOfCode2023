package aoc2024.day16

import org.scalatest.flatspec.AnyFlatSpec

class ReindeerMazeTest extends AnyFlatSpec {

  "First test maze" should "be solvable in a minimum of 7036 points" in {
    assert(new ReindeerMazeDijkstra().solve("aoc2024/day16/FirstTestMaze.txt") == 7036)
  }

  "Second test maze" should "be solvable in a minimum of 11048 points" in {
    assert(new ReindeerMazeDijkstra().solve("aoc2024/day16/SecondTestMaze.txt") == 11048)
  }

  "Full maze" should "be solvable in a minimum of 95476 points" in {
    assert(new ReindeerMazeDijkstra().solve("aoc2024/day16/FullMaze.txt") == 95476)
  }

  // Part 2

  "First test maze" should "have number of tiles on any best path of 45 points" in {
    assert(new ReindeerMazeDijkstraFullPath().solve("aoc2024/day16/FirstTestMaze.txt") == 45)
  }

  "Second test maze" should "have number of tiles on any best path of 64 points" in {
    assert(new ReindeerMazeDijkstraFullPath().solve("aoc2024/day16/SecondTestMaze.txt") == 64)
  }

  "Full maze" should "have number of tiles on any best path of 511 points" in {
    assert(new ReindeerMazeDijkstraFullPath().solve("aoc2024/day16/FullMaze.txt") == 511)
  }

}
