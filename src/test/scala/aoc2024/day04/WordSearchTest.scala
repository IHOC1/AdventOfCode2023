package aoc2024.day04

import org.scalatest.flatspec.AnyFlatSpec

class WordSearchTest extends AnyFlatSpec {

  "Example word search" should "have 18 versions of XMAS" in {
    assert(new WordSearch("aoc2024/day04/ExampleWordSearch.txt").count("XMAS") == 18)
  }

  "Full word search" should "have 2646 versions of XMAS" in {
    assert(new WordSearch("aoc2024/day04/FullWordSearch.txt").count("XMAS") == 2646)
  }

  "Example word search 2" should "have 9 versions of MAS in X formation" in {
    assert(new WordSearch("aoc2024/day04/ExampleWordSearch.txt").countInXFormation("MAS") == 9)
  }

  "Full word search" should "have 2000 versions of XMAS" in {
    assert(new WordSearch("aoc2024/day04/FullWordSearch.txt").countInXFormation("MAS") == 2000)
  }

}
