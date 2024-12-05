package aoc2024.day01

import org.scalatest.flatspec.AnyFlatSpec

class IdListsTest extends AnyFlatSpec {

  "SimpleExample" should "sum differences to 11" in {
    assert(new IdLists().totalDistanceBetweenLists("aoc2024/day01/SimpleExampleLocationIdLists.txt") == 11)
  }

  "Part one" should "sum differences to 2057374" in {
    assert(new IdLists().totalDistanceBetweenLists("aoc2024/day01/FullLocationIdLists.txt") == 2057374)
  }

  "SimpleExample" should "have similarity score of 31" in {
    assert(new IdLists().similarityScore("aoc2024/day01/SimpleExampleLocationIdLists.txt") == 31)
  }

  "Part two" should "have similarity score of 23177084" in {
    assert(new IdLists().similarityScore("aoc2024/day01/FullLocationIdLists.txt") == 23177084)
  }

}
