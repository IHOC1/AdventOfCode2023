package aoc2023.day03

import org.scalatest.flatspec.AnyFlatSpec

class EnginePartNumbersTest() extends AnyFlatSpec {

  "A number on a line" should "be parsed and located" in {
    assert(EnginePartNumbers.parseLine("467") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 467))))
    assert(EnginePartNumbers.parseLine("512") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 512))))
  }

}
