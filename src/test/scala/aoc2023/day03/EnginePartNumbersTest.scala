package aoc2023.day03

import org.scalatest.flatspec.AnyFlatSpec

class EnginePartNumbersTest() extends AnyFlatSpec {

  "A number on a line" should "be parsed and located" in {
    assert(EnginePartNumbers.parseLine("467") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 467))))
    assert(EnginePartNumbers.parseLine("512") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 512))))
  }

  "Non digit characters" should "be ignored when parsing part numbers" in {
    assert(EnginePartNumbers.parseLine(".467") === Row(numbers = Seq(PartNumber(from = 1, to = 3, number = 467))))
    assert(EnginePartNumbers.parseLine("512.") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 512))))
  }

  "Multiple part numbers" should "be parsed from a line" in {
    assert(EnginePartNumbers.parseLine(".467...23243.") ===
      Row(numbers = Seq(
        PartNumber(from = 1, to = 3, number = 467),
        PartNumber(from = 7, to = 11, number = 23243)
      )))
  }


  "A symbol on a line" should "be parsed and located" in {
    assert(EnginePartNumbers.parseLine("#") === Row(symbols = Seq(Symbol(0))))
  }

}
