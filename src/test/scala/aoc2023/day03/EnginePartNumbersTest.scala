package aoc2023.day03

import aoc2023.day03.EnginePartNumbers.{parseLine, sumGenuineEnginePartNumbers}
import org.scalatest.flatspec.AnyFlatSpec

class EnginePartNumbersTest() extends AnyFlatSpec {

  "A number on a line" should "be parsed and located" in {
    assert(parseLine("467") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 467))))
    assert(parseLine("512") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 512))))
  }

  "Non digit characters" should "be ignored when parsing part numbers" in {
    assert(parseLine(".467") === Row(numbers = Seq(PartNumber(from = 1, to = 3, number = 467))))
    assert(parseLine("512.") === Row(numbers = Seq(PartNumber(from = 0, to = 2, number = 512))))
  }

  "Multiple part numbers" should "be parsed from a line" in {
    assert(parseLine(".467...23243.") ===
      Row(numbers = Seq(
        PartNumber(from = 1, to = 3, number = 467),
        PartNumber(from = 7, to = 11, number = 23243)
      )))
  }


  "A symbol on a line" should "be parsed and located" in {
    assert(parseLine("#") === Row(symbols = Seq(Symbol(0))))
    assert(parseLine("..$..") === Row(symbols = Seq(Symbol(2))))
  }

  "Multiple symbols on a line" should "be parsed and located" in {
    assert(parseLine("..#...^...") === Row(symbols = Seq(Symbol(2), Symbol(6))))
  }

  "Engine part numbers immediately after symbols on the same line" should "be flagged as true engine part numbers " in {
    val row = parseLine("#2")
    assert(row.numbers(0).nextToSymbolOnSameLine(row.symbols) === true)
  }

  "Engine part numbers immediately before symbols on the same line" should "be flagged as true engine part numbers " in {
    val row = parseLine("2$")
    assert(row.numbers(0).nextToSymbolOnSameLine(row.symbols) === true)
  }

  "Engine part numbers not next to symbols on the same line" should "not be flagged as true engine part numbers " in {
    val row = parseLine("#.2")
    assert(row.numbers(0).nextToSymbolOnSameLine(row.symbols) === false)
  }

  "Engine part numbers immediately after symbols on an adjacent line" should "be flagged as true engine part numbers " in {
    val row = parseLine(".2")
    val adjacentRow = parseLine("#.")
    assert(row.numbers(0).nextToSymbolOnAdjacentLine(adjacentRow.symbols) === true)
  }

  "Engine part numbers immediately before symbols on an adjacent line" should "be flagged as true engine part numbers " in {
    val row = parseLine("2.")
    val adjacentRow = parseLine(".#")
    assert(row.numbers(0).nextToSymbolOnAdjacentLine(adjacentRow.symbols) === true)
  }

  "Engine part numbers within symbols on an adjacent line" should "be flagged as true engine part numbers " in {
    val row = parseLine(".34.")
    val adjacentRow = parseLine(".#..")
    assert(row.numbers(0).nextToSymbolOnAdjacentLine(adjacentRow.symbols) === true)
  }

  "Part 1 " should "Give the correct sum for example" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day03SmallSampleEnginePartNumbers.txt")
    val rows: Seq[Row] = source.getLines().
      map((line: String) => parseLine(line)).toList

    println(sumGenuineEnginePartNumbers(rows))

    source.close()
  }

  "Part 1 " should "Give the correct sum" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day03EnginePartNumbers.txt")
    val rows: Seq[Row] = source.getLines().
      map((line: String) => parseLine(line)).toList

    println(sumGenuineEnginePartNumbers(rows))

    source.close()
  }

}
