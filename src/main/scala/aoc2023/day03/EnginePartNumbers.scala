package aoc2023.day03

import scala.util.matching.Regex

class EnginePartNumbers {

}

object EnginePartNumbers {

  val numberPattern = "[0-9]+".r

  def parseLine(line: String): Row = {
    Row(partNumbers(line))
  }

  private def partNumbers(line: String) = {
    numberPattern.findAllIn(line).matchData.
      map((m: Regex.Match) => PartNumber(m.start, m.end - 1, m.group(0).toInt)).toSeq
  }
}

case class Row(numbers: Seq[PartNumber] = Seq(),
               symbols: Seq[Symbol    ] = Seq())

case class PartNumber(from: Int, to: Int, number: Int)
case class Symbol(position: Int)
