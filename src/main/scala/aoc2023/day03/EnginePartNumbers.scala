package aoc2023.day03

import scala.util.matching.Regex

class EnginePartNumbers {

}

object EnginePartNumbers {

  def parseLine(line: String): Row = {
    Row(partNumbers(line), symbols(line))
  }

  private val numberPattern: Regex = "[0-9]+".r

  private def partNumbers(line: String) = {
    numberPattern.findAllIn(line).matchData.
      map((m: Regex.Match) => PartNumber(m.start, m.end - 1, m.group(0).toInt)).toSeq
  }

  private val symbolPattern: Regex = "[^0-9.]".r

  def symbols(line: String): Seq[Symbol] =
    symbolPattern.findAllIn(line).matchData.
      map((m: Regex.Match) => Symbol(m.start)).toSeq
}

case class Row(numbers: Seq[PartNumber] = Seq(),
               symbols: Seq[Symbol    ] = Seq())

case class PartNumber(from: Int, to: Int, number: Int) {

  def nextToSymbolOnSameLine(symbols: Seq[Symbol]): Boolean =
    symbols.
      exists(p => isImmediatelyBeforeOrAfter(p))

  private def isImmediatelyBeforeOrAfter(p: Symbol) = {
    p.position == from - 1 || p.position == to + 1
  }

  def nextToSymbolOnAdjacentLine(symbols: Seq[Symbol]): Boolean = nextToSymbolOnSameLine(symbols)

}

case class Symbol(position: Int)
