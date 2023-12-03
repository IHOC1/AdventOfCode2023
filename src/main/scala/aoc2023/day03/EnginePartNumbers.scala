package aoc2023.day03

import scala.util.matching.Regex

class EnginePartNumbers {

}

object EnginePartNumbers {

  def sumGenuineEnginePartNumbers(rows: Seq[Row]) = {
    val sum = Range(0, rows.length).
      flatMap((rowNum: Int) => {
        val previousRow = rows.lift(rowNum - 1)
        val currentRow  = rows(rowNum)
        val nextRow     = rows.lift(rowNum + 1)
        currentRow.numbers.
          filter(
            partNumber => partNumber.nextToSymbolOnSameLine(currentRow.symbols) ||
              partNumber.nextToSymbolOnAdjacentLine(previousRow.map(_.symbols).getOrElse(Seq())) ||
              partNumber.nextToSymbolOnAdjacentLine(    nextRow.map(_.symbols).getOrElse(Seq())))
      }).map(_.number).sum
    sum
  }

  def sumGearRatios(rows: Seq[Row]) = {
    Range(0, rows.length).
      map((rowNum: Int) => {
        val previousRow = rows.lift(rowNum - 1)
        val currentRow = rows(rowNum)
        val nextRow = rows.lift(rowNum + 1)

        currentRow.symbols.
          filter(_.symbol == "*").
          map(symbol =>
            symbol.partNumbersOnSameLine(currentRow.numbers) ++
              symbol.partNumbersOnAdjacentLine(previousRow) ++
              symbol.partNumbersOnAdjacentLine(nextRow)
          ).
          filter(numbers => numbers.length == 2).
          map(numbers => numbers(0).number * numbers(1).number).
          sum
      }).sum
  }

  def parseLine(line: String): Row = {
    Row(partNumbers(line), symbols(line))
  }

  private val numberPattern: Regex = "([0-9]+)".r

  private def partNumbers(line: String) = {
    numberPattern.findAllIn(line).matchData.
      map((m: Regex.Match) => PartNumber(m.start, m.end - 1, m.group(0).toInt)).toSeq
  }

  private val symbolPattern: Regex = "([^0-9.])".r

  def symbols(line: String): Seq[Symbol] =
    symbolPattern.findAllIn(line).matchData.
      map((m: Regex.Match) => Symbol(m.group(0), m.start)).toSeq
}

case class Row(numbers: Seq[PartNumber] = Seq(),
               symbols: Seq[Symbol    ] = Seq())

case class PartNumber(from: Int, to: Int, number: Int) {

  def nextToSymbolOnSameLine(symbols: Seq[Symbol]): Boolean =
    symbols.exists(symbol => isImmediatelyBefore(symbol) || isImmediatelyAfter(symbol))

  def nextToSymbolOnAdjacentLine(symbols: Seq[Symbol]): Boolean =
    symbols.exists(symbol => isImmediatelyBefore(symbol) || isImmediatelyAfter(symbol) || isWithin(symbol))

  def isImmediatelyBefore(symbol: Symbol) = symbol.position == (from - 1)
  def isImmediatelyAfter(symbol: Symbol)  = symbol.position == (to   + 1)
  def isWithin(symbol: Symbol)            = from <= symbol.position && symbol.position <= to

}

case class Symbol(symbol :String, position: Int) {

  def partNumbersOnSameLine(numbers: Seq[PartNumber]): Seq[PartNumber] =
    numbers.filter(num => num.isImmediatelyBefore(this) || num.isImmediatelyAfter(this))

  def partNumbersOnAdjacentLine(adjacentRow: Option[Row]): Seq[PartNumber] =
    adjacentRow.map(row => row.numbers.filter(num => num.isImmediatelyBefore(this) || num.isImmediatelyAfter(this) || num.isWithin(this))).getOrElse(Seq())
}
