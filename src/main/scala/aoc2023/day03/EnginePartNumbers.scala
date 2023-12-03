package aoc2023.day03

class EnginePartNumbers {

}

object EnginePartNumbers {
  def parseLine(line: String): Row = ???

}

case class Row(numbers: Seq[PartNumber])

case class PartNumber(from: Int, to: Int, number: Int)