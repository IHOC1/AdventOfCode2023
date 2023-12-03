package aoc2023.day03

class EnginePartNumbers {

}

object EnginePartNumbers {

  def parseLine(line: String): Row = Row(Seq(PartNumber(0, 2, line.toInt)))

}

case class Row(numbers: Seq[PartNumber])

case class PartNumber(from: Int, to: Int, number: Int)