package aoc2023

class Day1SumOfCalibrationValues {

  def sumOfCalibrationValue(str: String): Int = str.toList.filter(_.isDigit) match {
    case first +: _ :+ last => (first.toString + last).toInt
    case _ => 0
  }

}
