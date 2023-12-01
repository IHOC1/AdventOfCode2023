package aoc2023

class Day1SumOfCalibrationValues {

  def sumOfCalibrationValue(str: String): Int = str.toList.filter(_.isDigit).mkString("").toInt

}
