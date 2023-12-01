package aoc2023

class Day1SumOfCalibrationValues {

  def sumOfCalibrationValue(str: String): Int = {
    val digits = str.toList.filter(_.isDigit)
    digits.head.asDigit * 10 + digits.last.asDigit
  }

}
