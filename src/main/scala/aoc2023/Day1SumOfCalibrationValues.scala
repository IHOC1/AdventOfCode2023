package aoc2023

class Day1SumOfCalibrationValues {

  def calibrationValue(text: String): Int = {
    val digits = text.toList.filter(_.isDigit)
    digits.head.asDigit * 10 + digits.last.asDigit
  }

}
