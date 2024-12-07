package aoc2024.day07

import aoc2024.day07.BridgeCalculations.concat
import utils.ParseFile.parseFile

class BridgeCalibrations {

  def sumOfValidCalibrationValues(file: String, operators: Seq[Char]): Long = {
    parseFile(file).
      map(line => parseLine(line)).
      filter(calibrationEquation => calibrationEquation.isValidWith(operators)).
      map(_.testValue).
      sum
  }

  private def parseLine(line: String): CalibrationEquation = {
    val split = line.split(":")

    val testValue = split(0).strip().toLong
    val numbers = split(1).strip().split(" ").map(_.toLong)

    CalibrationEquation(testValue, numbers)
  }

}

case class CalibrationEquation(testValue: Long, numbers: Seq[Long]) {

  def isValidWith(operators: Seq[Char]): Boolean =
    matchesTestValue(numbers.head, numbers.tail, operators)

  private def matchesTestValue(currentValue: Long, numbers: Seq[Long], operators: Seq[Char]): Boolean = {
    if (numbers.isEmpty)
      currentValue == testValue
    else if (currentValue > testValue)
      false
    else
      operators.exists(operator =>
        operator match {
          case '+' => matchesTestValue(currentValue + numbers.head, numbers.tail, operators)
          case '*' => matchesTestValue(currentValue * numbers.head, numbers.tail, operators)
          case '|' => matchesTestValue(concat(currentValue, numbers.head), numbers.tail, operators)
        }
      )
  }

}

object BridgeCalculations {
  def concat(num1: Long, num2: Long): Long =
    (num1.toString + num2.toString).toLong
}