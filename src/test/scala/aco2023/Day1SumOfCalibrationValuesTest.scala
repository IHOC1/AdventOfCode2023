package aco2023

import aoc2023.Day1SumOfCalibrationValues
import org.scalatest.flatspec.AnyFlatSpec

class Day1SumOfCalibrationValuesTest extends AnyFlatSpec {

  "A 2-digit number" should "return itself" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("13") === 13)
  }

  "Non-digit characters" should "be ignored" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("1abc2") === 12)
  }

  "If more than 2 digits" should "use only the first and last" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("123") === 13)
  }

  "Single digit values" should "use that digit for both first and last" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("4") === 44)
  }

  "Examples given" should "sum as expected" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("pqr3stu8vwx") === 38)
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("a1b2c3d4e5f") === 15)
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("treb7uchet" ) === 77)
  }

}
