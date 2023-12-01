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

}
