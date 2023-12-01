package aco2023

import aoc2023.Day1SumOfCalibrationValues
import org.scalatest.flatspec.AnyFlatSpec

class Day1SumOfCalibrationValuesTest extends AnyFlatSpec {

  "A simple number" should "total to itself" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("1") === 1)
  }

  "A 2-digit number" should "return itself" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("13") === 13)
  }

  "Non-digit characters" should "be ignored" in {
    assert(new Day1SumOfCalibrationValues().sumOfCalibrationValue("1abc2") === 12)
  }

}
