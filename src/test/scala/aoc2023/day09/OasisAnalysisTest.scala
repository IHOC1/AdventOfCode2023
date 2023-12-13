package aoc2023.day09

import aoc2023.day09.OasisAnalysis.{extrapolate, sumOfExtrapolatedSensorValues}
import org.scalatest.flatspec.AnyFlatSpec

class OasisAnalysisTest extends AnyFlatSpec {

  "Extrapolation" should "work for the simplest test input" in {
    assert(extrapolate(List(0, 3, 6, 9, 12, 15)) === 18L)
  }

  "Extrapolation" should "work for the next simplest test input" in {
    assert(extrapolate(List(1, 3, 6, 10, 15, 21)) === 28L)
  }

  "Extrapolation" should "work for the third simplest test input" in {
    assert(extrapolate(List(10, 13, 16, 21, 30, 45)) === 68L)
  }

  "Sum of extrapolated values for the test data" should "should be correct" in {
    assert(sumOfExtrapolatedSensorValues("Day09TestOasisSensorValues.txt") === 114L)
  }
}
