package aoc2023.day09

import aoc2023.day09.OasisAnalysis.{extrapolate, extrapolateBackwards, sumOfExtrapolatedBackwardsSensorValues, sumOfExtrapolatedSensorValues}
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

  "Sum of extrapolated values for the real data" should "should be correct" in {
    assert(sumOfExtrapolatedSensorValues("Day09OasisSensorValues.txt") === 1853145119L)
  }

  "Extrapolation backwards" should "work for the third simplest test input" in {
    assert(extrapolateBackwards(List(10, 13, 16, 21, 30, 45)) === 5L)
  }

  "Sum of backwards-extrapolated values for the test data" should "should be correct" in {
    assert(sumOfExtrapolatedBackwardsSensorValues("Day09TestOasisSensorValues.txt") === 2L)
  }

  "Sum of backwards-extrapolated values for the real data" should "should be correct" in {
    assert(sumOfExtrapolatedBackwardsSensorValues("Day09OasisSensorValues.txt") === 923L)
  }

}
