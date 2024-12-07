package aoc2024.day07

import org.scalatest.flatspec.AnyFlatSpec

class BridgeCalibrationsTest extends AnyFlatSpec {

  "Example bridge calibrations" should "sum of valid calibration results of 3749" in {
    assert(new BridgeCalibrations().sumOfValidCalibrationValues("aoc2024/day07/ExampleBridgeCalibrations.txt", Seq('+', '*')) == 3749L)
  }

  "Full bridge calibrations" should "sum of valid calibration results of 1289579105366" in {
    assert(new BridgeCalibrations().sumOfValidCalibrationValues("aoc2024/day07/FullBridgeCalibrations.txt", Seq('+', '*')) == 1289579105366L)
  }

  "Example bridge calibrations" should "sum of valid calibration results with 3 operators of 11387" in {
    assert(new BridgeCalibrations().sumOfValidCalibrationValues("aoc2024/day07/ExampleBridgeCalibrations.txt", Seq('+', '*', '|')) == 11387L)
  }

  "Full bridge calibrations" should "sum of valid calibration results with 3 operators of 92148721834692" in {
    assert(new BridgeCalibrations().sumOfValidCalibrationValues("aoc2024/day07/FullBridgeCalibrations.txt", Seq('+', '*', '|')) == 92148721834692L)
  }

  "Combine" should "combine numbers " in {
    assert(BridgeCalculations.concat(5, 6) == 56L)
    assert(BridgeCalculations.concat(50, 60) == 5060L)
  }
}
