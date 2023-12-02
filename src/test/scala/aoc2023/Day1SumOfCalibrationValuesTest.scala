package aoc2023

import org.scalatest.flatspec.AnyFlatSpec

class Day1SumOfCalibrationValuesTest extends AnyFlatSpec {

  val sumOfCalibrationValues = new Day1SumOfCalibrationValues()

  "A 2-digit number" should "return itself" in {
    assert(sumOfCalibrationValues.calibrationValue("13") === 13)
  }

  "Non-digit characters" should "be ignored" in {
    assert(sumOfCalibrationValues.calibrationValue("1abc2") === 12)
  }

  "If more than 2 digits" should "use only the first and last" in {
    assert(sumOfCalibrationValues.calibrationValue("123") === 13)
  }

  "Single digit values" should "use that digit for both first and last" in {
    assert(sumOfCalibrationValues.calibrationValue("4") === 44)
  }

  "Examples given" should "sum as expected" in {
    assert(sumOfCalibrationValues.calibrationValue("pqr3stu8vwx") === 38)
    assert(sumOfCalibrationValues.calibrationValue("a1b2c3d4e5f") === 15)
    assert(sumOfCalibrationValues.calibrationValue("treb7uchet" ) === 77)
  }

  "Part 1 " should "Give the correct sum" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day1CalibrationValues.txt")
    println(source.getLines().map(line => sumOfCalibrationValues.calibrationValue(line)).sum)
    source.close()
  }

  // Part 2

  "A single spelt number" should "should be used for both the first and last number" in {
    assert(sumOfCalibrationValues.calibrationValueWithWords("two") === 22)
  }

  "Two spelt numbers" should "are used for the first and last number" in {
    assert(sumOfCalibrationValues.calibrationValueWithWords("twothree") === 23)
  }

  "Part 2 Examples given" should "sum as expected" in {
    assert(sumOfCalibrationValues.calibrationValueWithWords("two1nine"        ) === 29)
    assert(sumOfCalibrationValues.calibrationValueWithWords("eightwothree"    ) === 83)
    assert(sumOfCalibrationValues.calibrationValueWithWords("abcone2threexyz" ) === 13)
    assert(sumOfCalibrationValues.calibrationValueWithWords("xtwone3four"     ) === 24)
    assert(sumOfCalibrationValues.calibrationValueWithWords("4nineeightseven2") === 42)
    assert(sumOfCalibrationValues.calibrationValueWithWords("zoneight234"     ) === 14)
    assert(sumOfCalibrationValues.calibrationValueWithWords("7pqrstsixteen"   ) === 76)
  }

  "Part 2" should "Give the correct sum" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day1CalibrationValues.txt")
    println(source.getLines().map(line => sumOfCalibrationValues.calibrationValueWithWords(line)).sum)
    source.close()
  }

}
