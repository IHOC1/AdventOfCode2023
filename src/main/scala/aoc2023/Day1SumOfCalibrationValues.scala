package aoc2023

class Day1SumOfCalibrationValues {

  def calibrationValue(text: String): Int = {
    val digits = text.toList.filter(_.isDigit)
    digits.head.asDigit * 10 + digits.last.asDigit
  }

  val numbers = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9
  )

  def toNumber(str: String, numbers: Map[String, Int]): Option[Int] =
    numbers.keys.
      find(num => str.startsWith(num)).
      map(num => numbers(num))

  def calibrationValueWithWords(text: String): Int = {

    val firstDigit: Int = Range(0, text.length).
      map(i => text.substring(i)).
      map(str => toNumber(str, numbers)).
      find(num => num.isDefined).get.get

    val lastDigit: Int = Range(0, text.length).reverse.
      map(i => text.substring(i)).
      map(str => toNumber(str, numbers)).
      find(num => num.isDefined).get.get

    firstDigit * 10 + lastDigit
  }

}
