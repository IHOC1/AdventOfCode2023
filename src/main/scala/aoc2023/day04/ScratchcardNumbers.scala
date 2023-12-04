package aoc2023.day04

class ScratchcardNumbers {
}

case class Card(cardNum: Int, winningNumbers: Seq[Int], numbersYouHave: Seq[Int])

object ScratchcardNumbers {

  def parseLine(line: String): Card = {
    val colonSeparateParts = line.split(": +")
    val cardNum: Int = parseCardNum(colonSeparateParts(0))

    val verticalBarSeparatedParts = colonSeparateParts(1).split(" +\\| +")
    val winningNumbers = parseNumbers(verticalBarSeparatedParts(0))
    val numbersYouHave = parseNumbers(verticalBarSeparatedParts(1))

    Card(cardNum, winningNumbers, numbersYouHave)
  }

  private def parseCardNum(cardStr: String): Int = cardStr.split(" +")(1).toInt

  private def parseNumbers(numbers: String): Seq[Int] = numbers.split(" +").map(_.toInt).toSeq

}