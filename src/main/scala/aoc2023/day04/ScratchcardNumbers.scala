package aoc2023.day04

import scala.math._

class ScratchcardNumbers {
}

case class Card(cardNum: Int, winningNumbers: Seq[Int], numbersYouHave: Seq[Int]) {

  def score(): Int = numberOfYourWinningNumbers match {
    case 0 => 0
    case n => 1 << (n - 1)
  }

  def numberOfYourWinningNumbers: Int = yourWinningNumbers().size

  def yourWinningNumbers(): Set[Int] = winningNumbers.toSet.intersect(numbersYouHave.toSet)

}

case class MultipleCards(num: Int, card: Card)

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

  def sumOfWinningCards(cards: Seq[Card]): Int = {
    val cardCounts = cards.map(MultipleCards(1, _: Card))

    sumOfWinningCards(0, cardCounts)
  }

  def sumOfWinningCards(sum: Int, cards: Seq[MultipleCards]): Int = {
    if (cards.isEmpty)
      sum
    else {
      val current = cards.head
      val remaining = cards.tail

      val numSubsequentCardsWon = current.card.numberOfYourWinningNumbers

      val nextCardsWon = remaining.
        take(numSubsequentCardsWon).
        map((mc: MultipleCards) => mc.copy(num = mc.num + current.num))

      val updatedRemaining = nextCardsWon ++ remaining.takeRight(max(remaining.size - numSubsequentCardsWon, 0))

      sumOfWinningCards(sum + current.num, updatedRemaining)
    }
  }
}