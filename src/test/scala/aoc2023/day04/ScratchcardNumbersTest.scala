package aoc2023.day04

import aoc2023.day04.ScratchcardNumbers.{parseLine, sumOfWinningCards}
import org.scalatest.flatspec.AnyFlatSpec

class ScratchcardNumbersTest extends AnyFlatSpec {

  "A line" should "be parsed as a Card" in {
    assert(parseLine("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") ===
      Card(1, Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)))
  }

  "A card" should "derive your winning numbers" in {
      assert(Card(1, Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)).yourWinningNumbers() === Set(48, 83, 86, 17))
  }

  "A card" should "give you your score" in {
      assert(Card(1, Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)).score() === 8)
  }

  "Example test data" should "generate the expected scores" in {
    assert(parseLine("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53").score() === 8)
    assert(parseLine("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19").score() === 2)
    assert(parseLine("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1").score() === 2)
    assert(parseLine("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83").score() === 1)
    assert(parseLine("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36").score() === 0)
    assert(parseLine("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11").score() === 0)
  }

  "Part 1" should "Give the correct sum of winning points" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day04ScratchCards.txt")
    val score: Int = source.getLines().
      map((line: String) => parseLine(line)).
      map(_.score()).
      sum

    assert(score === 25174)

    source.close()
  }

  // Part 2

  "Example test data" should "generate 30 winning scratchcards " in {
    val exampleCards = Seq(
      parseLine("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"),
      parseLine("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"),
      parseLine("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"),
      parseLine("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"),
      parseLine("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"),
      parseLine("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))
    assert(sumOfWinningCards(exampleCards) === 30)
  }

  "Part 2" should "Give the correct sum of winning points" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day04ScratchCards.txt")
    val cards = source.getLines().
      map((line: String) => parseLine(line)).toSeq

    val totalNumberOfCards = sumOfWinningCards(cards)

    println(totalNumberOfCards)

    assert(totalNumberOfCards === 6420979)

    source.close()
  }

}
