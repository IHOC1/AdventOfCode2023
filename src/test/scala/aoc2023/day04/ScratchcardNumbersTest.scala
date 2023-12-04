package aoc2023.day04

import aoc2023.day04.ScratchcardNumbers.parseLine
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

}
