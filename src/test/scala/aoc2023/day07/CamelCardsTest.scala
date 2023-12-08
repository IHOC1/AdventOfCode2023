package aoc2023.day07

import aoc2023.day07.CamelCards.totalWinnings
import aoc2023.day07.HandType._
import org.scalatest.flatspec.AnyFlatSpec

class CamelCardsTest extends AnyFlatSpec {

  "A Hand" should "have a type" in {
    assert(Hand("AAAAA").handType() === FiveOfAKind)
    assert(Hand("AA8AA").handType() === FourOfAKind)
    assert(Hand("23332").handType() === FullHouse)
    assert(Hand("TTT98").handType() === ThreeOfAKind)
    assert(Hand("23432").handType() === TwoPair)
    assert(Hand("A23A4").handType() === OnePair)
    assert(Hand("23456").handType() === HighCard)
  }

  "Hand comparison by hand type" should "be correct" in {
    assert(Hand("AAAAA").compareTo(Hand("AAAAA")) === 0)
    assert(Hand("AA8AA").compareTo(Hand("AA8AA")) === 0)
    assert(Hand("23332").compareTo(Hand("23332")) === 0)
    assert(Hand("TTT98").compareTo(Hand("TTT98")) === 0)
    assert(Hand("23432").compareTo(Hand("23432")) === 0)
    assert(Hand("A23A4").compareTo(Hand("A23A4")) === 0)
    assert(Hand("23456").compareTo(Hand("23456")) === 0)

    assert(Hand("AAAAA").compareTo(Hand("AA8AA")) > 0)
    assert(Hand("AA8AA").compareTo(Hand("23332")) > 0)
    assert(Hand("23332").compareTo(Hand("TTT98")) > 0)
    assert(Hand("TTT98").compareTo(Hand("23432")) > 0)
    assert(Hand("23432").compareTo(Hand("A23A4")) > 0)
    assert(Hand("A23A4").compareTo(Hand("23456")) > 0)

    assert(Hand("AA8AA").compareTo(Hand("AAAAA")) < 0)
    assert(Hand("23332").compareTo(Hand("AA8AA")) < 0)
    assert(Hand("TTT98").compareTo(Hand("23332")) < 0)
    assert(Hand("23432").compareTo(Hand("TTT98")) < 0)
    assert(Hand("A23A4").compareTo(Hand("23432")) < 0)
    assert(Hand("23456").compareTo(Hand("A23A4")) < 0)
  }

  "Hand comparison by card strength" should "be correct" in {
    assert(Hand("AAAAK").compareTo(Hand("AAAAQ")) > 0)
    assert(Hand("AAAKA").compareTo(Hand("AAAQA")) > 0)
    assert(Hand("AAKAA").compareTo(Hand("AAQAA")) > 0)
    assert(Hand("AKAAA").compareTo(Hand("AQAAA")) > 0)
  }

  "Total winnings for test data" should "be correct" in {
    assert(totalWinnings("Day07CamelCardsTestData.txt") === 6440)
  }

  "Total winnings for data" should "be correct" in {
    assert(totalWinnings("Day07CamelCards.txt") === 252295678)
  }

}
