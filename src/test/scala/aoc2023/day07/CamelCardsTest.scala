package aoc2023.day07

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
}
