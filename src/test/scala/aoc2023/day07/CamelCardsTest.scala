package aoc2023.day07

import aoc2023.day07.HandType.{FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind}
import org.scalatest.flatspec.AnyFlatSpec

class CamelCardsTest extends AnyFlatSpec {

  "A Hand" should "have a type" in {
    assert(Hand("AAAAA").handType() === FiveOfAKind)
    assert(Hand("AA8AA").handType() === FourOfAKind)
    assert(Hand("23332").handType() === FullHouse)
    assert(Hand("TTT98").handType() === ThreeOfAKind)
  }
}
