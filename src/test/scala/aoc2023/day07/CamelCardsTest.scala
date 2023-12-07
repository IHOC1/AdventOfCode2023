package aoc2023.day07

import aoc2023.day07.HandType.{FiveOfAKind, FourOfAKind}
import org.scalatest.flatspec.AnyFlatSpec

class CamelCardsTest extends AnyFlatSpec {

  "A Hand" should "have a type" in {
    assert(Hand("AAAAA").handType() === FiveOfAKind)
    assert(Hand("AA8AA").handType() === FourOfAKind)
  }
}
