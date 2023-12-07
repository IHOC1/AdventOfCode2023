package aoc2023.day07

import aoc2023.day07.HandType.FiveOfAKind

class CamelCards {

}

object HandType extends Enumeration {
  type HandType = Value
  val FiveOfAKind = Value
}

case class Hand(cards: String) {

  def handType() = {
    if (cards.tail.forall(card => card == cards.head))
      FiveOfAKind
  }

}
