package aoc2023.day07

import aoc2023.day07.HandType.{FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind}

class CamelCards {

}

object HandType extends Enumeration {
  type HandType = Value
  val FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind = Value
}

case class Hand(cards: String) {

  def handType() = {
    val groupedCards = cards.groupBy(c => c)
    val cardGroups = groupedCards.values
    val cardGroupSizes = cardGroups.map(_.length).toSet

    if (cardGroupSizes == Set(5)) FiveOfAKind
    else if (cardGroupSizes == Set(1, 4)) FourOfAKind
    else if (cardGroupSizes == Set(2, 3)) FullHouse
    else if (cardGroupSizes == Set(3, 1, 1)) ThreeOfAKind
  }

}
