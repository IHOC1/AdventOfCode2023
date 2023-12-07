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

    if (groupedCards.size == 1) FiveOfAKind
    else if (groupedCards.size == 2 && Set(1, 4).contains(groupedCards.head._2.length)) FourOfAKind
    else if (groupedCards.size == 2 && Set(2, 3).contains(groupedCards.head._2.length)) FullHouse
    else if (groupedCards.size == 3 && cardGroups.map(_.length).toSet == Set(3, 1, 1)) ThreeOfAKind
  }

}
