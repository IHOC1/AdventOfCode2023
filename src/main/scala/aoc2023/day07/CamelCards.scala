package aoc2023.day07

import aoc2023.day07.HandType.{FiveOfAKind, FourOfAKind}

class CamelCards {

}

object HandType extends Enumeration {
  type HandType = Value
  val FiveOfAKind, FourOfAKind = Value
}

case class Hand(cards: String) {

  def handType() = {
    val groupedCards = cards.groupBy(c => c)

    if (groupedCards.size == 1) FiveOfAKind
    else if (groupedCards.size == 2 && Set(1, 4).contains(groupedCards.head._2.length)) FourOfAKind
  }

}
