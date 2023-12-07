package aoc2023.day07

import aoc2023.day07.HandType.{FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair}

class CamelCards {

}

object HandType extends Enumeration {
  type HandType = Value
  val FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair = Value
}

case class Hand(cards: String) {

  def handType() = {
    val groupedCards = cards.groupBy(c => c)
    val cardGroups = groupedCards.values
    val cardGroupSizes = cardGroups.map(_.length).toList.sorted

    println(cardGroupSizes)

         if (cardGroupSizes == List(5)) FiveOfAKind
    else if (cardGroupSizes == List(1, 4)) FourOfAKind
    else if (cardGroupSizes == List(2, 3)) FullHouse
    else if (cardGroupSizes == List(1, 1, 3)) ThreeOfAKind
    else if (cardGroupSizes == List(1, 2, 2)) TwoPair
  }

}
