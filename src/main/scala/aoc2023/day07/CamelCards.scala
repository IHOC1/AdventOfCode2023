package aoc2023.day07

import aoc2023.day07.HandType.{FiveOfAKind, FourOfAKind, FullHouse, HighCard, OnePair, ThreeOfAKind, TwoPair}

class CamelCards {

}

object CamelCards {

  def totalWinnings(fileName: String): Int = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + fileName)
    val handsToBids = source.getLines().map(line => parseLine(line)).toMap
    source.close()

    val hands: Iterable[Hand] = handsToBids.keys.toList.sorted
    val ranks = 1 until handsToBids.size + 1

    hands.zip(ranks).toList.
      map(p => handsToBids(p._1) * p._2).
      sum
  }

  def parseLine(line: String): (Hand, Int) = {
    val parts = line.split(" ")

    val hand = Hand(parts(0))
    val bid = parts(1).toInt

    hand -> bid
  }
}

object HandType extends Enumeration {
  type HandType = Value
  val FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard = Value
}

case class Hand(cards: String) extends Comparable[Hand] {

  def handType() = {
    val cardGroups = cards.groupBy(c => c).values
    val cardGroupSizes = cardGroups.map(_.length).toList.sorted

         if (cardGroupSizes == List(5)         ) FiveOfAKind
    else if (cardGroupSizes == List(1, 4)      ) FourOfAKind
    else if (cardGroupSizes == List(2, 3)      ) FullHouse
    else if (cardGroupSizes == List(1, 1, 3)   ) ThreeOfAKind
    else if (cardGroupSizes == List(1, 2, 2)   ) TwoPair
    else if (cardGroupSizes == List(1, 1, 1, 2)) OnePair
    else                                         HighCard
  }

  val cardToStrength = Map('A' -> 13,
                           'K' -> 12,
                           'Q' -> 11,
                           'J' -> 10,
                           'T' -> 9,
                           '9' -> 8,
                           '8' -> 7,
                           '7' -> 6,
                           '6' -> 5,
                           '5' -> 4,
                           '4' -> 3,
                           '3' -> 2,
                           '2' -> 1)

  override def compareTo(other: Hand): Int = {
    -handType().compare(other.handType()) match {
      case       0 => compareHandsOnCardStrength(other)
      case unequal => unequal
    }
  }

  private def compareHandsOnCardStrength(o: Hand) = {
    cards.zip(o.cards).
      map(pair => compareCards(pair)).
      find(comparison => comparison != 0).getOrElse(0)
  }

  def compareCards(cards: (Char, Char)): Int = {
    cardToStrength(cards._1).compare(cardToStrength(cards._2))
  }

}
