package aoc2023.day12

import scala.annotation.tailrec

class HotSprings {
}

object HotSprings {

  def differentArrangements(conditionRecords: String, damagedSpringContiguousGroups: Seq[Int]): Seq[String] = {
    arrangements(damagedSpringContiguousGroups, Seq(conditionRecords)).filter(conditionRecord => knownGroups(conditionRecord) == damagedSpringContiguousGroups)
  }

  @tailrec
  private def arrangements(expectedGroups: Seq[Int], possibleConditionRecords: Seq[String]): Seq[String] = {
    if (!possibleConditionRecords.head.contains("?"))
      possibleConditionRecords
    else {
      val nextGeneration = generateForNextUnknown(possibleConditionRecords.head)
      arrangements(expectedGroups, possibleConditionRecords.tail ++ nextGeneration)
    }
  }

  private def generateForNextUnknown(conditionRecord: String): Seq[String] =
    Seq(".", "#").map((replacement: String) => conditionRecord.replaceFirst("\\?", replacement))

  private def knownGroups(conditionRecord: String): Seq[Int] = {
    conditionRecord.split("\\.+").toList.filter(_.nonEmpty).map(_.length)
  }

}
