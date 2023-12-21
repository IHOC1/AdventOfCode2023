package aoc2023.day12

import scala.annotation.tailrec

class HotSprings {
}

object HotSprings {

  def sumOfArrangements(filename: String): Long = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()

    lines.
      map((line: String) => {
        val parts = line.split(" ")
        (parts(0), parseContiguousGroup(parts(1)))
      }).
      map((conditionRecordAndGroup: (String, Seq[Int])) =>
        differentArrangements(conditionRecordAndGroup._1, conditionRecordAndGroup._2)).
      map(_.length).
      sum
  }

  private def parseContiguousGroup(group: String): Seq[Int] =
    group.split(",").map(_.toInt)

  def differentArrangements(conditionRecords: String, damagedSpringContiguousGroups: Seq[Int]): Seq[String] = {
    arrangements(damagedSpringContiguousGroups, Seq(conditionRecords)).
      filter((conditionRecord: String) => knownGroups(conditionRecord) == damagedSpringContiguousGroups)
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
