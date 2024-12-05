package aoc2023.day12

import scala.annotation.tailrec
import scala.math

class HotSprings {
}

object HotSprings {

  def sumOfArrangements(filename: String): Long = {
    springData(parseFile(filename)).
      map((conditionRecordAndGroup: (String, Seq[Int])) =>
        numArrangements(conditionRecordAndGroup._1, conditionRecordAndGroup._2)).
      sum
  }

  def sumOfExpandedArrangements(filename: String): Long = {
    springData(parseFile(filename)).
      map(data => expand(data)).
      map((conditionRecordAndGroup: (String, Seq[Int])) => {
        println(conditionRecordAndGroup)
        numArrangements(conditionRecordAndGroup._1, conditionRecordAndGroup._2)
      }).
      sum
  }

  def numArrangements(conditionRecord: String, contiguousGroups: Seq[Int]): Int = {
    val trimmedConditionRecord = removeConsecutiveOperationalSprings(conditionRecord)
//    println(trimmedConditionRecord)
    numArrangements(trimmedConditionRecord.split("\\."), contiguousGroups)
  }

  def removeConsecutiveOperationalSprings(conditionRecord: String): String = {
    if (conditionRecord.length == 1)
      conditionRecord
    else
      if (conditionRecord.head == '.' && conditionRecord.tail.head == '.')
        removeConsecutiveOperationalSprings(conditionRecord.tail)
      else
        conditionRecord.head + removeConsecutiveOperationalSprings(conditionRecord.tail)
  }
  def numArrangements(conditionRecord: Array[String], contiguousGroups: Seq[Int]): Int = {
    if (conditionRecord.isEmpty && contiguousGroups.nonEmpty)
      return 0

    // fail if there are is not enough space left to accommodate all remaining contiguousGroups
    // Is this a generification of the guard clause above?

    val firstConditionRecord = conditionRecord.head
    // If contiguous group fits... May need to do more about matching???
    if (firstConditionRecord.length >= contiguousGroups.head + 1) {
      // Loop over starting positions and see if it fits to existing '#'
      // If fits
      // recurse with remaining piece from firstConditionRecord and the tail of the contiguousGroups
      // else
      // skip to next?
    }

    0
  }

  def numArrangementsPython(conditionRecord: String, size: Int, contiguousGroups: Seq[Int]): Int = {
    if (contiguousGroups.isEmpty)
      return (if (conditionRecord.forall(_ != '#')) 1 else 0)

    val group = contiguousGroups.head
    val remainingGroups = contiguousGroups.tail
    val requiredSpace = remainingGroups.sum + remainingGroups.length

    (0 until (size - requiredSpace - group + 1)).map((before: Int) => {
      val candidate = chars(before, '.') + chars(group, '#') + "."

      if (candidate.zip(conditionRecord).forall((pair: (Char, Char)) => pair._1 == pair._2 || pair._2 == '?'))
        numArrangementsPython(conditionRecord.substring(math.min(candidate.length, conditionRecord.length)),
                                  size - group - before - 1,
                                  remainingGroups)
      else 0
    }).sum
  }

  private def chars(before: Int, c: Char) = {
    (0 until before).map(_ => c).mkString("")
  }

  def expand(data: (String, Seq[Int])): (String, Seq[Int]) = {
    (expandConditionRecord(data._1), expandGroups(data._2))
  }

  private def expandConditionRecord(conditionRecord: String) =
    (0 until 5).map(_ => conditionRecord).mkString("?")

  private def expandGroups(groups: Seq[Int]) =
    (0 until 5).flatMap(_ => groups)

  private def springData(lines: List[String]) = {
    lines.
      map((line: String) => {
        val parts = line.split(" ")
        (parts(0), parseContiguousGroup(parts(1)))
      })
  }

  private def parseFile(filename: String): List[String] = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()
    lines
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
      val nextGeneration = generateForNextUnknown(possibleConditionRecords.head).
        filter((conditionRecord: String) => 
          completeGroupsMatch(conditionRecord, expectedGroups) && sufficientRoomForRemainingGroups(conditionRecord, expectedGroups))
      arrangements(expectedGroups, possibleConditionRecords.tail ++ nextGeneration)
    }
  }

  def completeGroupsMatch(conditionRecord: String, expectedGroups: Seq[Int]): Boolean = {
    val completedConditionRecord = conditionRecord.split("\\?")(0)
    val groups = groupsIn(completedConditionRecord)
    val latestGroupIndex = groups.length - 1

    val previouslyBuiltGroupsAreOfTheCorrectSize = groups.take(latestGroupIndex) == expectedGroups.take(latestGroupIndex)

    lazy val currentGroupBeingBuiltIsWithinSize = (latestGroupIndex < 0) ||
      (groups.length <= expectedGroups.length && groups(latestGroupIndex) <= expectedGroups(latestGroupIndex))

    previouslyBuiltGroupsAreOfTheCorrectSize && currentGroupBeingBuiltIsWithinSize
  }

  private def groupsIn(conditionRecord: String) =
    conditionRecord.split("\\.+").filter(_.nonEmpty).map(_.length).toList

  private def generateForNextUnknown(conditionRecord: String): Seq[String] =
    Seq(".", "#").map((replacement: String) => conditionRecord.replaceFirst("\\?", replacement))

  def sufficientRoomForRemainingGroups(conditionRecord: String, expectedGroups: Seq[Int]): Boolean = {

    val indexOfFirstUnknown = conditionRecord.indexOf("?")

    // Limiting to the start of a new block of damaged springs allows the implementation to be simpler,
    // and probably saves doing this check more often than is necessary.
    if (indexOfFirstUnknown <= 0 || !startOfDamagedBlock(conditionRecord, indexOfFirstUnknown))
      return true

    val remainingConditionRecord = conditionRecord.substring(indexOfFirstUnknown, conditionRecord.length)
    val remainingConditionRecordPossibleGroups = groupsIn(remainingConditionRecord)

    val groups = groupsIn(conditionRecord.substring(0, indexOfFirstUnknown))
    val remainingExpectedGroups = expectedGroups.drop(groups.length)

    canBeAccommodated(remainingExpectedGroups, remainingConditionRecordPossibleGroups)
  }

  @tailrec
  private def canBeAccommodated(expectedGroups: Seq[Int], possibleGroups: List[Int]): Boolean = {
    if (expectedGroups.isEmpty)
      true
    else if (possibleGroups.isEmpty)
      false
    else if (expectedGroups.head == possibleGroups.head)
      canBeAccommodated(expectedGroups.tail, possibleGroups.tail)
    else if (expectedGroups.head <= possibleGroups.head)
      canBeAccommodated(expectedGroups.tail, (possibleGroups.head - expectedGroups.head - 1) +: possibleGroups.tail)
    else
      canBeAccommodated(expectedGroups, possibleGroups.tail)
  }

  private def startOfDamagedBlock(conditionRecord: String, indexOfFirstUnknown: Int): Boolean =
    conditionRecord.charAt(indexOfFirstUnknown - 1) == '.'

  private def knownGroups(conditionRecord: String): Seq[Int] = {
    conditionRecord.split("\\.+").toList.filter(_.nonEmpty).map(_.length)
  }

}
