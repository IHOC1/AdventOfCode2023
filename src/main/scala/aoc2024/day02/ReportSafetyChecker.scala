package aoc2024.day02

import utils.ParseFile.parseFile

class ReportSafetyChecker {

  def safeReportCount(file: String): Int = {
    parseFile(file).
      map(line => parseReport(line)).
      count(report => report.safe)
  }

  def safeReportCountWithDampener(file: String): Int = {
    parseFile(file).
      map(line => parseReport(line)).
      count(report => report.safeWithDampener)
  }

  private def parseReport(line: String): Report = Report(line.split(" +").map(level => level.toInt).toSeq)

}

case class Report(levels: Seq[Int]) {
  def safe: Boolean = allIncreasing || allDecreasing

  private def levelPairs(levels: Seq[Int]): Seq[(Int, Int)] = levels.zip(levels.tail)

  def allIncreasing: Boolean = allIncreasing(levels)
  def allIncreasing(levels: Seq[Int]): Boolean = allIncreasingForPairs(levelPairs(levels))
  def allIncreasingForPairs(pairs: Seq[(Int, Int)]): Boolean = pairs.forall { case (l1, l2) => isIncreasing(l1, l2) }

  def allDecreasing: Boolean = allDecreasing(levels)
  def allDecreasing(levels: Seq[Int]): Boolean = allDecreasingForPairs(levelPairs(levels))
  def allDecreasingForPairs(pairs: Seq[(Int, Int)]): Boolean = pairs.forall { case (l1, l2) => isDecreasing(l1, l2) }

  def safeWithDampener: Boolean = allIncreasingWithDampener || allDecreasingWithDampener

  def allIncreasingWithDampener: Boolean = {
    levels.dropRight(1).indices.map(i => isIncreasing(levels(i), levels(i + 1))).count(increasing => !increasing) match {
      case 0 => true
      case _ =>
        println("Inc " + levels)
        levels.dropRight(1).indices.
          map(i =>
            if (isIncreasing(levels(i), levels(i + 1)))
              false
            else {
              val (part1, part2) = levels.splitAt(i + 1)
              allIncreasing(part1.dropRight(1) ++ part2) || allIncreasing(part1 ++ part2.tail)
            }).
          find(b => b).getOrElse(false)
    }
  }

  private def isIncreasing(l1: Int, l2: Int) = l1 + 1 to l1 + 3 contains l2

  def allDecreasingWithDampener: Boolean = {
    levels.dropRight(1).indices.map(i => isDecreasing(levels(i), levels(i + 1))).count(decreasing => !decreasing) match {
      case 0 => true
      case _ =>
        println("Dec " + levels)
        levels.dropRight(1).indices.
        map(i =>
          if (isDecreasing(levels(i), levels(i + 1)))
            false
          else {
            val (part1, part2) = levels.splitAt(i + 1)
            allDecreasing(part1.dropRight(1) ++ part2) || allDecreasing(part1 ++ part2.tail)
          }).
        find(b => b).getOrElse(false)
    }
  }

  private def isDecreasing(l1: Int, l2: Int): Boolean = l1 - 3 to l1 - 1 contains l2
}
