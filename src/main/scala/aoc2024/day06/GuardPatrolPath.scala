package aoc2024.day06

import utils.ParseFile.parseFile

import scala.annotation.tailrec

class GuardPatrolPath {

  def numDistinctPositionsVisited(file: String): Int = {
    val lines = parseFile(file)

    val guardRow = lines.indices.find(rowIndex => lines(rowIndex).contains("^")).get
    val guardCol = lines(guardRow).indexOf("^")

    val (before, after) = lines.splitAt(guardRow)
    val suitManufacturingLab = before ++ Seq(after.head.replace('^', '.')) ++ after.tail

    totalPositionsVisited(distinctPatrolPositions(suitManufacturingLab.map(line => line.toArray).toArray, (guardCol, guardRow), 'N'))
  }

  @tailrec
  private def distinctPatrolPositions(suitManufacturingLab: Array[Array[Char]],
                                     posn: (Int, Int),
                                     dir: Char): Array[Array[Char]] = {
    val nextPosn = dir match {
      case 'N' => (posn._1, posn._2 - 1)
      case 'S' => (posn._1, posn._2 + 1)
      case 'E' => (posn._1 + 1, posn._2)
      case 'W' => (posn._1 - 1, posn._2)
    }

    if (leavingMappedArea(suitManufacturingLab, nextPosn)) {
      suitManufacturingLab(posn._2)(posn._1) = 'X'
      suitManufacturingLab
    }
    else if (suitManufacturingLab(nextPosn._2)(nextPosn._1) == '#') {
      distinctPatrolPositions(suitManufacturingLab, posn, rightTurn(dir))
    }
    else {
      suitManufacturingLab(posn._2)(posn._1) = 'X'
      distinctPatrolPositions(suitManufacturingLab, nextPosn, dir)
    }
  }

  private def totalPositionsVisited(suitManufacturingLab: Array[Array[Char]]): Int =
    suitManufacturingLab.map(row => row.count(c => c == 'X')).sum

  private def rightTurn(dir: Char): Char = dir match {
    case 'N' => 'E'
    case 'E' => 'S'
    case 'S' => 'W'
    case 'W' => 'N'
  }

  def numPositionsToCauseALoop(file: String): Int = {
    val lines = parseFile(file)

    val guardRow = lines.indices.find(rowIndex => lines(rowIndex).contains("^")).get
    val guardCol = lines(guardRow).indexOf("^")

    val (before, after) = lines.splitAt(guardRow)
    val suitManufacturingLab = before ++ Seq(after.head.replace('^', '.')) ++ after.tail

    val patrolPath = distinctPatrolPositions(suitManufacturingLab.map(line => line.toArray).toArray, (guardCol, guardRow), 'N')

    lines.indices.map(obstructionRowIndex =>
      (0 until lines.head.length).map(obstructionColIndex =>
        if ((obstructionRowIndex == guardRow &&
             obstructionColIndex == guardCol) || patrolPath(obstructionRowIndex)(obstructionColIndex) != 'X')
          0
        else {
          val suiteManufacturingLabArray = suitManufacturingLab.map(line => line.toArray).toArray
          suiteManufacturingLabArray.clone()(obstructionRowIndex)(obstructionColIndex) = '#'
          willCauseLoop((obstructionRowIndex, obstructionColIndex),
                        suiteManufacturingLabArray,
                        (guardCol, guardRow),
                        'N',
                        Seq())
        }
      ).sum
    ).sum
  }

  @tailrec
  private def willCauseLoop(obstructionPosn: (Int, Int),
                            suitManufacturingLab: Array[Array[Char]],
                            posn: (Int, Int),
                            dir: Char,
                            visitedPositions: Seq[VisitedPosition]): Int = {
    val nextPosn = dir match {
      case 'N' => (posn._1, posn._2 - 1)
      case 'S' => (posn._1, posn._2 + 1)
      case 'E' => (posn._1 + 1, posn._2)
      case 'W' => (posn._1 - 1, posn._2)
    }

    val positionBeingLeft = VisitedPosition(posn._2, posn._1, dir)

    if (visitedPositions.contains(positionBeingLeft))                   // if loop detected
      1
    else if (leavingMappedArea(suitManufacturingLab, nextPosn))         // guard leaving mapped area
      0
    else if (suitManufacturingLab(nextPosn._2)(nextPosn._1) == '#')     // if hit block
      willCauseLoop(obstructionPosn, suitManufacturingLab, posn, rightTurn(dir), visitedPositions ++ Seq(positionBeingLeft))
    else                                                                // moving forward
      willCauseLoop(obstructionPosn, suitManufacturingLab, nextPosn, dir, visitedPositions ++ Seq(positionBeingLeft))
  }

  private def leavingMappedArea(suitManufacturingLab: Array[Array[Char]], nextPosn: (Int, Int)): Boolean =
    nextPosn._1 < 0 || nextPosn._1 >= suitManufacturingLab(0).length ||
    nextPosn._2 < 0 || nextPosn._2 >= suitManufacturingLab.length

  case class VisitedPosition(rowIndex: Int, colIndex: Int, dir: Char)

}
