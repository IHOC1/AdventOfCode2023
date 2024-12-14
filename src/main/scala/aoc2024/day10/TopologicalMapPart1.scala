package aoc2024.day10

import utils.ParseFile.parseFile

class TopologicalMapPart1 {

  def sumTrailheadScores(file: String): Long = {
    val lines = parseFile(file)
    val topologicalMap = lines.map(line => line.iterator.map(c => if (c == '.') -1 else c.toString.toInt).toSeq)

    topologicalMap.indices.flatMap(rowIndex =>
      topologicalMap(rowIndex).indices.flatMap(colIndex =>
        numTrailheads(topologicalMap, rowIndex, colIndex)
      )
    ).size
  }

  private def numTrailheads(topologicalMap: Seq[Seq[Int]], rowIndex: Int, colIndex: Int): Set[(Int, Int)] =
    if (topologicalMap(rowIndex)(colIndex) != 0)
      Set()
    else
      numTrailsFrom(topologicalMap, rowIndex, colIndex)

  private def numTrailsFrom(topologicalMap: Seq[Seq[Int]], rowIndex: Int, colIndex: Int): Set[(Int, Int)] = {
    if (topologicalMap(rowIndex)(colIndex) == 9)
      Set((rowIndex, colIndex))
    else
      Set((-1,0), (1,0), (0,-1), (0,1)).flatMap { case (rowDelta, colDelta) =>
          val nextRowIndex = rowIndex + rowDelta
          val nextColIndex = colIndex + colDelta
          if (withinBounds(topologicalMap, nextRowIndex, nextColIndex) &&
              topologicalMap(nextRowIndex)(nextColIndex) == topologicalMap(rowIndex)(colIndex) + 1)
            numTrailsFrom(topologicalMap, nextRowIndex, nextColIndex)
          else
            Set()
      }
  }

  private def withinBounds(topologicalMap: Seq[Seq[Int]], rowIndex: Int, colIndex: Int): Boolean =
    0 <= rowIndex && rowIndex < topologicalMap.size &&
    0 <= colIndex && colIndex < topologicalMap.head.size

}
