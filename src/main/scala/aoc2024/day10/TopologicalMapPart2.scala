package aoc2024.day10

import utils.ParseFile.parseFile

class TopologicalMapPart2 {

  def sumTrailheadScores(file: String): Long = {
    val lines = parseFile(file)
    val topologicalMap = lines.map(line => line.iterator.map(c => if (c == '.') -1 else c.toString.toInt).toSeq).toSeq

    topologicalMap.indices.map(rowIndex =>
      topologicalMap(rowIndex).indices.map(colIndex =>
        numTrailheads(topologicalMap, rowIndex, colIndex)
      ).sum
    ).sum
  }

  private def numTrailheads(topologicalMap: Seq[Seq[Int]], rowIndex: Int, colIndex: Int): Int =
    if (topologicalMap(rowIndex)(colIndex) != 0)
      0
    else
      numTrailsFrom(topologicalMap, rowIndex, colIndex)

  private def numTrailsFrom(topologicalMap: Seq[Seq[Int]], rowIndex: Int, colIndex: Int): Int = {
    if (topologicalMap(rowIndex)(colIndex) == 9)
      1
    else
      Seq((-1,0), (1,0), (0,-1), (0,1)).map{ case (rowDelta, colDelta) =>
          val nextRowIndex = rowIndex + rowDelta
          val nextColIndex = colIndex + colDelta
          if (withinBounds(topologicalMap, nextRowIndex, nextColIndex) &&
              topologicalMap(nextRowIndex)(nextColIndex) == topologicalMap(rowIndex)(colIndex) + 1)
            numTrailsFrom(topologicalMap, nextRowIndex, nextColIndex)
          else
            0
      }.sum
  }

  private def withinBounds(topologicalMap: Seq[Seq[Int]], rowIndex: Int, colIndex: Int) =
    0 <= rowIndex && rowIndex < topologicalMap.size &&
    0 <= colIndex && colIndex < topologicalMap.head.size

}
