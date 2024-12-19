package aoc2024.day12

import utils.ParseFile.parseFile

class Garden(file: String) {

  val garden = parseFile(file).map(line => line.toCharArray).toArray

  def fencePrice(): Int = {
    gardenAreas(allLocations()).
      toSeq.
      map(region => region.cost).
      sum
  }

  private def gardenAreas(remainingGarden: Set[Point]): Set[Region] = {
    if (remainingGarden.isEmpty)
      Set()
    else {
      val startPoint = Set(remainingGarden.head)
      val region = findRegion(startPoint, Region(startPoint), remainingGarden.diff(startPoint))
      gardenAreas(remainingGarden.diff(region.points)) ++ Set(region)
    }
  }

  def findRegion(points: Set[Point],
                 region: Region,
                 remainingGarden: Set[Point]): Region = {
    if (points.isEmpty)
      region
    else {
      val neighboursAlsoInArea = points.
        flatMap(point => point.
          neighbours().
          intersect(remainingGarden). // Eliminates any neighbours not in the garden or already part of region
          filter(neighbour => neighbour.letter(garden) == point.letter(garden)))
      findRegion(neighboursAlsoInArea, region.union(neighboursAlsoInArea), remainingGarden.diff(neighboursAlsoInArea))
    }
  }

  def allLocations(): Set[Point] =
    garden.indices.flatMap(rowIndex => garden.head.indices.map(colIndex => Point(rowIndex, colIndex))).toSet

}

case class Point(row: Int, col: Int) {

  def neighbours(): Set[Point] = Set(
    Point(row + 1, col + 0),
    Point(row - 1, col + 0),
    Point(row + 0, col + 1),
    Point(row + 0, col - 1)
  )

  def letter(garden: Array[Array[Char]]): Char = garden(row)(col)

}

case class Region(points: Set[Point]) {

  def union(otherPoints: Set[Point]): Region =
    Region(points.union(otherPoints))

  def area: Int = points.size

  def perimeter: Int =
    points.
      toSeq.
      flatMap(point => point.neighbours()).
      count(neighbour => !points.contains(neighbour))

  def cost: Int = area * perimeter
}
