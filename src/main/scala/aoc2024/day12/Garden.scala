package aoc2024.day12

import aoc2024.day12.Garden.{down, left, right, up}
import utils.ParseFile.parseFile

class Garden(file: String) {

  val garden = parseFile(file).map(line => line.toCharArray).toArray

  def fencePrice(): Int = {
    gardenAreas(allLocations()).
      toSeq.
      map(region => region.cost).
      sum
  }

  def fencePriceBulkDiscount(): Int = {
    gardenAreas(allLocations()).
      toSeq.
      map(region => region.bulkDiscountCost).
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
    this.plus(down),
    this.plus(up),
    this.plus(right),
    this.plus(left)
  )

  def letter(garden: Array[Array[Char]]): Char = garden(row)(col)

  def plus(dir: Direction): Point =
    Point(row + dir.row,
          col + dir.col)
}


case class Direction(row : Int, col : Int)

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

  def bulkDiscountCost: Int  = area * sides.size

  def sides: Set[Side] = {
    points.
      flatMap(point =>
        Set(up, down, left, right).map(dir => (point, dir))).
      filter { case (point, dir) => !points.contains(point.plus(dir))}.
      groupBy{ case (point, dir) => dir}.
      map { case (dir, pointsAndDirs) => (dir, pointsAndDirs.map{ case (point, _) => point}) }.
      flatMap { case (dir, points) =>
        val startPoint = Set(points.head)
        findSides(startPoint, startPoint, points.diff(startPoint), dir)
      }.
      toSet
  }

  private def findSides(searchPoints: Set[Point],
                        sideSoFar: Set[Point],
                        remainingPoints: Set[Point],
                        dir: Direction): Set[Side] = {
    if (remainingPoints.isEmpty)
      Set(Side(searchPoints, dir))
    else if (searchPoints.isEmpty) {
      val startPoint = Set(remainingPoints.head)
      Set(Side(sideSoFar, dir)) ++ findSides(startPoint, startPoint, remainingPoints.diff(startPoint), dir)
    }
    else {
      val neighboursAlsoInSide = searchPoints.
        flatMap(point => point.neighbours().intersect(remainingPoints))
      findSides(neighboursAlsoInSide, sideSoFar.union(neighboursAlsoInSide), remainingPoints.diff(neighboursAlsoInSide), dir)
    }
  }

}

case class Side(points: Set[Point], dir: Direction)

object Garden {
  val down  = Direction( 1,  0)
  val up    = Direction(-1,  0)
  val right = Direction( 0,  1)
  val left  = Direction( 0, -1)
}