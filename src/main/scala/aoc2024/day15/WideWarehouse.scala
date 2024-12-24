package aoc2024.day15

import utils.ParseFile.parseFile

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

class WideWarehouse {

  def sumOfFinalBoxGPSCoordinates(file: String): Int = {
    val lines = parseFile(file)
    val (map, moves) = lines.splitAt(lines.indexOf(""))
    val warehouseMap = parseWarehouse(map)

    warehouseMap.moveRobot(moves.mkString.toCharArray)

    warehouseMap.boxCoordinates().
      map(point => point.gpsCoord()).
      sum
  }

  private def parseWarehouse(map: List[String]): Warehouse = {
    val warehouse: Map[Point, MapElement] = Map().withDefaultValue(new EmptySpace)

    map.indices.foreach(row =>
      map(row).indices.foreach(col => {

        warehouse(Point(row, 2 * col)) =
          map(row)(col) match {
            case '#' => new Wall
            case 'O' => new BoxLeft
            case '@' => new Robot
            case '.' => new EmptySpace
          }

        warehouse(Point(row, 2 * col + 1)) =
          map(row)(col) match {
            case '#' => new Wall
            case 'O' => new BoxRight
            case '@' => new EmptySpace
            case '.' => new EmptySpace
          }
      })
    )

    new Warehouse(warehouse)
  }

  case class Point(row: Int, col: Int) {
    def nextPoint(dir: Char): Point = dir match {
      case '^' => this.copy(row = this.row - 1)
      case 'v' => this.copy(row = this.row + 1)
      case '>' => this.copy(col = this.col + 1)
      case '<' => this.copy(col = this.col - 1)
    }

    def gpsCoord(): Int = 100 * row + col

    def right(): Point = this.copy(col = this.col + 1)
    def left (): Point = this.copy(col = this.col - 1)

  }

  class Warehouse(map: Map[Point, MapElement]) {

    @tailrec
    final def moveRobot(moves: Array[Char]): Boolean = {
      if (moves.isEmpty)
        true
      else {
        move(Set(robotLocation()), moves.head)
        moveRobot(moves.tail)
      }
    }

    def move(from: Set[Point], dir: Char): Boolean = {
      if (from.isEmpty)
        return true

      val obstacles =
        from.
          map(point => point.nextPoint(dir)).
          flatMap(point => map(point).allPoints(point, dir))

      if (obstacles.exists(point => map(point).isInstanceOf[Wall]))
        return false

      if (move(obstacles, dir)) {
        from.foreach(point => map(point.nextPoint(dir)) = map(point))
        from.foreach(point => map.remove(point))
        return true
      }

      false
    }

    private def robotLocation(): Point =
      map.
        find{ case (point, element: MapElement) => element.isInstanceOf[Robot] }.
        get._1

    def boxCoordinates(): mutable.Iterable[Point] = {
      map.
        filter { case(point, element) => element.isInstanceOf[BoxLeft] }.
        map    { case(point, box    ) => point                         }
    }

  }

  abstract sealed class MapElement{
    def allPoints(point: Point, dir: Char): Seq[Point]
  }

  class Robot extends MapElement {
    override def allPoints(point: Point, dir: Char): Seq[Point] = Seq()
    override def toString: String = "@"
  }

  class Wall extends MapElement {
    override def allPoints(point: Point, dir: Char): Seq[Point] = Seq(point)
    override def toString: String = "#"
  }

  class BoxLeft extends MapElement {
    override def allPoints(point: Point, dir: Char): Seq[Point] =
      if (dir == '^' || dir == 'v') Seq(point, point.right()) else Seq(point)
    override def toString: String = "["
  }

  class BoxRight extends MapElement {
    override def allPoints(point: Point, dir: Char): Seq[Point] =
      if (dir == '^' || dir == 'v') Seq(point, point.left()) else Seq(point)
    override def toString: String = "]"
  }

  class EmptySpace extends MapElement {
    override def allPoints(point: Point, dir: Char): Seq[Point] = Seq()
    override def toString: String = "."
  }

}
