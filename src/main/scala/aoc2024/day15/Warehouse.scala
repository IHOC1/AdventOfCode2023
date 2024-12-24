package aoc2024.day15

import utils.ParseFile.parseFile

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

class Warehouse {

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
      map(row).indices.foreach(col =>
        warehouse(Point(row, col)) =
          map(row)(col) match {
            case '#' => new Wall
            case 'O' => new Box
            case '@' => new Robot
            case '.' => new EmptySpace
          }
      )
    )

    new Warehouse(warehouse, map.head.size, map.size)
  }

  case class Point(row: Int, col: Int) {

    def gpsCoord(): Int = 100 * row + col

  }

  class Warehouse(map: Map[Point, MapElement],
                  width: Int,
                  height: Int) {

    @tailrec
    final def moveRobot(moves: Array[Char]): Boolean = {
//      display()
      if (moves.isEmpty)
        true
      else {
        moveElementAt(robotLocation(), moves.head)
        moveRobot(moves.tail)
      }
    }

    private def display(): Unit = {
      (0 until height).foreach(row => {
          (0 until width).foreach(col =>
              print(map(Point(row, col)))
            )
          println("")
        }
      )
    }

    def moveElementAt(point: Point, dir: Char): Boolean = {
      val destination = dir match {
        case '^' => point.copy(row = point.row - 1)
        case 'v' => point.copy(row = point.row + 1)
        case '>' => point.copy(col = point.col + 1)
        case '<' => point.copy(col = point.col - 1)
      }

      if (map(destination).move(destination, dir, this)) {
        map(destination) = map(point)
        map.remove(point)
        true
      }
      else
        false

    }

    private def robotLocation(): Point =
      map.
        find{ case (point, element: MapElement) => element.isInstanceOf[Robot] }.
        get._1

    def boxCoordinates(): mutable.Iterable[Point] = {
      map.
        filter { case(point, element) => element.isInstanceOf[Box] }.
        map    { case(point, box    ) => point                     }
    }

  }

  abstract class MapElement{
    def move(point: Point, dir: Char, world: Warehouse): Boolean
  }

  class Robot extends MapElement {
    override def move(point: Point, dir: Char, world: Warehouse): Boolean = world.moveElementAt(point, dir)
    override def toString: String = "@"
  }

  class Wall extends MapElement {
    override def move(point: Point, dir: Char, world: Warehouse): Boolean = false
    override def toString: String = "#"
  }

  class Box extends MapElement {
    override def move(point: Point, dir: Char, world: Warehouse): Boolean = world.moveElementAt(point, dir)
    override def toString: String = "O"
  }

  class EmptySpace extends MapElement {
    override def move(point: Point, dir: Char, world: Warehouse): Boolean = true
    override def toString: String = "."
  }

}
