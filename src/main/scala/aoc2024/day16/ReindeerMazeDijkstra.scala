package aoc2024.day16

import utils.ParseFile.parseFile

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.math.abs

class ReindeerMazeDijkstra {

  def solve(file: String): Int = {
    val maze = parseFile(file)

    val startPoint = locationsOf('S', maze)
    val endPoint   = locationsOf('E', maze)
    val tilePoints = locationsOf('.', maze)

    val unvisited: Map[Position, Int] =
      collection.mutable.Map((startPoint ++ tilePoints ++ endPoint).
        flatMap(point => point.allPositions()).
        map(position => (position, Int.MaxValue)).
        toMap.toSeq: _*)

    unvisited(Position(startPoint.head, E)) = 0

    findShortestRouteToEnd(unvisited, maze, endPoint.head)
  }

  @tailrec
  private def findShortestRouteToEnd(unvisited: Map[Position, Int], maze: List[String], end: Point): Int = {
    if (unvisited.isEmpty)
      return -1

    val minDistance = unvisited.values.min
    val currentNode = unvisited.find { case (position, distance) => distance == minDistance }.get._1

    if (unvisited(currentNode) == Int.MaxValue)
      return -2

    if (currentNode.point == end)
      return unvisited(currentNode)

    currentNode.neighbours().
      filter { case (neighbour, cost) => unvisited.keySet.contains(neighbour) }.
      map    { case (neighbour, cost) => (neighbour, unvisited(currentNode) + cost) }.
      foreach{ case (neighbour, cost) => if (cost < unvisited(neighbour)) unvisited(neighbour) = cost}

    unvisited.remove(currentNode)

    findShortestRouteToEnd(unvisited, maze, end)
  }

  private def locationsOf(c: Char, maze: List[String]): Seq[Point] = {
    maze.zipWithIndex.
      flatMap { case (row, rowIndex) =>
        row.zipWithIndex.
          filter { case (ch, colIndex) => ch == c }.
          map    { case (ch, colIndex) => Point(rowIndex, colIndex) }
      }
  }

  case class Point(row: Int, col: Int) {
    def + (delta: Dir): Point = Point(row + delta.row, col + delta.col)

    def allPositions(): Set[Position] = Set(N,S,E,W).map(dir => Position(this, dir))
  }

  case class Dir(row: Int, col: Int)

  case class Position(point: Point, dir: Dir) {

    def neighbours(): Set[(Position, Int)] =
      Set((forward, 1), (right, 1000), (left, 1000))

    private def forward: Position = this.copy(point = this.point + dir)

    private def right: Position = this.copy(dir = dir match {
      case N => E
      case E => S
      case S => W
      case W => N
    })

    private def left: Position = this.copy(dir = dir match {
      case N => W
      case E => N
      case S => E
      case W => S
    })

  }

  val N: Dir = Dir(-1,  0)
  val S: Dir = Dir( 1,  0)
  val E: Dir = Dir( 0,  1)
  val W: Dir = Dir( 0, -1)

}
