package aoc2024.day16

import utils.ParseFile.parseFile

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.math.abs

class ReindeerMazeDijkstraFullPath {

  def solve(file: String): Int = {

    val maze = parseFile(file)

    val startPoint = locationsOf('S', maze)
    val endPoint   = locationsOf('E', maze)
    val tilePoints = locationsOf('.', maze)

    val unvisited: Map[Position, Set[Seq[Position]]] =
      Map((startPoint ++ tilePoints ++ endPoint).
        flatMap(point => point.allPositions()).
        map(position => (position, Set(Seq[Position]()))).
        toMap.toSeq: _*)

    val startPosition = Position(startPoint.head, E)
    unvisited(startPosition) = Set(Seq(startPosition))

    findShortestRoutesToEnd(unvisited, maze, endPoint.head).
      flatMap(path => path.map(position => position.point).toSet).
      size
  }

  @tailrec
  private def findShortestRoutesToEnd(unvisited: Map[Position, Set[Seq[Position]]], maze: List[String], end: Point): Set[Seq[Position]] = {
    if (unvisited.isEmpty)
      return Set(Seq())

    val (currentPosition, currentMinPaths) = unvisited.minBy{ case (position: Position, path: Set[Seq[Position]]) => score(path.head)}

    if (currentPosition.point == end)
      return currentMinPaths

    currentPosition.neighbours().
      intersect(unvisited.keySet).
      map(neighbour => (neighbour, currentMinPaths.map(path => path :+ neighbour))).
      foreach{ case (neighbour, paths) =>
        if (score(paths.head) < score(unvisited(neighbour).head))
          unvisited(neighbour) = paths
        else if (score(paths.head) == score(unvisited(neighbour).head))
          unvisited(neighbour) = unvisited(neighbour) ++ paths
      }

    unvisited.remove(currentPosition)

    findShortestRoutesToEnd(unvisited, maze, end)
  }

  private def score(path: Seq[Position]): Int = {
    if (path.isEmpty)
      Int.MaxValue
    else
      path.zip(path.tail).map{ case(p1, p2) => p1.pointsTo(p2) }.sum
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

    def neighbours(): Set[Position] = Set(forward, right, left)

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

    def pointsTo(other: Position): Int =
      if (this.point == other.point &&
          this.dir   != other.dir) 1000 else 1
  }

  val N: Dir = Dir(-1,  0)
  val S: Dir = Dir( 1,  0)
  val E: Dir = Dir( 0,  1)
  val W: Dir = Dir( 0, -1)

}
