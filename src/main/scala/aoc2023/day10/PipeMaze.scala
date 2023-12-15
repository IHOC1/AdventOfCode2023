package aoc2023.day10

import aoc2023.day10.Direction.{East, North, South, West}

class PipeMaze {

}

sealed trait Direction {
    def move(coord: Coordinate): Coordinate
    def opposite(): Direction
}

object Direction extends {
  case object North extends Direction {
    override def move(coord: Coordinate): Coordinate = coord.copy(y = coord.y - 1)
    override def opposite(): Direction = South
  }

  case object South extends Direction {
    override def move(coord: Coordinate): Coordinate = coord.copy(y = coord.y + 1)
    override def opposite(): Direction = North
  }

  case object East extends Direction {
    override def move(coord: Coordinate): Coordinate = coord.copy(x = coord.x + 1)
    override def opposite(): Direction = West
  }

  case object West extends Direction {
    override def move(coord: Coordinate): Coordinate = coord.copy(x = coord.x - 1)
    override def opposite(): Direction = East
  }

  val allDirections = List(North, South, East, West)
}


case class Coordinate(x: Int, y: Int) {

  def getPipe(pipesGrid: Array[Array[Pipe]]) = pipesGrid(y)(x)

  def move(direction: Direction): Coordinate = direction.move(this)

  def isInGrid(pipesGrid: Array[Array[Pipe]]) =
    (0 <= x && x < pipesGrid(0).length) &&
    (0 <= y && y < pipesGrid.length)
}

case class Pipe(directions: List[Direction]) {

  def isStart(): Boolean = directions.length == 4

  def findDirectionsFromNeighbours(coord: Coordinate, pipesGrid: Array[Array[Pipe]]) =
    Direction.allDirections.
      filter((d: Direction) => {
        val neighbourCoord = coord.move(d)

        if (!neighbourCoord.isInGrid(pipesGrid))
          false
        else
          neighbourCoord.getPipe(pipesGrid).facesDirection(d.opposite())
      })

  def facesDirection(direction: Direction): Boolean = directions.contains(direction)

  def exitDirection(entryDirection: Direction): Direction = directions.filter(d => d != entryDirection).head
}

object PipeMaze {

  def furthestPoint(filename: String): Long = {
    val lines = extractLines(filename)

    val listsOfPipes = lines.map(line => parseLine(line))
    val width = listsOfPipes.head.length
    val height = listsOfPipes.length

    val pipesGrid: Array[Array[Pipe]] = Array.tabulate[Pipe](height, width) { (x, y) => listsOfPipes(x)(y) }

    val startCoord = startCoords(pipesGrid)
    val startPipe = startCoord.getPipe(pipesGrid)
    val startExitDirections = startPipe.findDirectionsFromNeighbours(startCoord, pipesGrid)
    val startDirection = startExitDirections.head

    loopLength(startCoord, startDirection, pipesGrid, 0) / 2L
  }

  def parseLine(line: String): List[Pipe] = line.split("").map {
    case "|" => Pipe(List(North, South))
    case "-" => Pipe(List(East, West))
    case "L" => Pipe(List(North, East))
    case "J" => Pipe(List(North, West))
    case "7" => Pipe(List(West, South))
    case "F" => Pipe(List(East, South))
    case "." => Pipe(List())
    case "S" => Pipe(List(North, South, East, West))
  }.toList

  private def loopLength(currentCoord: Coordinate, movingInDirection: Direction, pipesGrid: Array[Array[Pipe]], distance: Long): Long = {
    val nextCoordinate = currentCoord.move(movingInDirection)
    val nextPipe = nextCoordinate.getPipe(pipesGrid)
    if (nextPipe.isStart()) {
      distance + 1
    }
    else {
      val nextPipeOnwardDirection = nextPipe.exitDirection(movingInDirection.opposite())
      loopLength(nextCoordinate, nextPipeOnwardDirection, pipesGrid, distance + 1)
    }
  }

  def startCoords(pipesGrid: Array[Array[Pipe]]): Coordinate = {
    val yCoord = pipesGrid.indexWhere((row: Array[Pipe]) => startXCoord(row) != -1)
    Coordinate(startXCoord(pipesGrid(yCoord)), yCoord)
  }

  private def startXCoord(row: Array[Pipe]) = {
    row.indexWhere(pipe => pipe.isStart())
  }

  private def extractLines(filename: String) = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()
    lines
  }
}