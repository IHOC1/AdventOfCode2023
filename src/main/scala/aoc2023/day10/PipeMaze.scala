package aoc2023.day10

import aoc2023.day10.Direction.{East, North, South, West}

import scala.annotation.tailrec

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

  def isInside(loop: Set[Coordinate], pipesGrid: Array[Array[Pipe]]): Boolean = {
    (0 to y).count(y1 => {
      val checkCoord = Coordinate(x, y1)
      loop.contains(checkCoord) && checkCoord.getPipe(pipesGrid).isEastWest
    }) % 2 ==1
  }

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

  private val eastWest: Set[Direction] = Set(East, West)

  def isEastWest: Boolean = directions.exists(d => eastWest.contains(d))
}

object PipeMaze {

  def furthestPoint(filename: String): Long = {
    val pipesGrid = parseGrid(filename)

    loopFromStart(pipesGrid).length / 2L
  }


  def numInternalTiles(filename: String): Long = {
    val pipesGrid = parseGrid(filename)
    val loop = loopFromStart(pipesGrid).toSet

    val coords: Set[Coordinate] = allCoords(pipesGrid)
    val nonLoopCoords = coords.diff(loop)

    nonLoopCoords.count(coord => coord.isInside(loop, pipesGrid))
  }

  private def allCoords(pipesGrid: Array[Array[Pipe]]) = {
    pipesGrid.indices.
      flatMap(y => pipesGrid.head.indices.
        map(x => Coordinate(x, y))).toSet
  }

  private def loopFromStart(pipesGrid: Array[Array[Pipe]]) = {
    val startCoord = startCoords(pipesGrid)
    val startPipe = startCoord.getPipe(pipesGrid)
    val startExitDirections = startPipe.findDirectionsFromNeighbours(startCoord, pipesGrid)
    val startDirection = startExitDirections.head

    loop(startCoord, startDirection, pipesGrid, List(startCoord))
  }

  private def parseGrid(filename: String): Array[Array[Pipe]] = {
    val lines = extractLines(filename)

    val listsOfPipes = lines.map(line => parseLine(line))
    val width = listsOfPipes.head.length
    val height = listsOfPipes.length

    Array.tabulate[Pipe](height, width) { (x, y) => listsOfPipes(x)(y) }
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

  @tailrec
  private def loop(currentCoord: Coordinate, movingInDirection: Direction, pipesGrid: Array[Array[Pipe]], loopCoords: Seq[Coordinate]): Seq[Coordinate] = {
    val nextCoordinate = currentCoord.move(movingInDirection)
    val nextPipe = nextCoordinate.getPipe(pipesGrid)
    if (nextPipe.isStart()) {
      loopCoords
    }
    else {
      val nextPipeOnwardDirection = nextPipe.exitDirection(movingInDirection.opposite())
      loop(nextCoordinate, nextPipeOnwardDirection, pipesGrid, loopCoords :+ nextCoordinate)
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