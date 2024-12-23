package aoc2024.day14

import aoc2024.day14.BathroomPatrolSpace.Robot
import utils.ParseFile.parseFile

import scala.annotation.tailrec

class BathroomPatrolSpace(val width: Int, val height: Int) {

  def safetyFactor(file: String): Int = {
    parseRobots(file).
      map(_.positionAfter(100)).
      filter(!_.betweenQuadrants).
      groupBy(_.quadrant).
      map{ case (_, robots) => robots.size}.
      product
  }

  def numMovesUntilTreeFound(file: String): Int =
    findNumMovesUntilTreeFound(parseRobots(file), 0)

  @tailrec
  private def findNumMovesUntilTreeFound(robots: Seq[Robot], seconds: Int): Int = {
    if (treeFound(robots)) {
      show(robots)
      seconds
    }
    else
      findNumMovesUntilTreeFound(robots.map(_.positionAfter(1)), seconds + 1)
  }

  def treeFound(robots: Seq[Robot]): Boolean =
    robots.
      groupBy(robot => robot.posY).values.
      map (robotsInRow => consecutiveRobotCount(robotsInRow)).
      max > 20

  private def consecutiveRobotCount(robotsInRow: Seq[Robot]) = {
    val sortedRobots = robotsInRow.sortBy(robot => robot.posX)
    sortedRobots.zip(sortedRobots.tail).
      count { case (r1, r2) => r1.posX + 1 == r2.posX }
  }

  private def show(robots: Seq[Robot]): Unit = {
    val lobbyMap = robots.
      groupBy(robot => (robot.posX, robot.posY)).
      map { case (pos, robots) => (pos, robots.size.toString) }.
      withDefaultValue(".")

    (0 until height).foreach(row => {
      (0 until width).foreach(col =>
        print(lobbyMap(col, row))
      )
      println("")
    })
    println("")
  }

  private def parseRobots(file: String): Seq[Robot] = {
    parseFile(file).map(line => {
      val positionAndVelocity = line.split(" ")
      val position = positionAndVelocity(0).drop(2).split(",").map(_.toInt)
      val velocity = positionAndVelocity(1).drop(2).split(",").map(_.toInt)
      Robot(position(0), position(1),
        (velocity(0) + width) % width, (velocity(1) + height) % height, // correct for -ve velocities
        width, height)
    })
  }

}

object BathroomPatrolSpace {

  case class Robot(posX: Int, posY: Int,
                   velX: Int, velY: Int,
                   width: Int, height: Int) {

    def positionAfter(seconds: Int): Robot =
      Robot((posX + (seconds * velX)) % width,
        (posY + (seconds * velY)) % height,
        velX, velY, width, height)

    def betweenQuadrants: Boolean = atMiddleXPosition || atMiddleYPosition

    private def atMiddleXPosition: Boolean = posX == width  / 2
    private def atMiddleYPosition: Boolean = posY == height / 2

    def quadrant: Int =
      if (posX < width / 2)
        if (posY < height / 2) 0 else 1
      else
        if (posY < height / 2) 2 else 3

  }

}