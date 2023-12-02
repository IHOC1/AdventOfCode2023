package aoc2023.day02

import scala.math._

case class Game(red: Int, green: Int, blue: Int) {

  def isPossible(limit: Game): Boolean =
      red   <= limit.red   &&
      green <= limit.green &&
      blue  <= limit.blue

  def merge(game: Game): Game =
    new Game(
      max(game.red,   red),
      max(game.green, green),
      max(game.blue,  blue))

  def power(): Int = red * green * blue
}

object Game {

  def parseLine(line: String): (Int, List[Game]) = {
    val parts = line.split(": ")
    val gameNumber = parts(0).split(" ")(1).toInt
    gameNumber -> parts(1).split("; ").map((game: String) => parseGame(game)).toList
  }

  def parseGame(str: String): Game = {
    str.split(",").
      map(p => p.trim).
      map(p => p.split(" ")).
      foldLeft(new Game(0, 0, 0))((g, c) => c(1) match {
        case "red"   => g.copy(red   = colourValue(c))
        case "green" => g.copy(green = colourValue(c))
        case "blue"  => g.copy(blue  = colourValue(c))
        case _       => g
      })
  }

  private def colourValue(c: Array[String]) = c(0).toInt
}
