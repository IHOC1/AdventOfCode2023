package aoc2023.day02

case class Game(red: Int, green: Int, blue: Int) {

  def isPossible(limit: Game): Boolean =
      red   < limit.red   &&
      green < limit.green &&
      blue  < limit.blue
}

object Game {

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
