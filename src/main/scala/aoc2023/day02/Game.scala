package aoc2023.day02

case class Game(red: Int, green: Int, blue: Int) {

  def isPossible(limit: Game): Boolean =
      red   < limit.red   &&
      green < limit.green &&
      blue  < limit.blue

}
