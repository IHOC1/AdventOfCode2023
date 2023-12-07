package aoc2023.day06

case class BoatRace(duration: Int, record: Int) {
  def winningButtonPresses(): Int = (0 until duration).map(buttonHeld => Boat(duration).distance(buttonHeld)).filter(distance => distance > record).size

}

case class Boat(raceLasts: Int) {

  def distance(buttonHeld: Int): Int = buttonHeld * (raceLasts - buttonHeld)

}
