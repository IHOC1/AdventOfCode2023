package aoc2023.day06

case class BoatRace(duration: Int, record: Long) {

  def winningButtonPresses(): Long = (0 until duration).map(buttonHeld => Boat(duration).distance(buttonHeld)).filter(distance => distance > record).size

}

case class Boat(raceLasts: Long) {

  def distance(buttonHeld: Long): Long = buttonHeld * (raceLasts - buttonHeld)

}
