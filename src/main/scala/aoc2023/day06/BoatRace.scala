package aoc2023.day06

class BoatRace {

}

case class Boat(raceLasts: Int) {

  def distance(buttonHeld: Int): Int = buttonHeld * (raceLasts - buttonHeld)

}
