package aoc2023.day05

class Seeds {

}

case class Mapping(destinationRangeStart: Int,
                   sourceRangeStart: Int,
                   rangeLength: Int) {

  def isInRange(num: Int) = Range(sourceRangeStart, sourceRangeStart + rangeLength).contains(num)

  def correspondingNumber(sourceNumber: Int): Int = destinationRangeStart + (sourceNumber - sourceRangeStart)

}