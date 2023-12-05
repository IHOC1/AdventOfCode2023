package aoc2023.day05

class Seeds {

}

case class Mappings(mappings: Seq[Mapping]) {

  def correspondingNumber(num: Int): Int =
    mappings.
      find(_.isInRange(num)).
      map(_.correspondingNumber(num)).
      getOrElse(num)

}

case class Mapping(destinationRangeStart: Int,
                   sourceRangeStart: Int,
                   rangeLength: Int) {

  def isInRange(num: Int): Boolean = Range(sourceRangeStart, sourceRangeStart + rangeLength).contains(num)

  def correspondingNumber(sourceNumber: Int): Int = destinationRangeStart + (sourceNumber - sourceRangeStart)

}