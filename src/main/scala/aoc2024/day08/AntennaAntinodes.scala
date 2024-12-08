package aoc2024.day08

import utils.ParseFile.parseFile

class AntennaAntinodes {

  def numUniqueAntinodeLocations(file: String): Int = {
    val lines = parseFile(file)

    extractAntennaTypes(lines).
      flatMap(antennaType => antinodeLocations(antennaLocations(lines, antennaType))).
      count(antinodeLocation => antinodeLocation.within(lines.size, lines.head.size))
  }

  def numUniqueMultipleAntinodeLocations(file: String): Int = {
    val lines = parseFile(file)

    extractAntennaTypes(lines).
      flatMap(antennaType => multipleAntinodeLocations(antennaLocations(lines, antennaType), lines.size, lines.head.size)).
      size
  }

  private def extractAntennaTypes(lines: List[String]): Set[Char] =
    lines.flatMap(line => line.filter(c => """[A-ZAa-z0-9]""".r.matches(c.toString))).toSet

  private def antennaLocations(lines: List[String], antennaType: Char): Set[Location] = {
    lines.indices.
      flatMap(rowIndex =>
        lines(rowIndex).indices.
          filter(colIndex => lines(rowIndex)(colIndex) == antennaType).
          map(colIndex => Location(rowIndex, colIndex))).
      toSet
  }

  private def antinodeLocations(antennaLocations: Set[Location]):Set[Location] = {
    antennaLocations.
      flatMap(firstAntenna =>
        antennaLocations.
          filter(secondAntenna => secondAntenna != firstAntenna).
          flatMap(secondAntenna =>
              Set(firstAntenna.antinodeLocation(secondAntenna),
                  secondAntenna.antinodeLocation(firstAntenna))
          ))
  }

  private def multipleAntinodeLocations(antennaLocations: Set[Location], rowLimit: Int, colLimit: Int):Set[Location] = {
    antennaLocations.
      flatMap(firstAntenna =>
        antennaLocations.
          filter(secondAntenna => secondAntenna != firstAntenna).
          flatMap(secondAntenna =>
            firstAntenna.multipleAntinodeLocation(secondAntenna, rowLimit, colLimit) ++
            secondAntenna.multipleAntinodeLocation(firstAntenna, rowLimit, colLimit)
          ))
  }

  case class Location(rowIndex: Int, colIndex: Int) {

    def antinodeLocation(other: Location): Location =
      Location(other.rowIndex + (other.rowIndex - rowIndex),
               other.colIndex + (other.colIndex - colIndex))

    def multipleAntinodeLocation(other: Location, rowLimit: Int, colLimit: Int): Set[Location] = {
      if (!within(rowLimit, colLimit))
        Set()
      else {
        val nextLocation = Location(other.rowIndex + (other.rowIndex - rowIndex),
                                    other.colIndex + (other.colIndex - colIndex))
        Set(this) ++ other.multipleAntinodeLocation(nextLocation, rowLimit, colLimit)
      }
    }

    def within(rowLimit: Int, colLimit: Int): Boolean =
      (0 until rowLimit).contains(rowIndex) &&
      (0 until colLimit).contains(colIndex)

  }

}
