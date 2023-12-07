package aoc2023.day05

class Seeds {

}

object Seeds {

  def parseAlmanac(lines: Seq[String]): Almanac = {
    Almanac(parseSeeds(lines.head),
            parseMappings(lines.tail.tail))
  }

  private def parseSeeds(line: String): Seq[Long] = line.split(": +")(1).split(" ").map(_.toLong).toSeq

  private def parseMappings(lines: Seq[String]): Option[Mappings] = {
    if (lines.isEmpty)
      None
    else {
      val mappingsLines = lines.takeWhile(_.nonEmpty)
      val mappingsName = mappingsLines.head.split(" map:")(0)
      val mappings = mappingsLines.tail.
        map(line => line.split(" ").map(_.toLong)).
        map(nums => Mapping(nums(0), nums(1), nums(2)))
      Some(Mappings(mappingsName, mappings, parseMappings(lines.drop(mappingsLines.size + 1))))
    }
  }

}

case class Almanac(seeds: Seq[Long], mappings: Option[Mappings]) {

  private val seedRanges = seedsToAlmanacRanges(seeds)

  // Part 1
  def lowestLocationNumberFromInitialSeeds(): Long = {
    LazyList.from(1).
      map(_.toLong).
      find((locationNum: Long) => {
        val seed = seedFor(locationNum)
        seeds.contains(seed)
      }).get
  }

  // Part 2
  def lowestLocationNumberFromInitialSeedRanges(): Long = {
    LazyList.from(1).
      find((locationNum: Int) => {
        val seed = seedFor(locationNum)
        seedRanges.exists(r => r.contains(seed))
      }).get
  }

  private def seedFor(num: Long) = mappings.get.recursivelyReverseMap(num)

  private def seedsToAlmanacRanges(seeds: Seq[Long]): Seq[AlmanacRange] = {
    if (seeds.isEmpty)
      Seq()
    else {
      val start = seeds.head
      val length = seeds.tail.head
      Seq(AlmanacRange(start, length)) ++ seedsToAlmanacRanges(seeds.tail.tail)
    }
  }

}

case class AlmanacRange(start: Long, length: Long) {
  def contains(num: Long): Boolean = start <= num && num <= start + length - 1
}

case class Mappings(name: String = "",
                    mappings: Seq[Mapping] = Seq(),
                    nextMappings: Option[Mappings] = None) {

  def recursivelyReverseMap(num: Long): Long =
    nextMappings match {
      case Some(nextMappings) => reverseMapNum(nextMappings.recursivelyReverseMap(num))
      case None => reverseMapNum(num)
    }

  def reverseMapNum(num: Long): Long =
    mappings.
      find(_.inDestinationRange(num)).
      map(_.destToSource(num)).
      getOrElse(num)

}

case class Mapping(destinationRangeStart: Long,
                   sourceRangeStart: Long,
                   rangeLength: Long) {

  private val destinationRange = AlmanacRange(destinationRangeStart, rangeLength)

  def inDestinationRange(num: Long): Boolean = destinationRange.contains(num)

  def destToSource(num: Long): Long = (num - destinationRangeStart) + sourceRangeStart

}
