package aoc2023.day05

class Seeds {

}

object Seeds {

  def parseAlmanac(lines: Seq[String]): Almanac = {
    Almanac(parseSeeds(lines.head),
            parseMappings(lines.tail.tail))
  }

  def parseSeeds(line: String): Seq[Long] = line.split(": +")(1).split(" ").map(_.toLong).toSeq

  def parseMappings(lines: Seq[String]): Option[Mappings] = {
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

  def transformedSeeds(): Seq[Long] = seeds.map((seed: Long) => mappings.get.transform(seed))

  def rangedSeeds(): Seq[Long] = seedsToRanges(seeds)

  def seedsToRanges(seeds: Seq[Long]): Seq[Long] = {
    if (seeds.isEmpty)
      Seq()
    else {
      val start = seeds.head
      val length = seeds.tail.head
      (start to (start + length)).exclusive ++ seedsToRanges(seeds.tail.tail)
    }
  }
}

case class Mappings(name: String = "",
                    mappings: Seq[Mapping] = Seq(),
                    nextMappings: Option[Mappings] = None) {

  def transform(num: Long): Long = nextMappings match {
    case None               => correspondingNumber(num)
    case Some(nextMappings) => nextMappings.transform(correspondingNumber(num))
  }

  def correspondingNumber(num: Long): Long =
    mappings.
      find(_.isInRange(num)).
      map(_.correspondingNumber(num)).
      getOrElse(num)

}

case class Mapping(destinationRangeStart: Long,
                   sourceRangeStart: Long,
                   rangeLength: Long) {

  def isInRange(num: Long): Boolean = (sourceRangeStart to sourceRangeStart + rangeLength).exclusive.contains(num)

  def correspondingNumber(sourceNumber: Long): Long = destinationRangeStart + (sourceNumber - sourceRangeStart)

}