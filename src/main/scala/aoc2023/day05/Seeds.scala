package aoc2023.day05

class Seeds {

}

object Seeds {

  def parseAlmanac(lines: Seq[String]): Almanac = {
    Almanac(parseSeeds(lines.head),
            parseMappings(lines.tail.tail))
  }

  def parseSeeds(line: String): Seq[Int] = line.split(": +")(1).split(" ").map(_.toInt).toSeq

  def parseMappings(lines: Seq[String]): Option[Mappings] = {
    if (lines.isEmpty)
      None
    else {
      val mappingsLines = lines.takeWhile(_.nonEmpty)
      val mappingsName = mappingsLines.head.split(" map\\:")(0)
      val mappings = mappingsLines.tail.
        map(line => line.split(" ").map(_.toInt)).
        map(nums => Mapping(nums(0), nums(1), nums(2)))
      Some(Mappings(mappingsName, mappings, parseMappings(lines.drop(mappingsLines.size + 1))))
    }
  }
}

case class Almanac(seeds: Seq[Int], mappings: Option[Mappings]) {

  def transformedSeeds(): Seq[Int] = seeds.map((seed: Int) => mappings.get.transform(seed))
}

case class Mappings(name: String = "",
                    mappings: Seq[Mapping] = Seq(),
                    nextMappings: Option[Mappings] = None) {

  def transform(num: Int): Int = nextMappings match {
    case None               => correspondingNumber(num)
    case Some(nextMappings) => nextMappings.transform(correspondingNumber(num))
  }

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