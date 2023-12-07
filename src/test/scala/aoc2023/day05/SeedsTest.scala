package aoc2023.day05

import aoc2023.day05.Seeds.parseAlmanac
import org.scalatest.flatspec.AnyFlatSpec

import scala.Console.flush

class SeedsTest extends AnyFlatSpec {

  "Many mappings" should "reverse map all values in range" in {
    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).reverseMapNum(49) === 49)
    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).reverseMapNum(52) === 50)
    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).reverseMapNum(99) === 97)

    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).reverseMapNum(50) === 98)
    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).reverseMapNum(51) === 99)
    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).reverseMapNum(100) === 100)
  }

  "Part 1 almanac" should "transform seed values correctly" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05Almanac.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    val transformedSeeds = almanac.lowestLocationNumberFromInitialSeeds()

    println(transformedSeeds)

    assert(transformedSeeds === 107430936)

    source.close()
  }

  // Part 2

  "Part 2 test almanac" should "give correct min value" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05AlmanacExample.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    assert(almanac.lowestLocationNumberFromInitialSeedRanges() === 46)

    source.close()
    flush()
  }

  "Part 2 almanac" should "give correct min value" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05Almanac.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    val min = almanac.lowestLocationNumberFromInitialSeedRanges()
    println(min)
    assert(min === 23738616)

    source.close()
  }

  "A Mapping" should "should reverse map a value it contains" in {
    assert(Mapping(50, 98, 4).destToSource(50) === 98)
    assert(Mapping(50, 98, 4).destToSource(51) === 99)
    assert(Mapping(50, 98, 4).destToSource(52) === 100)
    assert(Mapping(50, 98, 4).destToSource(53) === 101)
  }

}
