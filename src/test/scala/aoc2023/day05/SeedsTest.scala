package aoc2023.day05

import aoc2023.day05.Seeds.parseAlmanac
import org.scalatest.flatspec.AnyFlatSpec

class SeedsTest extends AnyFlatSpec {

  "A mapping" should "transform all values in range" in {
    assert(Mapping(50, 98, 2).correspondingNumber(98) === 50)
    assert(Mapping(50, 98, 2).correspondingNumber(99) === 51)
  }

  "A mapping" should "indicated values in range" in {
    assert(Mapping(50, 98, 2).isInRange(97) === false)
    assert(Mapping(50, 98, 2).isInRange(98) === true)
    assert(Mapping(50, 98, 2).isInRange(99) === true)
    assert(Mapping(50, 98, 2).isInRange(100) === false)
  }

  "Many mappings" should "transform all values in range" in {
    assert(Mappings("", Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).correspondingNumber(98) === 50)
  }

  "Part 1 test almanac" should "transform seed values correctly" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05AlmanacExample.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    assert(almanac.transformedSeeds() === Seq(82, 43, 86, 35))

    source.close()
  }

  "Part 1 almanac" should "transform seed values correctly" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05Almanac.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    val transformedSeeds = almanac.transformedSeeds()

    println(transformedSeeds.min)

    assert(transformedSeeds.min === 107430936)

    source.close()
  }

  // Part 2

  "Part 2 test almanac" should "should generate the correct seed ranges" in {
    val almanac = Almanac(Seq(2, 3, 6, 2), None)
    assert(almanac.rangedSeeds() === Seq(2 to 4, 6 to 7))
  }

  "Part 2 test almanac" should "give correct min value" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05AlmanacExample.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    assert(almanac.transformedSeedRanges().min === 46)
    assert(almanac.minTransformedSeedValue() === 46)

    source.close()
  }

  "Part 2 almanac" should "give correct min value" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day05Almanac.txt")
    val almanac: Almanac = parseAlmanac(source.getLines().toSeq)

    val min = almanac.minTransformedSeedValue()
    println(min)
    assert(min === 46)

    source.close()
  }

}
