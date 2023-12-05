package aoc2023.day05

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
    assert(Mappings(Seq(Mapping(50, 98, 2), Mapping(52, 50, 48))).correspondingNumber(98) === 50)
  }

}
