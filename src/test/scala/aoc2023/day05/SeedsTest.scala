package aoc2023.day05

import org.scalatest.flatspec.AnyFlatSpec

class SeedsTest extends AnyFlatSpec {

  "A mapping" should "transform all values in range" in {
    assert(Mapping(50, 98, 2).correspondingNumber(98) === 50)
  }

}
