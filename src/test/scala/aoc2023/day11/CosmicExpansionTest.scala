package aoc2023.day11

import aoc2023.day11.CosmicExpansion.parseImage
import org.scalatest.flatspec.AnyFlatSpec

class CosmicExpansionTest extends AnyFlatSpec {

  "Test image" should "have sum of shortest distances between galaxies is 374" in {
    assert(parseImage("Day11CosmicExpansionTest1.txt").sumOfShortestDistancesBetweenGalaxies() == 374L)
  }

  "Image" should "have sum of shortest distances between galaxies is 10313550" in {
    assert(parseImage("Day11CosmicExpansion.txt").sumOfShortestDistancesBetweenGalaxies() == 10313550L)
  }
}
