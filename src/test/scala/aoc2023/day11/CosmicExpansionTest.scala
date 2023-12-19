package aoc2023.day11

import aoc2023.day11.CosmicExpansion.parseImage
import org.scalatest.flatspec.AnyFlatSpec

class CosmicExpansionTest extends AnyFlatSpec {

  "Test image" should "have sum of shortest distances between galaxies is 374" in {
    assert(parseImage("Day11CosmicExpansionTest1.txt", 2L).sumOfShortestDistancesBetweenGalaxies() == 374L)
  }

  "Image" should "have sum of shortest distances between galaxies is 10313550" in {
    assert(parseImage("Day11CosmicExpansion.txt", 2L).sumOfShortestDistancesBetweenGalaxies() == 10313550L)
  }

  // Part 2

  "Test image with expansion factor of 10" should "have sum of shortest distances between galaxies is 1030" in {
    assert(parseImage("Day11CosmicExpansionTest1.txt", 10L).sumOfShortestDistancesBetweenGalaxies() == 1030L)
  }

  "Image with expansion factor of 1M" should "have sum of shortest distances between galaxies is 611998089572" in {
    assert(parseImage("Day11CosmicExpansion.txt", 1000000L).sumOfShortestDistancesBetweenGalaxies() == 611998089572L)
  }

}
