package aoc2024.day08

import org.scalatest.flatspec.AnyFlatSpec

class AntennaAntinodesTest extends AnyFlatSpec {

  "Example antenna locations 1" should "have 2 distinct anti-nodes" in {
    assert(new AntennaAntinodes().numUniqueAntinodeLocations("aoc2024/day08/ExampleAntennaLocations1.txt") == 2)
  }

  "Simple antenna locations" should "have 14 distinct anti-nodes" in {
    assert(new AntennaAntinodes().numUniqueAntinodeLocations("aoc2024/day08/SimpleAntennaLocations.txt") == 14)
  }

  "Full antenna locations" should "have 254 distinct anti-nodes" in {
    assert(new AntennaAntinodes().numUniqueAntinodeLocations("aoc2024/day08/FullAntennaLocations.txt") == 254)
  }

  "Simple antenna locations" should "have 34 distinct multiple anti-nodes" in {
    assert(new AntennaAntinodes().numUniqueMultipleAntinodeLocations("aoc2024/day08/SimpleAntennaLocations.txt") == 34)
  }

  "Full antenna locations" should "have 951 distinct anti-nodes" in {
    assert(new AntennaAntinodes().numUniqueMultipleAntinodeLocations("aoc2024/day08/FullAntennaLocations.txt") == 951)
  }

}
