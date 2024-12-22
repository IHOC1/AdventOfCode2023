package aoc2024.day12

import org.scalatest.flatspec.AnyFlatSpec

class GardenTest extends AnyFlatSpec {

  "Simple Garden" should "finds area A" in {
    val garden = new Garden("aoc2024/day12/SimpleGarden.txt")
    assert(garden.findRegion(Set(Point(0, 0)), Region(Set(Point(0, 0))), garden.allLocations) == Region(Set(Point(0,0), Point(0,1), Point(0, 2), Point(0, 3))))
    assert(garden.findRegion(Set(Point(1, 0)), Region(Set(Point(1, 0))), garden.allLocations) == Region(Set(Point(1,0), Point(1,1), Point(2, 0), Point(2, 1))))
  }

  "Test Garden" should "cost 1930 to fence" in {
    assert(new Garden("aoc2024/day12/TestGarden.txt").fencePrice() == 1930)
  }

  "OXO Garden" should "cost 772 to fence" in {
    assert(new Garden("aoc2024/day12/OXOGarden.txt").fencePrice() == 772)
  }

  "Full Garden" should "cost 1464678 to fence" in {
    assert(new Garden("aoc2024/day12/FullGarden.txt").fencePrice() == 1464678)
  }

  // Part 2

  "Simple Garden" should "find 4 sides for area A" in {
    val garden = new Garden("aoc2024/day12/SimpleGarden.txt")
    assert(garden.findRegion(Set(Point(0, 0)), Region(Set(Point(0, 0))), garden.allLocations).sides.size == 4)
  }

  "Simple Garden" should "cost 80 to fence with bulk discount" in {
    assert(new Garden("aoc2024/day12/SimpleGarden.txt").fencePriceBulkDiscount() == 80)
  }

  "Test Garden" should "cost 1206 to fence with bulk discount" in {
    assert(new Garden("aoc2024/day12/TestGarden.txt").fencePriceBulkDiscount() == 1206)
  }

  "Full Garden" should "cost 877492 to fence with bulk discount" in {
    assert(new Garden("aoc2024/day12/FullGarden.txt").fencePriceBulkDiscount() == 877492)
  }

}
