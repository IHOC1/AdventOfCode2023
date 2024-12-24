package aoc2024.day15

import org.scalatest.flatspec.AnyFlatSpec

class WarehouseTest extends AnyFlatSpec {

  "Simplest Test Warehouse" should "sum final box GPS coordinates to 2028" in {
    assert(new Warehouse().sumOfFinalBoxGPSCoordinates("aoc2024/day15/SimpleTestWarehouse.txt") == 2028)
  }

  "Test Warehouse" should "sum final box GPS coordinates to 10092" in {
    assert(new Warehouse().sumOfFinalBoxGPSCoordinates("aoc2024/day15/TestWarehouse.txt") == 10092)
  }

  "Full Warehouse" should "sum final box GPS coordinates to 1511865" in {
    assert(new Warehouse().sumOfFinalBoxGPSCoordinates("aoc2024/day15/FullWarehouse.txt") == 1511865)
  }

  // Part 2
  "Simplest Test Wide Warehouse" should "sum final box GPS coordinates to 618" in {
    assert(new WideWarehouse().sumOfFinalBoxGPSCoordinates("aoc2024/day15/SimpleTestWideWarehouse.txt") == 618)
  }

  "Test Wide Warehouse" should "sum final box GPS coordinates to 9021" in {
    assert(new WideWarehouse().sumOfFinalBoxGPSCoordinates("aoc2024/day15/TestWarehouse.txt") == 9021)
  }

  "Full Wide Warehouse" should "sum final box GPS coordinates to 1519991" in {
    assert(new WideWarehouse().sumOfFinalBoxGPSCoordinates("aoc2024/day15/FullWarehouse.txt") == 1519991)
  }

}
