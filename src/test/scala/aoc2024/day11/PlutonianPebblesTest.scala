package aoc2024.day11

import aoc2024.day11.day11.stringToPebbles
import org.scalatest.flatspec.AnyFlatSpec

class PlutonianPebblesTest extends AnyFlatSpec {

  "Simple start point" should "yield 55312 stones after 25 blinks" in {
    assert(new PlutonianPebbles().countStonesAfter(stringToPebbles("125 17"), 25) == 55312L)
  }

  "Full start point" should "yield 203457 stones after 25 blinks" in {
    assert(new PlutonianPebbles().countStonesAfter(stringToPebbles("1 24596 0 740994 60 803 8918 9405859"), 25) == 203457L)
  }

  "Full start point" should "yield 241394363462435 stones after 75 blinks" in {
    assert(new PlutonianPebbles().countStonesAfter(stringToPebbles("1 24596 0 740994 60 803 8918 9405859"), 75) == 241394363462435L)
  }
}
