package aoc2024.day03

import org.scalatest.flatspec.AnyFlatSpec

class CorruptedMemoryTest extends AnyFlatSpec {

  "Example data" should "calculate as 161" in {
    assert(new CorruptedMemory().calculate("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))") == 161)
  }

  "Example data file" should "calculate as 161" in {
    assert(new CorruptedMemory().calculateFromFile("aoc2024/day03/ExampleMemory.txt") == 161)
  }

  "Full data file" should "calculate as 173529487" in {
    assert(new CorruptedMemory().calculateFromFile("aoc2024/day03/FullMemory.txt") == 173529487)
  }

  "Example data file" should "calculate with do and don't as 48" in {
    assert(new CorruptedMemory().calculateFromFileDoDont("aoc2024/day03/ExampleMemory2.txt") == 48)
  }

  "Full data file" should "calculate with do and don't as 99532691" in {
    assert(new CorruptedMemory().calculateFromFileDoDont("aoc2024/day03/FullMemory.txt") == 99532691)
  }

}
