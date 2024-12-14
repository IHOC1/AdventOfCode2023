package aoc2024.day09

import org.scalatest.flatspec.AnyFlatSpec

class DiskCompactorTest extends AnyFlatSpec {

  "Simple disc map" should "have compacted checksum of 2858" in {
    assert(new DiskCompactor().compactedChecksumFromFile("aoc2024/day09/SimpleDiscMap.txt") == 2858L)
  }

  "Full disc map" should "have compacted checksum of 6408966547049" in {
    assert(new DiskCompactor().compactedChecksumFromFile("aoc2024/day09/FullDiscMap.txt") == 6408966547049L)
  }

}

