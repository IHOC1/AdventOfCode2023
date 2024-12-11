package aoc2024.day09

import org.scalatest.flatspec.AnyFlatSpec

class DiskFragmenterTest extends AnyFlatSpec {

  "12345" should "have defragmented checksum of 60" in {
    // 022111222
    assert(new DiskFragmenter().defragmentedChecksum1("12345") == 60)
  }

  "Simple Disc Map" should "have defragmented checksum of 1928" in {
    assert(new DiskFragmenter().defragmentedChecksum("aoc2024/day09/SimpleDiscMap.txt") == 1928)
  }

  "Full Disc Map" should "have defragmented checksum of 6384282079460" in {
    assert(new DiskFragmenter().defragmentedChecksum("aoc2024/day09/FullDiscMap.txt") == 6384282079460L)
  }

  "Checksum for 022111222" should "be 60" in {
    assert("022111222".iterator.map(_.toString.toLong).zipWithIndex.map{ case (fileId, index) => fileId * index}.sum == 60)
  }

  "Checksum for 0099811188827773336446555566" should "be 1928" in {
    assert("0099811188827773336446555566".iterator.map(_.toString.toLong).zipWithIndex.map{ case (fileId, index) => fileId * index}.sum == 1928)
  }

  "Full Length" should "be 15 for 12345" in {
    assert(new DiskFragmenter().fullLength("12345") == 15)
  }

  "ForwardIterator1" should "turn 12345 into 0..111....22222" in {
    assert(iteratorToString(new DiskFragmenter().forwardIterator("12345")) == "0..111....22222")
    assert(new DiskFragmenter().forwardIterator("12345").toSeq.head == (0, 0))
    assert(new DiskFragmenter().forwardIterator("12345").toSeq.last == (2, 14))
  }

  "ReverseIterator1" should "turn 12345 into 0..111....22222" in {
    assert(iteratorToString(new DiskFragmenter().reverseIterator("12345")) == "222221110")
    assert(new DiskFragmenter().reverseIterator("12345").toSeq.head == (2, 14))
    assert(new DiskFragmenter().reverseIterator("12345").toSeq.last == (0, 0))
  }

  private def iteratorToString(it: Iterator[(Int, Int)]) =
    it.map(_._1).foldLeft("")((str: String, fileIdOrFree: Int) => str + (if (fileIdOrFree == -1) "." else fileIdOrFree.toString))
}
