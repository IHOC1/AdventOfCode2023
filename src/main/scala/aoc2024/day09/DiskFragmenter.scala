package aoc2024.day09

import utils.ParseFile.parseFile

class DiskFragmenter {

  def defragmentedChecksum(file: String): Long = {
    val lines = parseFile(file)
    val discMap = lines.head

//    println(discMap)

    defragmentedChecksum1(discMap)
  }


  def defragmentedChecksum1(discMap: String): Long = {
    val fwdIt = forwardIterator(discMap)
    val revIt = reverseIterator(discMap)

    var ended = false
    var checksum = 0L
    var revIndex = Int.MaxValue

    while (fwdIt.hasNext && revIt.hasNext && !ended) {
      val fwd = fwdIt.next()
      val fileIdAndIndex = if (fwd._1 == -1) {
        val rev = revIt.next()
        revIndex = rev._2
        (rev._1, fwd._2)
      }
      else
        fwd

      if (fwd._2 >= revIndex)
        ended = true
      else
        checksum += fileIdAndIndex._1 * fileIdAndIndex._2
    }
    checksum
  }

  def fullLength(discMap: String): Int =
    discMap.iterator.map(_.toString.toInt).sum

  def forwardIterator(diskMap: String): Iterator[(Int, Int)] = {
    diskMap.iterator.
      zipWithIndex.
      flatMap { case(c, i) =>
        (0 until c.toString.toInt).
          map(_ => idNumberOrFree(i))
      }
      .zipWithIndex
  }

  def reverseIterator(diskMap: String): Iterator[(Int, Int)] = {
    def fullDiscLength: Int = fullLength(diskMap)
    diskMap.
      reverseIterator.
      zipWithIndex.
      flatMap { case(c, i) =>
        (0 until c.toString.toInt).
          map(_ => idNumberOrFree(diskMap.length - i - 1))
      }.
      zipWithIndex.
      map {case (c, i) => (c, fullDiscLength - i - 1)}.
      filter(_._1 != -1)
  }

  private def idNumberOrFree(diskMapIndex: Int): Int =
    if (diskMapIndex % 2 == 0) diskMapIndex / 2 else -1

}




