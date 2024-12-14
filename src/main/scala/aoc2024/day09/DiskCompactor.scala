package aoc2024.day09

import utils.ParseFile.parseFile

class DiskCompactor {

  def compactedChecksumFromFile(file: String): Long = {
    val lines = parseFile(file)
    val discMap = lines.head
    compactedChecksum(discMap)
  }

  def compactedChecksum(discMap: String): Long = {
    val disc = discMap.zipWithIndex.
      map { case (c, index) => DiscFile(c.toString.toInt, fileIdOrFreeSpace(index)) }

    var compactedDisc = disc

    val fileIterator = disc.reverseIterator.filter(df => df.fileId != -1)
    while (fileIterator.hasNext) {
      val fileToMove = fileIterator.next()

      val indexOfFileToMove = compactedDisc.zipWithIndex.
        find { case (df, _) => df == fileToMove }.
        map { case (_, index) => index }.
        get

      val spaceForFile: Option[(DiscFile, Int)] = compactedDisc.zipWithIndex.
        filter { case (df, _) => df.fileId == -1 }.
        find { case (freeSpace, freeSpaceIndex) => freeSpace.size >= fileToMove.size &&
                                                   freeSpaceIndex < indexOfFileToMove
        }

      spaceForFile.foreach { case (freeSpace, freeSpaceLocation) =>

        // Move file to space
        val (beforeFree, freeAndAfter) = compactedDisc.splitAt(freeSpaceLocation)
        val replacement =
          if (freeSpace.size == fileToMove.size)
            Seq(fileToMove)
          else
            Seq(fileToMove, DiscFile(freeSpace.size - fileToMove.size, -1))
        compactedDisc = beforeFree ++ replacement ++ freeAndAfter.tail

        // Replace original file location with free space
        val (beforeFile, fileAndAfter) = compactedDisc.splitAt(indexOfFileToMove + replacement.size - 1)
        compactedDisc = beforeFile ++ Seq(DiscFile(fileToMove.size, -1)) ++ fileAndAfter.tail
      }
    }

    checksum(compactedDisc)
  }

  def checksum(compactedDisc: IndexedSeq[DiscFile]): Long = {
    compactedDisc.iterator.
      flatMap(df => (0 until df.size).map(_ => df.fileId)).
      zipWithIndex.
      map { case (fileId, index) => if (fileId == -1) 0 else fileId.toLong * index.toLong }.
      sum
  }

  private def fileIdOrFreeSpace(index: Int): Int =
    if (index % 2 == 0)
      index / 2
    else
      -1

}

case class DiscFile(size: Int, fileId: Int)
