package aoc2024.day01

import utils.ParseFile._

class IdLists {

  def totalDistanceBetweenLists(file: String): Int = {
    val lines = parseFile(file)
    val idPairs = lines.map(line => line.split(" +"))
    val firstIds  = idPairs.map(p => p(0).toInt).sorted
    val secondIds = idPairs.map(p => p(1).toInt).sorted
    firstIds.zip(secondIds).map{ case (id1, id2) => math.abs(id1 - id2) }.sum
  }

  def similarityScore(file: String): Int = {
    val lines = parseFile(file)
    val idPairs = lines.map(line => line.split(" +"))
    val firstIds  = idPairs.map(p => p(0).toInt).sorted
    val secondIds = idPairs.map(p => p(1).toInt).sorted

    val id2ToCount = secondIds.groupBy(id => id).map { case (id, ids) => (id, ids.size) }.withDefaultValue(0)

    firstIds.map(id1 => id1 * id2ToCount(id1)).sum
  }

}
