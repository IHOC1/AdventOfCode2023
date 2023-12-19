package aoc2023.day11

import scala.annotation.tailrec
import scala.math.abs

case class CosmicExpansion(galaxies: Seq[Galaxy]) {

  def sumOfShortestDistancesBetweenGalaxies(): Long = {
    val galaxyPairs = pairs(expand(galaxies), Seq())
    galaxyPairs.map(galaxyPair => shortestDistance(galaxyPair._1, galaxyPair._2)).sum
  }

  private def shortestDistance(galaxy1: Galaxy, galaxy2: Galaxy) =
    abs(galaxy1.x - galaxy2.x) +
    abs(galaxy1.y - galaxy2.y)

  @tailrec
  final def pairs(galaxies: Seq[Galaxy], galaxyPairings: Seq[(Galaxy, Galaxy)]): Seq[(Galaxy, Galaxy)] = {
    if (galaxies.isEmpty)
      galaxyPairings
    else {
      val headPairedWithRest = galaxies.tail.map((galaxies.head, _))
      pairs(galaxies.tail, galaxyPairings ++ headPairedWithRest)
    }
  }

  def expand(galaxies: Seq[Galaxy]): Seq[Galaxy] = {
    expandCols(expandRows(galaxies))
  }

  private def expandRows(galaxies: Seq[Galaxy]) = blankRows(galaxies).reverse.foldLeft(galaxies)((gals, row) => gals.map(gal => if (gal.y > row) gal.copy(y = gal.y + 1) else gal))
  private def expandCols(galaxies: Seq[Galaxy]) = blankCols(galaxies).reverse.foldLeft(galaxies)((gals, col) => gals.map(gal => if (gal.x > col) gal.copy(x = gal.x + 1) else gal))

  private def blankRows(galaxies: Seq[Galaxy]): Seq[Long] = (0L until maxRow(galaxies)).filter((row: Long) => !galaxies.exists(_.y == row))
  private def blankCols(galaxies: Seq[Galaxy]): Seq[Long] = (0L until maxCol(galaxies)).filter((col: Long) => !galaxies.exists(_.x == col))

  private def maxRow(galaxies: Seq[Galaxy]): Long = galaxies.map(_.y).max
  private def maxCol(galaxies: Seq[Galaxy]): Long = galaxies.map(_.x).max

}

object CosmicExpansion {

  def parseImage(filename: String): CosmicExpansion = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()
    val galaxies: Seq[Galaxy] = lines.zipWithIndex.flatMap(lineAndYIndex => {
      val line = lineAndYIndex._1
      val y    = lineAndYIndex._2
      line.zipWithIndex.filter(p => p._1 == '#').map(p => Galaxy(p._2, y))
    })
    CosmicExpansion(galaxies)
  }

}

case class Galaxy(x: Long, y: Long)
