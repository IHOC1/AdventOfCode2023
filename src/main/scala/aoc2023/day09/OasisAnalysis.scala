package aoc2023.day09

import scala.annotation.tailrec

class OasisAnalysis {

}

object OasisAnalysis {

  def sumOfExtrapolatedSensorValues(filename: String) = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()

    lines.
      map(line => lineToValues(line)).
      map(values => extrapolate(values)).
      sum
  }

  private def lineToValues(line: String) = {
    line.split(" ").map(_.toLong).toList
  }

  @tailrec
  private def differences(values: List[Long], diffs: List[Long]): List[Long] =
    if (values.length < 2)
      diffs
    else
      differences(values.tail, diffs :+ (values.tail.head - values.head))

  def extrapolate(values: List[Long]): Long = {
    if (values.forall(_ == 0L))
      values.last
    else {
      values.last + extrapolate(differences(values, List()))
    }
  }

}
