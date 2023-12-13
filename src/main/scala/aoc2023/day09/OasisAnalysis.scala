package aoc2023.day09

import scala.Predef
import scala.annotation.tailrec
import scala.io.Source

class OasisAnalysis {

}

object OasisAnalysis {

  def sumOfExtrapolatedSensorValues(filename: String) = {
    extractLines(filename).
      map(line => lineToValues(line)).
      map(values => extrapolate(values)).
      sum
  }

  def sumOfExtrapolatedBackwardsSensorValues(filename: String) = {
    extractLines(filename).
      map(line => lineToValues(line)).
      map(values => extrapolateBackwards(values)).
      sum
  }

  private def extractLines(filename: String) = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()
    lines
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

  def extrapolateBackwards(values: List[Long]): Long = {
    if (values.forall(_ == 0L))
      values.head
    else {
      values.head - extrapolateBackwards(differences(values, List()))
    }
  }

}
