package aoc2023.day09

import scala.annotation.tailrec

class OasisAnalysis {

}

object OasisAnalysis {

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
