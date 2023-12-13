package aoc2023.day09

import aoc2023.day09.OasisAnalysis.extrapolate
import org.scalatest.flatspec.AnyFlatSpec

class OasisAnalysisTest extends AnyFlatSpec {

  "Extrapolation" should "work for the simplest test input" in {
    assert(extrapolate(List(0, 3, 6, 9, 12, 15)) === 18L)
  }
}
