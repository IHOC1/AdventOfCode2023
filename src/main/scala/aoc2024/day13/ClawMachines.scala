package aoc2024.day13

import aoc2024.day13.ClawMachines.ClawMachineConfig
import utils.ParseFile.parseFile

class ClawMachines {
  def minTokensToWinAllPrizes(file: String): Long =
    parseMachineConfigs(parseFile(file).iterator).
      filter(_.withinButtonPressLimit).
      map(_.winningTokenCost).
      sum

  def minTokensToWinAllPrizesConversionCorrected(file: String): Long =
    parseMachineConfigs(parseFile(file).iterator).
      map(_.correctConversion).
      // Note that 100 button press limit does not apply to part 2!
      map(_.winningTokenCost).
      sum

  private def parseMachineConfigs(lineIterator: Iterator[String]): Seq[ClawMachineConfig] = {
    if (!lineIterator.hasNext)
      Seq()
    else {
      val machineLines = lineIterator.take(4).toSeq

      val buttonA = parseButton(machineLines(0))
      val buttonB = parseButton(machineLines(1))
      val prize = parsePrize(machineLines(2))

      ClawMachineConfig(
        buttonA(0), buttonA(1),
        buttonB(0), buttonB(1),
        prize(0), prize(1),
      ) +: parseMachineConfigs(lineIterator)
    }
  }

  private def parseButton(buttonLine: String) = {
    buttonLine.split(":")(1).split(",").map(_.trim).map(dir => dir.split("""\+""")(1).toLong).toSeq
  }

  private def parsePrize(buttonLine: String) = {
    buttonLine.split(":")(1).split(",").map(_.trim).map(dir => dir.split("""\=""")(1).toLong).toSeq
  }

}

object ClawMachines {
  case class ClawMachineConfig(
                                xA    : Long, yA    : Long,
                                xB    : Long, yB    : Long,
                                xPrize: Long, yPrize: Long,
                              ) {
    private val mNumerator = yB * xPrize - yPrize * xB
    private val mDenominator = xA * yB - yA * xB
    val numAPresses = mNumerator / mDenominator

    private val nNumerator = yPrize - numAPresses * yA
    private val nDenominator = yB
    val numBPresses = nNumerator / nDenominator

    val hasWinningCombination = mNumerator % mDenominator == 0 && nNumerator % nDenominator == 0

    val withinButtonPressLimit = numAPresses <= 100 && numBPresses <= 100

    lazy val winningTokenCost = hasWinningCombination match {
      case true  => 3 * numAPresses + numBPresses
      case false => 0
    }

    def correctConversion(): ClawMachineConfig =
      ClawMachineConfig(xA, yA,
                        xB, yB,
                        xPrize + 10000000000000L, yPrize + 10000000000000L)
  }

}