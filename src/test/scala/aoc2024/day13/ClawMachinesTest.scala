package aoc2024.day13

import org.scalatest.flatspec.AnyFlatSpec

class ClawMachinesTest extends AnyFlatSpec {

  "Test claw machine configs" should "use a minimum of 480 tokens to will all prizes" in {
    assert(new ClawMachines().minTokensToWinAllPrizes("aoc2024/day13/TestClawMachineConfigs.txt") == 480L)
  }

  "Full claw machine configs" should "use a minimum of 39748 tokens to will all prizes" in {
    assert(new ClawMachines().minTokensToWinAllPrizes("aoc2024/day13/FullClawMachineConfigs.txt") == 39748L)
  }

  "Test claw machine configs with conversion corrected" should "not be able to will all prizes without going over 100 press limit" in {
    assert(new ClawMachines().minTokensToWinAllPrizesConversionCorrected("aoc2024/day13/TestClawMachineConfigs.txt") == 875318608908L)
  }

  "Test case 2 with conversion corrected" should "have a solution over the 100 button press limit" in {
    val case2 = ClawMachines.ClawMachineConfig(26, 66, 67, 21, 12748, 12176).correctConversion()
    assert(case2.hasWinningCombination == true)
    assert(case2.winningTokenCost == 459236326669L)
  }

  "Test case 4 with conversion corrected" should "have a solution over the 100 button press limit" in {
    val case4 = ClawMachines.ClawMachineConfig(69, 23, 27, 71, 18641, 10279).correctConversion()
    assert(case4.hasWinningCombination == true)
    assert(case4.winningTokenCost == 416082282239L)
  }

  "Full claw machine configs with conversion corrected" should "use a minimum of 74478585072604 tokens to will all prizes" in {
    assert(new ClawMachines().minTokensToWinAllPrizesConversionCorrected("aoc2024/day13/FullClawMachineConfigs.txt") == 74478585072604L)
  }

}
