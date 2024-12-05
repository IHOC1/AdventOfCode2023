package aoc2024.day05

import org.scalatest.flatspec.AnyFlatSpec

class PageOrderingRulesTest extends AnyFlatSpec {

  "Example Page Ordering Rules" should "sum of middle numbers of correctly ordered updates is 143" in {
    assert(new PageOrderingRules().sumOfMiddleNumbersOfCorrectlyOrderedUpdates("aoc2024/day05/ExamplePageOrderingRules") == 143)
  }

  "Full Page Ordering Rules" should "sum of middle numbers of correctly ordered updates is 4814" in {
    assert(new PageOrderingRules().sumOfMiddleNumbersOfCorrectlyOrderedUpdates("aoc2024/day05/FullPageOrderingRules") == 4814)
  }

  "Example Page Ordering Rules" should "sum of middle numbers of corrected updates is 123" in {
    assert(new PageOrderingRules().sumOfMiddleNumbersOfCorrectedOrderedUpdates("aoc2024/day05/ExamplePageOrderingRules") == 123)
  }

  "Full Page Ordering Rules" should "sum of middle numbers of corrected updates is 5448" in {
    assert(new PageOrderingRules().sumOfMiddleNumbersOfCorrectedOrderedUpdates("aoc2024/day05/FullPageOrderingRules") == 5448)
  }

}
