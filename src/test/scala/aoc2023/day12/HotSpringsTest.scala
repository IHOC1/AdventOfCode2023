package aoc2023.day12

import aoc2023.day12.HotSprings.{differentArrangements, numArrangementsPython, removeConsecutiveOperationalSprings, sumOfArrangements, sumOfExpandedArrangements}
import org.scalatest.flatspec.AnyFlatSpec

class HotSpringsTest extends AnyFlatSpec {

  /* This is the distribution of numbers of unknowns
     1        5
     1        6
     3        7
    25        8
    52        9
    97       10
   119       11
   116       12
   125       13
   131       14
   109       15
    92       16
    65       17
    45       18
    13       19
     4       20
     2       21
     Since the search space is 2^n (n is 5 to 21) then we will likely need some pruning to avoid slow computation
     (2^21 is about 2 million cases, and it total there are 83 million possible combinations)

     We could start at the beginning and prune when the groups don't match.
     There is also the possibility to try to use any anchoring groups to limit the search space? This seems harder to detect.
     */
  "Condition records with no unknown conditions" should "have only one arrangement" in {
//    assert(numArrangements("#.#.###"            , Seq(1,1,3)  ) === 1)
//    assert(numArrangements(".#...#....###."     , Seq(1,1,3)  ) === 1)
//    assert(numArrangements(".#.###.#.######"    , Seq(1,3,1,6)) === 1)
//    assert(numArrangements("####.#...#..."      , Seq(4,1,1)  ) === 1)
//    assert(numArrangements("#....######..#####.", Seq(1,6,5)  ) === 1)
//    assert(numArrangements(".###.##....#"       , Seq(3,2,1)  ) === 1)
  }

  "Condition records with some unknown conditions" should "have varying numbers of arrangements" in {
//    assert(numArrangements("???.###"            , Seq(1,1,3)  ) ===  1)
//    assert(numArrangements(".??..??...?##."     , Seq(1,1,3)  ) ===  4)
//    assert(numArrangements("?#?#?#?#?#?#?#?"    , Seq(1,3,1,6)) ===  1)
//    assert(numArrangements("????.#...#..."      , Seq(4,1,1)  ) ===  1)
//    assert(numArrangements("????.######..#####.", Seq(1,6,5)  ) ===  4)
//    assert(numArrangements("?###????????"       , Seq(3,2,1)  ) === 10)
  }

  "Sum of different arrangements" should "be 21 for the test data" in {
    assert(sumOfArrangements("Day12HotSpringsTest1.txt") === 21)
  }

  "Sum of different arrangements" should "be 7490 for the data" in {
    assert(sumOfArrangements("Day12HotSprings.txt") === 7490)
  }

  // Part 2

  "Sum of different arrangements for part 2" should "be 525152 for the test data" in {
    assert(sumOfExpandedArrangements("Day12HotSpringsTest1.txt") === 525152)
  }

  "Sum of different arrangements for part 2" should "be 525152 for the data" in {
    assert(sumOfExpandedArrangements("Day12HotSprings.txt") === 525152)
  }

}
