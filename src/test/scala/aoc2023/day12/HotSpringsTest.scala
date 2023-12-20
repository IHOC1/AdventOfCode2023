package aoc2023.day12

import aoc2023.day12.HotSprings.differentArrangements
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
  "Condition records" should "have only one arrangement if no conditions are unknown" in {
    assert(differentArrangements("#.#.###"            , Seq(1,1,3)  ) === Seq("#.#.###")            )
    assert(differentArrangements(".#...#....###."     , Seq(1,1,3)  ) === Seq(".#...#....###.")     )
    assert(differentArrangements(".#.###.#.######"    , Seq(1,3,1,6)) === Seq(".#.###.#.######")    )
    assert(differentArrangements("####.#...#..."      , Seq(4,1,1)  ) === Seq("####.#...#...")      )
    assert(differentArrangements("#....######..#####.", Seq(1,6,5)  ) === Seq("#....######..#####."))
    assert(differentArrangements(".###.##....#"       , Seq(3,2,1)  ) === Seq(".###.##....#")       )
  }

}
