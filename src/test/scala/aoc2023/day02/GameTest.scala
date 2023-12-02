package aoc2023.day02

import org.scalatest.flatspec.AnyFlatSpec

class GameTest extends AnyFlatSpec {

  "A Game is possible" should "all coloured cubes be less than the limit" in {
    assert(new Game(2, 2, 2).isPossible(new Game(3, 3, 3)))
  }

  "A Game is not possible" should "the red cubes be greater than the limit" in {
    assert(new Game(2, 2, 2).isPossible(new Game(1, 3, 3)) === false)
  }

  "A Game is not possible" should "the green cubes be greater than the limit" in {
    assert(new Game(2, 4, 2).isPossible(new Game(3, 3, 3)) === false)
  }

  "A Game is not possible" should "the blue cubes be greater than the limit" in {
    assert(new Game(2, 2, 5).isPossible(new Game(3, 3, 4)) === false)
  }

  "A simple game " should "be parsable" in {
    assert(Game.parseGame("1 red, 2 green, 6 blue") === new Game(1, 2, 6))
  }

  "A line " should "be parsable" in {
    assert(Game.parseLine("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") ===
      (1 -> List(new Game(4, 0, 3),
                 new Game(1, 2, 6),
                 new Game(0, 2, 0))))
  }

  "Part 1 " should "Give the correct sum" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day02Games.txt")
    val limit = Game(12, 13, 14)
    val sum = source.getLines().
      map(line => Game.parseLine(line)).
      filter(l => l._2.forall(g => g.isPossible(limit))).
      map(l => l._1).sum
    println(sum)
    source.close()
  }

  // Part 2

  "Game merging" should "take the highest of each element" in {
    assert(new Game(4, 2, 1).merge(new Game(3, 2, 2)) === new Game(4, 2, 2))
  }

  "Part 2 " should "Give the correct sum" in {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/Day02Games.txt")
    val sum = source.getLines().
      map(line => Game.parseLine(line)._2).
      map(games => games.reduce((g1, g2) => g1.merge(g2))).
      map(game => game.power()).
      sum

    println(sum)
    source.close()
  }


}
