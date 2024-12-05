package aoc2024.day05

import utils.ParseFile.parseFile

class PageOrderingRules {

  def sumOfMiddleNumbersOfCorrectlyOrderedUpdates(file: String): Int = {
    val lines = parseFile(file)
    val pageOrderingRules = parsePageOrderingRules(lines)
    val updatePageNumbers = parsePageUpdateNumbers(lines, pageOrderingRules)

    updatePageNumbers.
      filter(update =>
        rulesForUpdate(pageOrderingRules, update).
          forall { case (pageBefore, pageAfter) => update.indexOf(pageBefore) < update.indexOf(pageAfter) }
      ).
      map(update => update(update.length / 2)).
      sum
  }

  def sumOfMiddleNumbersOfCorrectedOrderedUpdates(file: String): Int = {
    val lines = parseFile(file)
    val pageOrderingRules = parsePageOrderingRules(lines)
    val updatePageNumbers = parsePageUpdateNumbers(lines, pageOrderingRules)

    updatePageNumbers.
      filter(update =>
        !rulesForUpdate(pageOrderingRules, update).
          forall { case (pageBefore, pageAfter) => update.indexOf(pageBefore) < update.indexOf(pageAfter) }).
      map(incorrectlyOrderedUpdate =>
        correctUpdateOrder(rulesForUpdate(pageOrderingRules, incorrectlyOrderedUpdate))).
      map(update => update(update.length / 2)).
      sum
  }

  private def rulesForUpdate(pageOrderingRules: Seq[(Int, Int)], update: Seq[Int]): Seq[(Int, Int)] = {
    pageOrderingRules.filter(rule => update.contains(rule._1) && update.contains(rule._2))
  }

  private def correctUpdateOrder(rulesForUpdate: Seq[(Int, Int)]): Seq[Int] = {
    if (rulesForUpdate.isEmpty)
      Seq()
    else {
      val startingRules = findStartingRules(rulesForUpdate)
      Seq(startingRules.head._1) ++ correctUpdateOrder(rulesForUpdate.filter(rule => !startingRules.contains(rule)))
    }
  }

  private def findStartingRules(rulesForUpdate: Seq[(Int, Int)]): Seq[(Int, Int)] =
    rulesForUpdate.
      filter { case (pageBefore, _) =>
        !rulesForUpdate.map { case (_, pageAfter) => pageAfter }.contains(pageBefore) }

  private def parsePageUpdateNumbers(lines: List[String], pageOrderingRules: Seq[(Int, Int)]): Seq[Seq[Int]] =
    lines.drop(pageOrderingRules.length).tail.map(line => line.split(",").map(_.toInt).toSeq)

  private def parsePageOrderingRules(lines: List[String]): Seq[(Int, Int)] =
    lines.takeWhile(line => line != "").map(line => {
      val pageOrders = line.split("""\|""").map(_.toInt)
      (pageOrders(0), pageOrders(1))
    })
}
