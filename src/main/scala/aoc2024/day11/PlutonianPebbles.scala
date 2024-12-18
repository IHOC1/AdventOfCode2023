package aoc2024.day11

import scala.annotation.tailrec
import scala.collection.mutable.Map

class PlutonianPebbles {

  @tailrec
  final def countStonesAfter(pebbles: Map[Long, Long], count: Int):Long = {
    println(s"$count")
    if (count == 0)
      pebbles.values.sum
    else
      countStonesAfter(blink(pebbles), count - 1)
  }

  private def blink(pebbles: Map[Long, Long]): Map[Long, Long] = {
    val newMap: collection.mutable.Map[Long, Long] = scala.collection.mutable.Map.empty[Long, Long].withDefaultValue(0)
    pebbles.keySet.foreach(p => {
      val numPebbles = pebbles(p)
      if (p == 0)
        newMap(1L) = newMap(1L) + numPebbles
      else {
        val numAsStr = p.toString
        if (numAsStr.length % 2 == 0) {
          val (firstHalfDigits, secondHalfDigits) = numAsStr.splitAt(numAsStr.length / 2)
          val firstNum = firstHalfDigits.toLong
          val secondNum = secondHalfDigits.toLong
          newMap(firstNum) = newMap(firstNum) + numPebbles
          newMap(secondNum) = newMap(secondNum) + numPebbles
        }
        else {
          val multipliedPebble: Long = p * 2024L
          newMap(multipliedPebble) = newMap(multipliedPebble) + numPebbles
        }
      }
    })

    newMap
  }

}

object day11 {

  def stringToPebbles(initialPebbles: String): collection.mutable.Map[Long, Long] = {
    val pebbles: Predef.Map[Long, Long] = initialPebbles.split(" ").map(_.toLong).groupBy(p => p).map { case (p, a) => (p, a.length) }
    collection.mutable.Map(pebbles.toSeq: _*)
  }

}