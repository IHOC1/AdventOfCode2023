package aoc2023.day08

import scala.annotation.tailrec

case class HauntedWasteland(leftRightInstructions: Array[Char],
                            network: Map[String, (String, String)]) {

  def numberOfStepsToAllEndingInZ(): Int = {
    val startingPoints = everyNodeThatEndsWithA()
    numberOfStepsToAllEndingInZ(startingPoints, 0, 0)
  }

  def numberOfStepsToAllEndingInZ(nodes: List[String], leftRightIndex: Int, noSteps: Int): Int = {
    if (nodes.forall(_.endsWith("Z")))
      noSteps
    else {
      val nextNodes = nodes.map(node => {
        leftRightInstructions(leftRightIndex) match {
          case 'L' => network(node)._1
          case 'R' => network(node)._2
        }
      })
      val nextIndex = (leftRightIndex + 1) % leftRightInstructions.length
      numberOfStepsToAllEndingInZ(nextNodes, nextIndex, noSteps + 1)
    }
  }

  def everyNodeThatEndsWithA() = network.keys.filter(node => node.endsWith("A")).toList

  @tailrec
  final def numberOfStepsToZZZ(node: String, leftRightIndex: Int, noSteps: Int): Int = {
    if (node == "ZZZ")
      noSteps
    else {
      val nextNode = leftRightInstructions(leftRightIndex) match {
        case 'L' => network(node)._1
        case 'R' => network(node)._2
      }
      val nextIndex = (leftRightIndex + 1) % leftRightInstructions.length
      numberOfStepsToZZZ(nextNode, nextIndex, noSteps + 1)
    }
  }

}

object HauntedWasteland {

  def parse(mapFileName: String): HauntedWasteland = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + mapFileName)
    val lines = source.getLines().toList
    source.close()

    val leftRightInstructions = lines.head.toCharArray

    val network: Map[String, (String, String)] = lines.tail.tail.map(line => {
      val parts = line.split(" = ")

      val key = parts(0)

      val leftAndRight = parts(1).split(", ")

      key -> (leftAndRight(0).substring(1) -> leftAndRight(1).substring(0,3))
    }).toMap

    HauntedWasteland(leftRightInstructions, network)
  }

}
