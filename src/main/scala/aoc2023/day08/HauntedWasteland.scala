package aoc2023.day08

import scala.Console.flush
import scala.annotation.tailrec

case class HauntedWasteland(leftRightInstructions: Array[Char],
                            network: Map[String, (String, String)]) {

  final def numberOfStepsToAllEndingInZ(): Long = {
    numberOfStepsToAllEndingInZ(network.keys.filter(_.endsWith("A")).toList, 0, 0)
  }

  @tailrec
  final def numberOfStepsToAllEndingInZ(stringNodes: List[String], leftRightIndex: Int, noSteps: Long): Long = {

    val nextStringNodes = leftRightInstructions(leftRightIndex) match {
      case 'L' => stringNodes.map(node => network(node)._1)
      case 'R' => stringNodes.map(node => network(node)._2)
    }

    if (nextStringNodes.forall(_.endsWith("Z")))
      noSteps + 1
    else {
      val nextIndex = (leftRightIndex + 1) % leftRightInstructions.length
      numberOfStepsToAllEndingInZ(nextStringNodes, nextIndex, noSteps + 1)
    }
  }

  @tailrec
  final def numberOfStepsToZZZ(node: String, leftRightIndex: Int, noSteps: Long): Long = {
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

  def calculateNumberOfStepsToAllEndingInZ(): Option[Long] = {
    println(f"num instructions: ${leftRightInstructions.length}")

    // Works out the number of steps from each individual node-ending-in-A to a node-ending-in-Z
    val nodesEndingWithA = network.keys.filter(_.endsWith("A")).toList
    val noStepsAToZ = nodesEndingWithA.map(node => this.numberOfStepsToAllEndingInZ(List(node), 0, 0L))
    println("num steps from ending in A " + nodesEndingWithA.zip(noStepsAToZ))
    println(noStepsAToZ)

    // Works out the number of steps from each node ending in Z to a node ending in Z (i.e. the loops in the data)
    val nodesEndingWithZ = network.keys.filter(_.endsWith("Z"))
    val noStepsZToZ = nodesEndingWithZ.map(node => this.numberOfStepsToAllEndingInZ(List(node), 0, 0L))
    println("num steps from ending in Z " + nodesEndingWithZ.zip(noStepsZToZ))
    println(noStepsZToZ)

    // Generate compressed map
    val initialNodes = network.keys.toList
    println("Initial nodes: " + initialNodes)
    val finalNodes = leftRightInstructions.foldLeft(initialNodes)((nodes, instruction) => instruction match {
      case 'L' => nodes.map(node => network(node)._1)
      case 'R' => nodes.map(node => network(node)._2)
    })
    val compressedMap = initialNodes.zip(finalNodes).toMap
    println("Compressed map: " + compressedMap)

    nodesEndingWithA.foreach(node => println(s"Entry point for: ${node} -> ${compressedMap(node)}"))

    // Count number and size of circular mappings
    val loops = nodesEndingWithZ.map(node => {
      loopFor(node, node, compressedMap, List(node))
    })

    loops.foreach(loop => println(f"Loop (${loop.size}) ${loop.filter(_.endsWith("Z"))} ${loop} "))
    flush()

    val loopLengths = loops.map(_.length.toLong)
    val productOfPrimeLoopLengths: Long = loopLengths.foldLeft(1L)((n, length) => n * length)

    // Check that after all the iteration, we do end up at nodes ending in Z
    if (!loops.forall(loop => loop((productOfPrimeLoopLengths % loop.length).toInt).endsWith("Z")))
      return None

    Some(productOfPrimeLoopLengths * leftRightInstructions.length)
  }

  @tailrec
  final def loopFor(terminalNode: String, node: String, compressedMap: Map[String, String], loop: List[String]): List[String] = {
    val nextNode = compressedMap(node)
    if (nextNode == terminalNode)
      loop
    else
      loopFor(terminalNode, nextNode, compressedMap, loop :+ nextNode)
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
