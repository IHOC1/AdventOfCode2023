package aoc2023.day08

case class HauntedWasteland(leftRightInstructions: Array[Char],
                            network: Map[String, (String, String)]) {

  def numberOfStepsToZZZ(node: String, leftRightIndex: Int): Int = {
    println(node)
    if (node == "ZZZ")
      0
    else {
      println(network)
      val next = leftRightInstructions(leftRightIndex) match {
        case 'L' => network(node)._1
        case 'R' => network(node)._2
      }
      1 + numberOfStepsToZZZ(next, (leftRightIndex + 1) % leftRightInstructions.length)
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
