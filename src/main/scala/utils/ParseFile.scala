package utils

object ParseFile {

  def parseFile(filename: String): List[String] = {
    import scala.io.Source

    val source = Source.fromFile("src/test/resources/" + filename)
    val lines = source.getLines().toList
    source.close()
    lines
  }

}
