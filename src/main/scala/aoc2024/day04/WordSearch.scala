package aoc2024.day04

import utils.ParseFile.parseFile

class WordSearch(file: String) {

  def count(searchWord: String): Int = {
    val wordSearch = parseFile(file)
    wordSearch.
      zipWithIndex.
      map{ case(line, rowIndex) =>
        line.zipWithIndex.map{ case (letter, colIndex) =>
          countWordsAt(wordSearch, rowIndex, colIndex, searchWord)
        }.sum
      }.sum
  }

  private def countWordsAt(wordSearch: List[String], rowIndex: Int, colIndex: Int, searchWord: String): Int = {
    (-1 to 1).map(rowDelta =>
      (-1 to 1).map(colDelta =>
        countWordAtLocationAndDirection(wordSearch, rowIndex, colIndex, searchWord, rowDelta, colDelta)
      ).sum
    ).sum
  }

  private def countWordAtLocationAndDirection(wordSearch: List[String], rowIndex: Int, colIndex: Int, searchWord: String, rowDelta: Int, colDelta: Int): Int = {
    val rowEndOfWord = rowIndex + searchWord.length * rowDelta
    val colEndOfWord = colIndex + searchWord.length * colDelta
    if (rowEndOfWord < -1 || rowEndOfWord >= wordSearch.head.length + 1 ||
        colEndOfWord < -1 || colEndOfWord >= wordSearch.length + 1) 0
    else {
      val chars = new String((0 until searchWord.length).map(index =>
        wordSearch(rowIndex + (index * rowDelta))(colIndex + (index * colDelta))
      ).toArray)
      if (chars == searchWord) 1 else 0
    }
  }

  def countInXFormation(searchWord: String): Int = {
    val wordCentre = searchWord.length / 2
    val wordSearch = parseFile(file)
    (wordCentre until wordSearch.length - wordCentre).map(rowIndex =>
      (wordCentre until wordSearch.head.length - wordCentre).map(colIndex =>
        countWordsInXFormationAt(wordSearch, rowIndex, colIndex, searchWord)
      ).sum
    ).sum
  }

  private def countWordsInXFormationAt(wordSearch: List[String], rowIndex: Int, colIndex: Int, searchWord: String): Int = {
    val numXFormationWords =
      Seq(-1, 1).map(rowDelta =>
        Seq(-1, 1).map(colDelta =>
          countWordsInXFormationAtLocationAndDirection(wordSearch, rowIndex, colIndex, searchWord, rowDelta, colDelta)
        ).sum
      ).sum
    if (numXFormationWords == 2) 1 else 0
  }

  private def countWordsInXFormationAtLocationAndDirection(wordSearch: List[String], rowIndex: Int, colIndex: Int, searchWord: String, rowDelta: Int, colDelta: Int): Int = {
    val chars = new String((0 - (searchWord.length / 2) to (searchWord.length / 2)).map(index => {
      val x = rowIndex + (index * rowDelta)
      val y = colIndex + (index * colDelta)
      wordSearch(x)(y)
    }
    ).toArray)
    if (chars == searchWord) 1 else 0
  }

}
