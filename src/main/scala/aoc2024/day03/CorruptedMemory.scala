package aoc2024.day03

import utils.ParseFile.parseFile

import scala.util.matching.Regex

class CorruptedMemory {

  def calculate(memory: String): Int = {
    val regex: Regex = """mul\(([0-9]+),([0-9]+)\)""".r
    regex.findAllIn(memory).map {
      case regex(num1, num2) => num1.toInt * num2.toInt
    }.sum
  }

  def calculateFromFile(file: String): Int = parseFile(file).map(line => calculate(line)).sum

  def calculateFromFileDoDont(file: String): Int = parseFile(file).map(line => calculateDoDont(line)).sum

  var enabled = true

  def calculateDoDont(memory: String): Int = {
    val regex:     Regex = """mul\(([0-9]+),([0-9]+)\)|do\(\)|don't\(\)""".r
    val mulRegex:  Regex = """mul\(([0-9]+),([0-9]+)\)""".r
    val doRegex:   Regex = """do\(\)""".r
    val dontRegex: Regex = """don't\(\)""".r

    regex.findAllIn(memory).
        map {
      case mulRegex (num1, num2) => if (enabled) num1.toInt * num2.toInt else 0
      case dontRegex() => enabled = false; 0
      case doRegex()   => enabled = true; 0
      case s =>
          0
    }.sum
  }

}
