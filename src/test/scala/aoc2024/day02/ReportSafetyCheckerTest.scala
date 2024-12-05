
package aoc2024.day02

import org.scalatest.flatspec.AnyFlatSpec

class ReportSafetyCheckerTest extends AnyFlatSpec {

  "safeReportCount" should "show 2 safe reports in example data" in {
    assert(new ReportSafetyChecker().safeReportCount("aoc2024/day02/SimpleExampleReports.txt") == 2)
  }

  "Report" should "allIncreasing" in {
    assert(Report(Seq(7, 6, 4, 2, 1)).allIncreasing == false)
    assert(Report(Seq(7, 6, 4, 2, 1)).allDecreasing == true)
  }


  "safeReportCount" should "show 236 safe reports in full data" in {
    assert(new ReportSafetyChecker().safeReportCount("aoc2024/day02/FullReports.txt") == 236)
  }

  "safeReportCountWithDampener" should "have 4 safe reports in example data" in {
    assert(new ReportSafetyChecker().safeReportCountWithDampener("aoc2024/day02/SimpleExampleReports.txt") == 4)
  }

  "safeReportCountWithDampener" should "show 308 safe reports in full data" in {
    assert(new ReportSafetyChecker().safeReportCountWithDampener("aoc2024/day02/FullReports.txt") == 308)
  }

  "Report" should "allIncreasingWithDampener" in {
    assert(Report(Seq(7, 6, 4, 2, 1)).allIncreasingWithDampener == false)
    assert(Report(Seq(7, 6, 4, 2, 1)).allDecreasingWithDampener == true)

    assert(Report(Seq(1, 2, 7, 8, 9)).allIncreasingWithDampener == false)
    assert(Report(Seq(1, 2, 7, 8, 9)).allDecreasingWithDampener == false)

    assert(Report(Seq(9, 7, 6, 2, 1)).allIncreasingWithDampener == false)
    assert(Report(Seq(9, 7, 6, 2, 1)).allDecreasingWithDampener == false)

    assert(Report(Seq(1, 3, 2, 4, 5)).allIncreasingWithDampener == true)
    assert(Report(Seq(1, 3, 2, 4, 5)).allDecreasingWithDampener == false)

    assert(Report(Seq(8, 6, 4, 4, 1)).allIncreasingWithDampener == false)
    assert(Report(Seq(8, 6, 4, 4, 1)).allDecreasingWithDampener == true)

    assert(Report(Seq(1, 3, 6, 7, 9)).allIncreasingWithDampener == true)
    assert(Report(Seq(1, 3, 6, 7, 9)).allDecreasingWithDampener == false)


    assert(Report(Seq(8, 6, 4, 5, 3)).allIncreasingWithDampener == false)
    assert(Report(Seq(8, 11, 7, 6, 5)).allDecreasingWithDampener == true)

    assert(Report(Seq(5, 6, 7, 8, 3)).allIncreasingWithDampener == true)
    assert(Report(Seq(8, 7, 6, 5, 8)).allDecreasingWithDampener == true)

    assert(Report(Seq(5, 6, 7, 8, 3)).allIncreasingWithDampener == true)
    assert(Report(Seq(8, 7, 6, 7, 5)).allDecreasingWithDampener == true)
    assert(Report(Seq(8, 7, 6, 7)).allDecreasingWithDampener == true)
    assert(Report(Seq(7, 7, 6, 5)).allDecreasingWithDampener == true)

    assert(Report(Seq(92, 94, 97, 98, 97)).allIncreasingWithDampener == true)
    assert(Report(Seq(92, 94, 97, 98, 97)).safeWithDampener == true)
    assert(Report(Seq(26, 27, 28, 31, 33, 34, 37, 37)).allIncreasingWithDampener == true)
    assert(Report(Seq(26, 27, 28, 31, 33, 34, 37, 37)).safeWithDampener == true)
    assert(Report(Seq(56, 59, 60, 61, 62, 65, 69)).allIncreasingWithDampener == true)
    assert(Report(Seq(42, 44, 46, 48, 55)).allIncreasingWithDampener == true)
    assert(Report(Seq(42, 44, 46, 48, 55)).safeWithDampener == true)
    assert(Report(Seq(15, 18, 19, 22, 21, 24, 26)).allIncreasingWithDampener == true)
    assert(Report(Seq(15, 18, 19, 22, 21, 24, 26)).safeWithDampener == true)
    assert(Report(Seq(74, 76, 77, 76, 77, 78, 75)).allIncreasingWithDampener == false)
    assert(Report(Seq(74, 76, 77, 76, 77, 78, 75)).allDecreasingWithDampener == false)
    assert(Report(Seq(74, 76, 77, 76, 77, 78, 75)).safeWithDampener == false)
    assert(Report(Seq(60, 62, 64, 65, 64, 64)).allIncreasingWithDampener == false)
    assert(Report(Seq(60, 62, 64, 65, 64, 64)).allDecreasingWithDampener == false)
    assert(Report(Seq(60, 62, 64, 65, 64, 64)).safeWithDampener == false)
    assert(Report(Seq(61, 64, 67, 64, 67, 68, 72)).allIncreasingWithDampener == false)
    assert(Report(Seq(61, 64, 67, 64, 67, 68, 72)).allDecreasingWithDampener == false)
    assert(Report(Seq(61, 64, 67, 64, 67, 68, 72)).safeWithDampener == false)
    assert(Report(Seq(71, 74, 73, 76, 82)).allIncreasingWithDampener == false)
    assert(Report(Seq(71, 74, 73, 76, 82)).allDecreasingWithDampener == false)
    assert(Report(Seq(71, 74, 73, 76, 82)).safeWithDampener == false)
    assert(Report(Seq(25, 26, 27, 29, 29, 32)).allIncreasingWithDampener == true)
    assert(Report(Seq(25, 26, 27, 29, 29, 32)).safeWithDampener == true)
    assert(Report(Seq(27, 28, 30, 30, 32, 35, 34)).allIncreasingWithDampener == false)
    assert(Report(Seq(27, 28, 30, 30, 32, 35, 34)).allDecreasingWithDampener == false)
    assert(Report(Seq(27, 28, 30, 30, 32, 35, 34)).safeWithDampener == false)
    assert(Report(Seq(6, 7, 7, 9, 9)).allIncreasingWithDampener == false)
    assert(Report(Seq(6, 7, 7, 9, 9)).allDecreasingWithDampener == false)
    assert(Report(Seq(6, 7, 7, 9, 9)).safeWithDampener == false)
    assert(Report(Seq(33, 34, 34, 35, 39)).allIncreasingWithDampener == false)
    assert(Report(Seq(33, 34, 34, 35, 39)).allDecreasingWithDampener == false)
    assert(Report(Seq(33, 34, 34, 35, 39)).safeWithDampener == false)
    assert(Report(Seq(17, 19, 20, 23, 23, 25, 26, 32)).allIncreasingWithDampener == false)
    assert(Report(Seq(17, 19, 20, 23, 23, 25, 26, 32)).allDecreasingWithDampener == false)

    assert(Report(Seq(17, 19, 20, 23, 23, 25, 26, 32)).safeWithDampener == false)
    assert(Report(Seq(29, 30, 31, 35, 37, 38, 39, 41)).safeWithDampener == false)
    assert(Report(Seq(22, 25, 29, 32, 33, 34, 37, 34)).safeWithDampener == false)
    assert(Report(Seq(15, 18, 20, 24, 26, 27, 30, 30)).safeWithDampener == false)
    assert(Report(Seq(44, 45, 48, 52, 54, 55, 58, 62)).safeWithDampener == false)
    assert(Report(Seq(35, 37, 38, 39, 42, 46, 49, 55)).safeWithDampener == false)
    assert(Report(Seq(71, 74, 77, 84, 86, 89, 91)).safeWithDampener == false)
    assert(Report(Seq(23, 25, 27, 32, 29)).safeWithDampener == true)
    assert(Report(Seq(14, 15, 16, 18, 25, 25)).safeWithDampener == false)
    assert(Report(Seq(10, 11, 14, 19, 21, 24, 28)).safeWithDampener == false)
    assert(Report(Seq(40, 41, 46, 47, 49, 55)).safeWithDampener == false)
    assert(Report(Seq(10, 11, 13, 15, 18, 20)).safeWithDampener == true)
    assert(Report(Seq(12, 10, 11, 13, 15, 18, 20)).safeWithDampener == true)
  }

}
