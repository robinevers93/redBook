package Day2

import Day2.Intcode.replaceSecondTwoValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class IntcodeSpec extends AnyFlatSpec with Matchers {

  val filename = "src/main/resources/puzzleInputDay2"
  val csvFile: List[Int] = Intcode.getCSV(filename)
  val newCsvFile: List[Int] = replaceSecondTwoValues(csvFile, 12, 2)
  val splitFile: List[List[Int]] = Intcode.splitCSV(csvFile)

  "getCSV" should "split a csv into a list of integers" in {
    csvFile.head mustBe 1
    csvFile(1) mustBe 0
    csvFile.last mustBe 0
  }

  "replaceSecondTwoValues" should "replace the first two values in a List" in {
    val sampleList = List(3500,9,10)
    val replacedSampleList = replaceSecondTwoValues(sampleList, 12, 2)
    replacedSampleList.head mustBe 3500
    replacedSampleList(1) mustBe 12
    replacedSampleList.last mustBe 2
  }

  "splitFile" should "split list on 99 or 4th comma" in {
    val unorderedList = List(3500,9,10,70,2,3,11,0,99,30,40,50)
    Intcode.splitCSV(unorderedList) mustBe List(List(3500,9,10,70),List(2,3,11,0),List(99,30,40,50))
  }

  "executeOpcode1" should "change position z of the csv to equal the sum of x and y" in {
    val list = List(1,9,10,3,2,3,11,0,99,30,40,50)
    Intcode.executeOpcode1(list,1,2,1) mustBe List(1,19,10,3,2,3,11,0,99,30,40,50)
  }

  "executeOpcode2" should "change position z of the csv to equal the multiplication of x and y" in {
    val list = List(1,9,10,3,2,3,11,0,99,30,40,50)
    Intcode.executeOpcode2(list,1,2,1) mustBe List(1,90,10,3,2,3,11,0,99,30,40,50)
  }

  "runProgram" should "function on test cases" in {
    Intcode.executeProgram(List(1,0,0,0,99)) mustBe List(2,0,0,0,99)
    Intcode.executeProgram(List(2,3,0,3,99)) mustBe List(2,3,0,6,99)
    Intcode.executeProgram(List(2,4,4,5,99,0)) mustBe List(2,4,4,5,99,9801)
    Intcode.executeProgram(List(1,1,1,4,99,5,6,0,99)) mustBe List(30,1,1,4,2,5,6,0,99)
  }

  "runProgram" should "have 4138658 at position 0 when the program halts for replaced puzzle input" in {
    Intcode.executeProgram(newCsvFile, 0).head mustBe 4138658
  }

  "findNounVerb" should "function on test case" in {
    Intcode.findNounVerb(csvFile,4138658) mustBe 1202
  }

  "findNounVerb" should "equal ... for output 19690720" in {
    Intcode.findNounVerb(csvFile,19690720) mustBe 7264
  }
}
