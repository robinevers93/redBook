package Day2

import SharedContent.General.getPuzzleInput

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Intcode {

  def getCSV(filename: String): List[Int] = {
    getPuzzleInput(_.toString)(filename).head.split(",").map(_.toInt).toList
  }

  def replaceSecondTwoValues(csvFile: List[Int], x: Int, y: Int): List[Int] = {
    csvFile.patch(1, Seq(x, y), 2)
  }

  def splitCSV(csvFile: List[Int]): List[List[Int]] = {
    var listSplit = new ListBuffer[List[Int]]
    var shortList = new ListBuffer[Int]()
    var maxThree = 0

    for (x <- csvFile) {
      shortList += x
      if (maxThree == 3) {listSplit += shortList.toList; shortList.clear(); maxThree = 0}
      else maxThree = maxThree + 1
    }
    if (shortList.nonEmpty) listSplit += shortList.toList
    listSplit.toList
  }

  def executeOpcode1(csvFile: List[Int], x: Int, y: Int, z: Int): List[Int] = {
    val sum: Int = csvFile(x) + csvFile(y)
    csvFile.patch(z, Seq(sum), 1)
  }

  def executeOpcode2(csvFile: List[Int], x: Int, y: Int, z: Int): List[Int] = {
    val multiply = csvFile(x) * csvFile(y)
    csvFile.patch(z, Seq(multiply), 1)
  }

  @tailrec
  def executeProgram(csvFile: List[Int], loopNumber: Int = 0): List[Int] = {
    val list = splitCSV(csvFile)(loopNumber)
    list.head match {
      case 1 => executeProgram(executeOpcode1(csvFile,list(1),list(2),list(3)), loopNumber + 1)
      case 2 => executeProgram(executeOpcode2(csvFile,list(1),list(2),list(3)), loopNumber + 1)
      case 99 => csvFile
    }
  }
}
