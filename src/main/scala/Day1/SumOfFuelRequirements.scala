package Day1

import scala.io.{BufferedSource, Source}


object SumOfFuelRequirements {

  def getMassesList(filename: String): List[Int] = {
    val bufferedSource: BufferedSource = Source.fromFile(filename)
    val massesList: List[Int] = bufferedSource.getLines.toList.map(_.toInt)
    bufferedSource.close
    massesList
  }

  def requiredFuel(moduleMass: Int): Int = {
    math.floor(moduleMass/3).toInt - 2
  }

  def getFuelList(massesList: List[Int]): List[Int] =  massesList.map(requiredFuel)

  def sumAll(fuelList: List[Int]): Int = fuelList.sum
}