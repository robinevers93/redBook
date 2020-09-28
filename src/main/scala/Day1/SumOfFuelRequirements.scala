package Day1

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}


object SumOfFuelRequirements {

  def getMassesList(filename: String): List[Int] = {
    val bufferedSource: BufferedSource = Source.fromFile(filename)
    val massesList: List[Int] = bufferedSource.getLines.toList.map(_.toInt)
    bufferedSource.close
    massesList
  }

  @tailrec
  def requiredFuel(moduleMass: Int, totalRequiredFuel: Int = 0): Int = {
    val fuelForMass = math.floor(moduleMass/3).toInt - 2
    if(fuelForMass >= 9 ) {
      requiredFuel(fuelForMass, totalRequiredFuel + fuelForMass)
    } else totalRequiredFuel + fuelForMass
  }

  def getFuelList(massesList: List[Int]): List[Int] = massesList.map(requiredFuel(_,0))

  def sumAll(fuelList: List[Int]): Int = fuelList.sum
}