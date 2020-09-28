package Day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SumOfFuelRequirementsSpec extends AnyFlatSpec with Matchers {

  val filename = "src/main/resources/puzzleInputDay1"
  val massesList: List[Int] = SumOfFuelRequirements.getMassesList(filename)
  val fuelList: List[Int] = SumOfFuelRequirements.getFuelList(massesList)


  "getRequiredFuel" should "take the floor of a number divided by 3, then subtract 2" in {
    val massToFuel = SumOfFuelRequirements.requiredFuel(12)
    massToFuel mustBe 2
    val massToFuel2 = SumOfFuelRequirements.requiredFuel(14)
    massToFuel2 mustBe 2
    val massToFuel3 = SumOfFuelRequirements.requiredFuel(1969)
    massToFuel3 mustBe 654
    val massToFuel4 = SumOfFuelRequirements.requiredFuel(100756)
    massToFuel4 mustBe 33583
  }

  "massesList" should "start with 74819 and end with 127943" in {

    val firstMass = massesList.head
    firstMass mustBe 74819
    val lastMass = massesList.last
    lastMass mustBe 127943
  }

  "fuelList" should "start with " in {
    val firstFuel = fuelList.head
    firstFuel mustBe SumOfFuelRequirements.requiredFuel(74819)
    val lastFuel = fuelList.last
    lastFuel mustBe SumOfFuelRequirements.requiredFuel(127943)
  }

  "sum" should "equal 3380880" in {
    val answer = SumOfFuelRequirements.sumAll(fuelList)
    answer mustBe 3380880
  }
}
