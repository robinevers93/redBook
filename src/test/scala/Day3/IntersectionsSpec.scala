package Day3

import Day3.Intersections
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class IntersectionsSpec extends AnyFlatSpec with Matchers {

  val filename = "src/main/resources/puzzleInputDay3"
  val firstWire: List[String] = Intersections.getWires(filename, 0)
  val secondWire: List[String] = Intersections.getWires(filename, 1)

  "getWires" should "split each wire up from csv to a list" in {
    firstWire.head mustBe "R990"
    firstWire.last mustBe "L63"
    secondWire.head mustBe "L995"
    secondWire.last mustBe "L669"
  }

  "instructionsToTuples" should "turn an instruction into the correct vector of tuples" in {
    val testInstruction = "R3"
    Intersections.instructionToTuples(testInstruction, Vector((0,0))) mustBe Vector((0,0),(1,0),(2,0),(3,0))
  }
  it should "also work for not starting at (0,0)" in {
    val testInstruction = "U2"
    Intersections.instructionToTuples(testInstruction, Vector((1,2))) mustBe Vector((1,2),(1,3),(1,4))
  }

  "wireToPath" should "turn a list of instructions into the corresponding path" in {
    val testInstructionList = List("R3","U2","L2")
    Intersections.wireToPath(testInstructionList) mustBe Vector((1,0), (2,0), (3,0), (3,1), (3,2), (2,2), (1,2))
  }

  "findIntersections" should "find the intersections of two paths" in {
    val path1 = Vector((1, 0), (2, 0), (3, 0), (3, 1), (3, 2), (2, 2), (1, 2))
    val path2 = Vector((1, 1), (2, 1), (3, 1), (4, 1), (4, 2), (3, 2), (2, 2), (1, 2))

    Intersections.findIntersections(path1, path2, Vector((999, 0))) mustBe Vector((999,0), (3,1), (3,2), (2,2), (1,2))
  }

  "manhattanDistance" should "find the manhattan distance" in {
    Intersections.manhattanDistance((5,-6)) mustBe 11
  }

  "findShortestIntersectionDistance" should "find the shortest intersection distance" in {
    val intersections = Vector((3,1), (3,2), (2,2), (1,2))
    Intersections.findShortestIntersectionDistance(intersections, 999) mustBe 3
  }

  "test case 1" should "give manhattan distance 159" in {
    val wire1 = List("R75","D30","R83","U83","L12","D49","R71","U7","L72")
    val wire2 = List("U62","R66","U55","R34","D71","R55","D58","R83")
    Intersections.wiresToClosestIntersection(wire1, wire2) mustBe 159
  }

  "test case 2" should "give manhattan distance 135" in {
    val wire1 = List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
    val wire2 = List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
    Intersections.wiresToClosestIntersection(wire1, wire2) mustBe 135
  }

  "puzzle solution" should "give manhattan distance 280" in {
    Intersections.wiresToClosestIntersection(firstWire, secondWire) mustBe 280
  }

  "findFewestStepIntersectionDistance" should "find the fastest intersection distance" in {
    val path1 = Vector((1, 0), (2, 0), (3, 0), (3, 1), (3, 2), (2, 2), (1, 2))
    val path2 = Vector((1, 1), (2, 1), (3, 1), (4, 1), (4, 2), (3, 2), (2, 2), (1, 2))
    Intersections.findFewestStepIntersectionDistance(path1, path2) mustBe 7
  }

  "test case 1" should "give fewest step distance 159" in {
    val wire1 = List("R75","D30","R83","U83","L12","D49","R71","U7","L72")
    val wire2 = List("U62","R66","U55","R34","D71","R55","D58","R83")
    Intersections.wiresToFastestIntersection(wire1, wire2) mustBe 610
  }

  "test case 2" should "give fewest step distance 135" in {
    val wire1 = List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
    val wire2 = List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
    Intersections.wiresToFastestIntersection(wire1, wire2) mustBe 410
  }

  "puzzle solution" should "give fewest step distance 10554" in {
    Intersections.wiresToFastestIntersection(firstWire, secondWire) mustBe 10554
  }
}


