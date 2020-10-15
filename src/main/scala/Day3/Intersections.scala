package Day3

  import SharedContent.General.getPuzzleInput

  import scala.annotation.tailrec
  import scala.math.abs

  object Intersections {

    def getWires(filename: String, wireNumber: Int = 0): List[String] = {
      getPuzzleInput(_.toString)(filename)(wireNumber).split(",").toList
    }

    def instructionToTuples(instruction: String, acc: Vector[(Int, Int)]): Vector[(Int, Int)] = {
      val direction: Char = instruction.head
      val steps: Int = instruction.tail.toInt
      val previousPosition = acc.last

      val newPositions: Seq[(Int, Int)] = direction match {
        case 'U' => 1 to steps map (i => (previousPosition._1, previousPosition._2 + i))
        case 'D' => 1 to steps map (i => (previousPosition._1, previousPosition._2 - i))
        case 'R' => 1 to steps map (i => (previousPosition._1 + i, previousPosition._2))
        case 'L' => 1 to steps map (i => (previousPosition._1 - i, previousPosition._2))
      }
      acc ++ newPositions
    }

    @tailrec
    def wireToPath(wire: List[String], path: Vector[(Int, Int)] = Vector((0, 0))): Vector[(Int, Int)] = wire match {
      case Nil => path.tail
      case h :: t => wireToPath(t, instructionToTuples(h, path))
    }

    @tailrec
    def findIntersections(path1: Vector[(Int, Int)], path2: Vector[(Int, Int)], acc: Vector[(Int, Int)]): Vector[(Int, Int)] = {
      if (path1.isEmpty) acc
      else if (path2.contains(path1.head)) findIntersections(path1.tail, path2, acc :+ path1.head)
      else findIntersections(path1.tail, path2, acc)
    }

    @tailrec
    def findIntersections2(path1: Vector[(Int, Int)], path2: Vector[(Int, Int)], acc: Vector[(Int, Int)]): Vector[(Int, Int)] = path1 match {
      case b if b.isEmpty => acc
      case a if path2.contains(a.head) => findIntersections2(a.tail, path2, acc :+ a.head)
      case _ => findIntersections2(path1.tail, path2, acc)
    }

    def findIntersections3(path1: Vector[(Int, Int)], path2: Vector[(Int, Int)], acc: Vector[(Int, Int)]): Vector[(Int, Int)] = path1.intersect(path2)

    def manhattanDistance(location: (Int, Int)): Int = {
      abs(location._1) + abs(location._2)
    }

    @tailrec
    def findShortestIntersectionDistance(intersections: Vector[(Int, Int)], shortestDistance: Int): Int = {
      if (intersections.isEmpty) shortestDistance
      else if (manhattanDistance(intersections.head) < shortestDistance)
        findShortestIntersectionDistance(intersections.tail, manhattanDistance(intersections.head))
      else findShortestIntersectionDistance(intersections.tail, shortestDistance)
      }

    def findFewestStepIntersectionDistance(path1: Vector[(Int, Int)], path2: Vector[(Int, Int)]): Int = {
      val intersectionList = path1.intersect(path2)
      def sortPath(path: Vector[(Int, Int)]) = path.zipWithIndex.filter(x => intersectionList.contains(x._1)).distinctBy(_._1).sortBy(_._1)

      val filteredPath1 = sortPath(path1)
      val filteredPath2 = sortPath(path2)

      val addedPaths =  filteredPath1.zip(filteredPath2).map(field => (field._1._1, field._1._2 + field._2._2)).sortBy(_._2)
      addedPaths.head._2 + 2
    }

    def wiresToClosestIntersection(wire1: List[String], wire2: List[String]) : Int = {
      findShortestIntersectionDistance(findIntersections3(wireToPath(wire1), wireToPath(wire2), Vector((999,0))), 999)
    }

    def wiresToFastestIntersection(wire1: List[String], wire2: List[String]) : Int = {
      findFewestStepIntersectionDistance(wireToPath(wire1), wireToPath(wire2))
    }
  }
