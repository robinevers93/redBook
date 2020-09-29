package SharedContent

import scala.io.{BufferedSource, Source}

object General {

  def getPuzzleInput[T](transform: String => T)(filename: String): List[T] = {
    val bufferedSource: BufferedSource = Source.fromFile(filename)
    val puzzleAsList: List[T] = bufferedSource.getLines.toList.map(transform)
    bufferedSource.close
    puzzleAsList
  }
}
