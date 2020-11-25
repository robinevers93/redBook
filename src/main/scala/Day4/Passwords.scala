package Day4

import SharedContent.General.getPuzzleInput

import scala.annotation.tailrec

object Passwords {

    def isSixDigits(x: Int): Boolean = { x>=100000 & x<1000000 }

    def rangeToListOfStrings(x: Int, y: Int): List[String] = (x to y).map(_.toString).toList

    def stringToIntList(x: String): List[Int] = x.map(_.asDigit).toList

    @tailrec
    def hasAdjacentDigits(x: List[Int]): Boolean = {
        if (x.length < 2)
            false
        else if (x.head == x.tail.head)
            true
        else hasAdjacentDigits(x.tail)
    }

    @tailrec
    def digitsNeverDecrease(x: List[Int]): Boolean = {
        if (x.length < 2)
            true
        else if (x.head <= x.tail.head)
            digitsNeverDecrease(x.tail)
        else
            false
    }

  @tailrec
  def numberOfPossiblePasswords(x: List[String], acc: Int = 0): Int = {
    if (x.isEmpty)
      return acc

    val firstNumber = stringToIntList(x.head)

    if (hasAdjacentDigits(firstNumber) && digitsNeverDecrease(firstNumber)) {
      println(firstNumber)
      numberOfPossiblePasswords(x.tail, acc + 1)
    } else
      numberOfPossiblePasswords(x.tail, acc)
  }

  @tailrec
  def hasExactlyTwoAdjacentDigits(x: List[Int], acc: Int = 1): Boolean =
    x match {
      case _ if x.length < 2 => acc == 2
      case _ if x.head == x.tail.head => hasExactlyTwoAdjacentDigits(x.tail, acc + 1)
      case _ if acc == 2 => true
      case _ => hasExactlyTwoAdjacentDigits(x.tail)
    }

  @tailrec
  def numberOfPossiblePasswords2(x: List[String], acc: Int = 0): Int = {
    if (x.isEmpty)
      return acc

    val firstNumber = stringToIntList(x.head)

    if (hasExactlyTwoAdjacentDigits(firstNumber) & digitsNeverDecrease(firstNumber)) {
      println(firstNumber)
      numberOfPossiblePasswords2(x.tail, acc + 1)
    } else
      numberOfPossiblePasswords2(x.tail, acc)
  }

}
