package Day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers


class PasswordsSpec extends AnyFlatSpec with Matchers {

  "iSixDigits" should "check an int has exactly 6 digits" in {
    Passwords.isSixDigits(462947) mustBe true
    Passwords.isSixDigits(5739562) mustBe false
    Passwords.isSixDigits(57562) mustBe false
  }

  "hasAdjacentDigits" should "check a List[Int] has two adjacent digits that are the same" in {
    val noAdjacentDigits = Passwords.stringToIntList("12345")
    val adjacentDigits1 = Passwords.stringToIntList("12344")
    val adjacentDigits2 = Passwords.stringToIntList("11234")
    val adjacentDigits3 = Passwords.stringToIntList("12234")

    Passwords.hasAdjacentDigits(noAdjacentDigits) mustBe false
    Passwords.hasAdjacentDigits(adjacentDigits1) mustBe true
    Passwords.hasAdjacentDigits(adjacentDigits2) mustBe true
    Passwords.hasAdjacentDigits(adjacentDigits3) mustBe true
  }

  "digitsNeverDecrease" should "check a List[Int] never decreases" in {
    val neverDecreases1 = Passwords.stringToIntList("12345")
    val neverDecreases2 = Passwords.stringToIntList("14789")
    val doesDecrease1 = Passwords.stringToIntList("17943")
    val doesDecrease2 = Passwords.stringToIntList("12343")

    Passwords.digitsNeverDecrease(neverDecreases1) mustBe true
    Passwords.digitsNeverDecrease(neverDecreases2) mustBe true
    Passwords.digitsNeverDecrease(doesDecrease1) mustBe false
    Passwords.digitsNeverDecrease(doesDecrease2) mustBe false
  }

  "meet puzzle rules" should "be true" in {
    val meetsCriteria = Passwords.stringToIntList("111111")
    val doesntMeetCriteria1 = Passwords.stringToIntList("223450")
    val doesntMeetCriteria2 = Passwords.stringToIntList("123789")

    Passwords.digitsNeverDecrease(meetsCriteria) & Passwords.hasAdjacentDigits(meetsCriteria) mustBe true
    Passwords.digitsNeverDecrease(doesntMeetCriteria1) & Passwords.hasAdjacentDigits(doesntMeetCriteria1) mustBe false
    Passwords.digitsNeverDecrease(doesntMeetCriteria2) & Passwords.hasAdjacentDigits(doesntMeetCriteria2) mustBe false
  }

  "numberOfPossiblePasswords" should "calculate the number of possible passwords" in {
    val listOfStrings = Passwords.rangeToListOfStrings(100,200)
    Passwords.numberOfPossiblePasswords(listOfStrings) mustBe 9 + 8
  }

  "puzzle result" should "be 495" in {
    val listOfStrings = Passwords.rangeToListOfStrings(367479,893698)
    Passwords.numberOfPossiblePasswords(listOfStrings) mustBe 495
  }

  "hasExactlyTwoAdjacentDigits" should "check a List[Int] has exactly two adjacent digits that are the same" in {
    val noAdjacentDigits = Passwords.stringToIntList("123444")
    val adjacentDigits1 = Passwords.stringToIntList("112233")
    val adjacentDigits2 = Passwords.stringToIntList("111122")

   Passwords.hasExactlyTwoAdjacentDigits(noAdjacentDigits) mustBe false
   Passwords.hasExactlyTwoAdjacentDigits(adjacentDigits1) mustBe true
   Passwords.hasExactlyTwoAdjacentDigits(adjacentDigits2) mustBe true
  }

  "puzzle result2" should "be 305" in {
    val listOfStrings = Passwords.rangeToListOfStrings(367479,893698)
    Passwords.numberOfPossiblePasswords2(listOfStrings) mustBe 305
  }

}