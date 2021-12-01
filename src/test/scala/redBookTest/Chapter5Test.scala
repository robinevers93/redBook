package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import redBook.chapter5._

class Chapter5Test extends AnyFlatSpec with Matchers {

  "toList" should "turn stream into list" in {
    val stream = Stream(1,2,3,4)
    val expectedList = List(1,2,3,4)

    stream.toList shouldBe expectedList
  }

  "take n" should "take the first n elements" in {
    val stream = Stream(1,2,3,4)
    val expectedStream = Stream(1,2)

    stream.take(2).toList shouldBe expectedStream.toList
  }

  "drop n" should "drop the first n elements" in {
    val stream = Stream(1,2,3,4)
    val expectedStream = Stream(3,4)

    stream.drop(2).toList shouldBe expectedStream.toList
  }

  "take while" should "take the first 2 elements" in {
    val stream = Stream(1,2,3,4)
    val p: Int => Boolean = (x: Int) => x < 4
    val expectedStream = Stream(1,2,3)

    stream.takeWhile(p).toList shouldBe expectedStream.toList
  }

  "forall" should "be true if and only if all elements in a stream match the predicate" in {
    val stream = Stream(1,2,3,4)
    val p1: Int => Boolean = (x: Int) => x < 4
    val p2: Int => Boolean = (x: Int) => x < 5

    stream.forAll(p1) shouldBe false
    stream.forAll(p2) shouldBe true
  }


}
