package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import redBook.Chapter4._

class Chapter4Test extends AnyFlatSpec with Matchers {

  "mean" should "compute variance" in {
    val list= Seq(1.0,2.0,3.0,4.0)
    Chapter4.mean(list) shouldBe Some(2.5)
  }

  "variance" should "compute variance" in {
    val list = Seq(1.0,2.0,3.0,4.0)
//    val mean = 2.5
//    val diffFromMean = Seq(1.5, 0.5, 0.5, 1.5)
//    val diffFromMeanSquared = Seq(2.25, 0.25, 0.25, 2.25)
    val variance = 1.25

    Chapter4.variance(list) shouldBe Some(variance)
  }

  "map2" should "lift function with 2 parameters" in {
    val f: (Int, Int) => Int = (x: Int,y: Int) => x + y
    val a = Chapter4.Try("5".toInt)
    val b = Chapter4.Try("4".toInt)
    val c = Chapter4.Try("hello".toInt)

    Chapter4.map2(a, b)(f) shouldBe Some(9)
    Chapter4.map2(a, c)(f) shouldBe None()

    Chapter4.map2asForComprehension(a, b)(f) shouldBe Some(9)
    Chapter4.map2asForComprehension(a, c)(f) shouldBe None()
  }

  "sequence" should "return None if one or more None is found in list and Some of list otherwise" in {
    val a = List(Some(1), Some(2), Some(3), Some(4))
    val b = List(Some(1), Some(2), None(), Some(4))

    Chapter4.sequence(a) shouldBe Some(List(1,2,3,4))
    Chapter4.sequence(b) shouldBe None()

    Chapter4.sequenceUsingTraverse(a) shouldBe Some(List(1,2,3,4))
    Chapter4.sequenceUsingTraverse(b) shouldBe None()
  }

  "traverse" should "return None if one or more None is found in list after mapping and Some of mapped list otherwise" in {
    val a = List("1","2","3","4")
    val b = List("1", "2", "hello", "4")

    val f: String => Option[Int] = x => Chapter4.Try(x.toInt)

    Chapter4.traverseSlow(a)(f) shouldBe Some(List(1,2,3,4))
    Chapter4.traverseSlow(b)(f) shouldBe None()

    Chapter4.traverse(a)(f) shouldBe Some(List(1,2,3,4))
    Chapter4.traverse(b)(f) shouldBe None()

    Chapter4.traverseUsingFold(a)(f) shouldBe Some(List(1,2,3,4))
    Chapter4.traverseUsingFold(b)(f) shouldBe None()
  }

}
