package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import redBook.Chapter4
import redBook.Some

class Chapter4Test extends AnyFlatSpec with Matchers {

  "mean" should "compute variance" in {
    val list= Seq(1.0,2.0,3.0,4.0)
    Chapter4.mean(list) mustBe Some(2.5)
  }

  "variance" should "compute variance" in {
    val list= Seq(1.0,2.0,3.0,4.0)
    val mean = 2.5
    val diffFromMean = Seq(1.5, 0.5, 0.5, 1.5)
    val diffFromMeanSquared = Seq(2.25, 0.25, 0.25, 2.25)
    val variance = 1.25

    Chapter4.variance(list) mustBe Some(1.25)

  }

}
