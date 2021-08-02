package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import redBook.Chapter2._

class Chapter2Test extends AnyFlatSpec with Matchers {

  "fibonaci caluclator" should "be calculated correctly" in {
    Chapter2.fib(0) mustBe 1
    Chapter2.fib(1) mustBe 1
    Chapter2.fib(2) mustBe 2
    Chapter2.fib(3) mustBe 3
    Chapter2.fib(4) mustBe 5
    Chapter2.fib(5) mustBe 8
  }

  "sorting checker" should "work correctly correctly" in {
    Chapter2.isSorted[Int](Array(1, 2, 3, 4, 5), (a, b) => a <= b) mustBe true
    Chapter2.isSorted[Int](Array(1, 2, 4, 3, 5), (a, b) => a <= b) mustBe false
  }

  "currying" should "convert function to be partially applied" in {
    val addition: (Int, Int) => Int = (x: Int, y: Int) => x + y
    val x: Int => Int => Int = Chapter2.curry(addition)

    x(10)(15) mustBe 25
  }

  "uncurrying" should "revert partially applied function to original" in {
    val addition: (Int, Int) => Int = (x: Int, y: Int) => x + y
    val x: Int => Int => Int = Chapter2.curry(addition)
    val y = Chapter2.uncurry(x)

    y(10, 15) mustBe 25
    //    y mustBe addition
  }

  "compose" should "compose two functions " in {
    val double: Int => Int = (x: Int) => 2*x
    val half: Int => Int = (x: Int) => x/2
    val x: Int => Int = Chapter2.compose(half, double)

    x(10) mustBe 10
    x(4938) mustBe 4938
  }
}
