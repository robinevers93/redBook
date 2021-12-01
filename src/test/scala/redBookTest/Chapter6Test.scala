package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import redBook.chapter6.RNG.{Rand, doubleOld, map2, sequence}
import redBook.chapter6.candyDispenserStateUpdater.simulateMachine
import redBook.chapter6.{Coin, Machine, RNG, SimpleRNG, State, Turn}

class Chapter6Test extends AnyFlatSpec with Matchers {

  "generating random numbers with the same initial state + seed" should "give the same output" in {
    val rng = SimpleRNG(42)
    val (n1, nextRng1) = rng.nextInt
    val (n2, nextRng2) = rng.nextInt

    n1 shouldEqual n2
    nextRng1 shouldEqual nextRng2
  }

  "generating non-negative random numbers" should "do what it says on the tin" in {
    val rng = SimpleRNG(42)
    val (n1, nextRng1) = RNG.nonNegativeInt(rng)
    val (n2, nextRng2) = RNG.nonNegativeInt(nextRng1)
    val (n3, nextRng3) = RNG.nonNegativeInt(nextRng2)
    val (n4, nextRng4) = RNG.nonNegativeInt(nextRng3)
    val (n5, _) = RNG.nonNegativeInt(nextRng4)

    val (m1, nextRng11) = rng.nextInt
    val (m2, nextRng21) = nextRng11.nextInt
    val (m3, nextRng31) = nextRng21.nextInt
    val (m4, nextRng41) = nextRng31.nextInt
    val (m5, _) = nextRng41.nextInt

    val minValueToPositive: Int = -(Int.MinValue+1)
    val minValueToPositiveFail: Int = -Int.MinValue
    assert(minValueToPositive > 0)
    assert(minValueToPositiveFail < 0)

    assert(n1 == m1)
    assert(n2 == 1-m2)
    assert(n3 == 1-m3)
    assert(n4 == 1-m4)
    assert(n5 == m5)
  }

  "generating a random double" should "work" in {
    val rng = SimpleRNG(42)
    val (n1, rng1) = RNG.doubleOld(rng)
    val (n2, _) = RNG.doubleOld(rng1)

    assert (n1 >= 0)
    assert (n1 < 1)
    assert (n2 >= 0)
    assert (n2 < 1)
  }

  "ints" should "create a list of random integers with specified size" in {
    val ints = RNG.ints(10)(SimpleRNG(42))
    ints._1.length shouldBe 10

    ints._1.map(
      int => assert(int.isValidInt)
    )
  }

  "ints2" should "also create a list of random integers with specified size" in {
    val intsRand: Rand[List[Int]] = RNG.ints2(10)
    val ints = intsRand(SimpleRNG(42))
    ints._1.length shouldBe 10

    ints._1.map(
      int => assert(int.isValidInt)
    )
  }

  "unit" should "should not modify the rng" in {
    val rng = SimpleRNG(42)
    val (int, rng2) = RNG.unit(15)(rng)

    int shouldBe 15
    rng2 shouldBe rng
  }

  "int" should "generate random integers as in nextInt" in {
    val rng = SimpleRNG(42)
    val (i, r) = RNG.int(rng)
    (i,r) shouldEqual rng.nextInt
  }

  "double" should "generate random doubles as in doubleOld" in {
    val rng = SimpleRNG(42)
    val (d, r) = RNG.double(rng)
    (d,r) shouldEqual doubleOld(rng)
  }

  "map" should "map a Rand[Int] to a Rand[String]" in {
    val randInt = RNG.int
    val randString = RNG.map(randInt)(_.toString)
    randString(SimpleRNG(42))._1 shouldBe randInt(SimpleRNG(42))._1.toString
  }

  "nonNegativeEven" should "generate random non-negative even numbers" in {
    val rng = SimpleRNG(42)
    val (i, r) = RNG.nonNegativeEven(rng)
    val (i1, r1) = RNG.nonNegativeEven(r)
    val (i2, _) = RNG.nonNegativeEven(r1)
    i % 2 shouldBe 0
    i1 % 2 shouldBe 0
    i2 % 2 shouldBe 0
  }

  "map2" should "map a Rand[Int] and a Rand[Int] to a Rand[String]" in {
    val randInt: Rand[Int] = _ => SimpleRNG(42).nextInt
    val f: (Int, Int) => String = (a,b) => s"${a.toString} and ${b.toString}"
    val randString = map2(randInt, randInt)(f)
    val int42 = randInt(SimpleRNG(42))._1
    randString(SimpleRNG(42))._1 shouldBe s"$int42 and $int42"
  }

  "sequence" should "turn a list of Rands into a Rand of lists" in {
    // list of maps RNG -> (a, RNG) to map (RNG1, RNG2, RNG3) -> ((a1, a2, a3), RNG4)
    val rand37: Rand[Int] = _ => SimpleRNG(37).nextInt
    val rand42: Rand[Int] = _ => SimpleRNG(42).nextInt
    val rands: List[Rand[Int]] = List(rand37, rand42)
    val listRand: Rand[List[Int]] = sequence(rands)

    val testRng = SimpleRNG(1993)

    val (intsA, rngA) = listRand(testRng)
    val (intB1, rngB1) = rand37(testRng)
    val (intB2, rngB2) = rand42(rngB1)

    intsA shouldBe List(intB1, intB2)
    rngA shouldBe rngB2
  }

  "A candy dispenser with 10 coins and 5 candies" should "have 14 coins and 1 candy after 4 candies are bought" in {
    val machineBefore = Machine(locked = true, 5, 10)
    val actions = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val simulateCandyDispenser: State[Machine, (Int, Int)] = simulateMachine(actions)
    val ((candies, coins), machineAfter): ((Int, Int), Machine) = simulateCandyDispenser.run(machineBefore)

    print(machineAfter)
    candies shouldBe 1
    coins shouldBe 14
  }

}
