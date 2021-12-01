package redBook.chapter6


import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)
  type Rand2[+A] = State[RNG, A]
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, nextRNG) if n < 0 => (-(n+1), nextRNG)
    case _ => rng.nextInt
  }

  def doubleOld(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, nextRNG) => (n/(Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, ints: List[Int]): (List[Int], RNG) = {
      if (count == 0) (ints, rng)
      else {
        val (n, rngNew) = rng.nextInt
        go(count-1, rngNew, n :: ints)
      }
    }
    go(count, rng, List())
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  def unit[A](a:A): Rand[A] =
    rng => (a, rng)

  val int: Rand[Int] = _.nextInt
  val double: Rand[Double] = r => {
    val (i,r1) = nonNegativeInt(r)
    (i/(Int.MaxValue.toDouble+1), r1)
    }
  val nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i=> if (i % 2 == 0) i else i-1)
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](as: List[Rand[A]]): Rand[List[A]] = {
    val z: Rand[List[A]] = unit(List[A]())
    val f: (Rand[A], Rand[List[A]]) => Rand[List[A]] = (a, acc) => map2(a, acc)(_ :: _)
    as.foldRight(z)(f)
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def flatMap[A,B](f:Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ a =>
    val mod: Int = a % n
    if (a + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

}