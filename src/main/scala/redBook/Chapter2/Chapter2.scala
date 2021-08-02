package redBook.Chapter2

import scala.annotation.tailrec

object Chapter2 {

  @tailrec
  private def go(n: Int, m: Int, accA: Int = 1, accB: Int = 1): Int =
    if (m == n) accA + accB else go(n, m + 1, accB, accA + accB)

  def fib(n: Int): Int = n match {
    case 0 => 1
    case 1 => 1
    case _ => go(n, 2)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  @tailrec
  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.isEmpty || as.length == 1) true
    else if (ordered(as.head, as.tail.head)) isSorted2(as.tail, ordered)
    else false
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
