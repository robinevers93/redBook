package redBook

import scala.annotation.tailrec


sealed trait List[+A] //+ means lists of subtype of A are subtypes of List[A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) //_* allows us to pass in a Seq

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, x) => x
  }

  def head[A](list: List[A]): A = list match {
    case Nil => throw new Exception
    case Cons(x, _) => x
  }

  def setHead[A](list: List[A], first: A): List[A] = {
    Cons(first, tail(list))
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n == 0) list
    else drop(tail(list), n-1)
  }

  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Cons(x, y) if f(x) => dropWhile(y, f)
    case _ => list
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }

  /** 3.6 Implement a function that returns a List considering of all but the last element of a List */
  def reverseInit[A](l: List[A]): List[A] = {
    //ends up reversed
    @tailrec
    def go(l: List[A], newList: List[A] = List()): List[A] = l match {
      case Cons(_, Nil) => newList
      case Cons(h,t) => go(Cons(h, newList), t)
    }
    go(l)
  }


  def init[A](l: List[A]): List[A] = {
    //inefficient because we need to copy all the previous Cons objects
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
    }
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], identity: B)(f: (A,B) => B): B = //f in its own argument group to maximise type inference
    as match {
      case Nil => identity
      case Cons(x, xs) => f(x, foldRight(xs, identity)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x+y) // on List(1,2,3) this is 1+(2+(3+0))

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)((x,y) => x*y)

  /**3.9 Compute the length of a list using foldRight.*/
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_,x) => x + 1)
  }

  /**3.10 Write a tail-recursive foldLeft function.*/
  @tailrec
  def foldLeft[A,B](as: List[A], identity: B)(f: (B,A) => B): B = {
    as match {
      case Nil => identity
      case Cons(x, xs) => foldLeft(xs, f(identity, x))(f)
    }
  }

  /**3.11 Write sum, product and length functions using foldLeft*/
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _) // on List(1,2,3) this is ((0+1)+2)+3

  def product3(l: List[Double]): Double =
    foldRight(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = {
    foldRight(l, 0)((_,x) => x + 1)
  }

  /**3.12 Write a function that returns the reverse of a List using a fold*/
  def reverse[A](l: List[A]): List[A] ={
    foldLeft(l, List(): List[A])((x, y) => Cons(y, x))
  }

  /**3.13 (hard) Write foldLeft in terms of foldRight and foldRight in terms of foldLeft*/
  def foldLeft2[A,B](l: List[A], identity: B)(f: (B,A) => B): B = {
    foldRight(reverse(l), identity)((a,b) => f(b,a))
  }

  def foldRight2[A,B](l: List[A], identity: B)(f: (A,B) => B): B = {
    foldLeft(reverse(l), identity)((b,a) => f(a,b))
  }

  /**3.14 Implement append in terms of either foldLeft or foldRight*/
  def append2[A](appendingList: List[A], startingList: List[A]): List[A] = {
    foldLeft(reverse(appendingList), startingList)((list, newHead) => Cons(newHead, list))
  }

  def append3[A](appendingList: List[A], startingList: List[A]): List[A] = {
    foldRight(appendingList, startingList)((newHead, list) => Cons(newHead, list))
  }

  /**3.15 (hard) Write a function that concatenates a list of lists into a single list*/
  def concat[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, Nil: List[A])(append)
  }



}