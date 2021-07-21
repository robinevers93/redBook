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

  /**3.16 Write a function that transforms a list of integers by adding 1 to each element*/
  def addOne(list: List[Int]): List[Int] = {
    val f = (x: Int, y: List[Int]) => Cons(x+1, y)
    foldRight(list, List[Int]())(f)
  }

  /**3.17 Write a function that transforms a list of doubles into a list of strings*/
  def listToString(list: List[Double]): List[String] = {
    val f = (x: Double, y: List[String]) => Cons(x.toString, y)
    foldRight(list, List[String]())(f)
  }

  /**3.18 Write a function for map*/
  def map[A,B](list: List[A])(f: A => B): List[B] = {
    val g = (x: A, y: List[B]) => Cons(f(x), y)
    foldRight(list, List[B]())(g)
  }

  /**3.19 Write a function for filter*/
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val g = (x:A, y: List[A]) => if (f(x)) Cons(x, y) else y
    foldRight(as, List[A]())(g)
  }

  /**3.20 Write a function for flatmap*/
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  /**3.21 Use flatmap to implement filter*/
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    val g = (x: A) => if (f(x)) List(x) else List()
    flatMap(as)(g)
  }

  /**3.22 Write a function that constructs a new list by adding entries of indexes*/
  def addLists(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case(Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2,t2)) => Cons(h1+h2, addLists(t1, t2))
  }

  /**3.23 Write a function that constructs a new list by adding entries of indexes*/
  def zipWith[A](list1: List[A], list2: List[A])(f: (A,A) => A): List[A] = (list1, list2) match {
    case(Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**3.24 (hard) Write a function that returns true if and only if a subsequence is found in a list*/
  @tailrec
  def startsWith[A](list: List[A], startList: List[A]): Boolean = (list, startList) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) startsWith(t1, t2) else false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(_,t1), _) => if (startsWith(sup, sub)) true else hasSubsequence(t1, sub)
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    /** 3.25 Write a function size that counts the number of leaves and branches in a tree */
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    /** 3.26 Write a function maximum that maximum element in a tree */
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

    /** 3.27 Write a function depth that counts the maximum path length from the root to a leaf in a tree */
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    /** 3.28 Write a map function for trees */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    /** 3.29 Write a function fold that generalises the map/depth/max/size functions for a tree */
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(x) => f(x)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    def mapFromFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
      val g: A => Tree[B] = x => Leaf(f(x))
      val h: (Tree[B], Tree[B]) => Tree[B] = (x, y) => Branch(x,y)

      fold(t)(g)(h)
    }

    def depthFromFold[A](t: Tree[A]): Int =
      fold(t)(_ => 0)((x, y) => 1 + x.max(y))

    def maxFromFold(t: Tree[Int]): Int =
      fold(t)(x => x)((x, y) => x.max(y))

    def sizeFromFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)((x, y) => x+y+1)
  }