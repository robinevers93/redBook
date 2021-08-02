package redBook.Chapter3

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
    val h: (Tree[B], Tree[B]) => Tree[B] = (x, y) => Branch(x, y)

    fold(t)(g)(h)
  }

  def depthFromFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((x, y) => 1 + x.max(y))

  def maxFromFold(t: Tree[Int]): Int =
    fold(t)(x => x)((x, y) => x.max(y))

  def sizeFromFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x, y) => x + y + 1)
}