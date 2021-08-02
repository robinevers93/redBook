package redBook.Chapter4

sealed trait Option[+A] {

  /** 4.1 implement map/flatmap/getOrElse/orElse/filter */
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None() => None()
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None() => None()
  }

  def getOrElse[B >: A](default: => B): B = this match { //type => B acts like def, evaluated lazily
    case Some(get) => get
    case None() => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None() => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case _ => None()
  }
}
case class Some[+A](get: A) extends Option[A]
case class None() extends Option[Nothing]

object Chapter4 {

  /** 4.2 implement variance */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.nonEmpty) Some(xs.foldLeft(0.0)((x, y) => x+y)/xs.size)
    else None()

  def variance(xs: Seq[Double]): Option[Double] = {
    val f: Double => Option[Double] = m => mean(xs.map(x => math.pow(x-m, 2)))
    mean(xs).flatMap(f)
  }

  /** 4.3 implement map2 */
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case _: Exception => None()}

  def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2asForComprehension[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  /**4.4 implement sequence*/
  def sequence[A](a:List[Option[A]]): Option[List[A]] = a match {
      case Some(h)::t => sequence(t).map(h :: _)
      case None()::_  => None()
      case Nil        => Some(Nil)
    }

  /**4.5 implement traverse*/
  def traverseSlow[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case h :: t => f(h) match {
      case Some(x) => traverse(t)(f).map(x :: _)
      case None() => None()
    }
    case Nil => Some(Nil)
  }

  def traverseUsingFold[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val z: Option[List[B]] = Some(List[B]())
    a.foldRight(z)((aa,b) => map2(f(aa), b)(_ :: _))
  }

  def sequenceUsingTraverse[A](a:List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}