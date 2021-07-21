package redBook

trait Option[+A] {

  /** 4.1 implement map/flatmap/getOrElse/orElse/filter */

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None() => None()
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None() => None()
  }

  def getOrElse[B >: A](default: => B): B = this match {  //type => B acts like def, evaluated lazily
    case Some(get) => get
    case None() => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None() => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get)  => this
    case _ => None()
  }
}

case class Some[+A](get: A) extends Option[A]
case class None[+A]() extends Option[Nothing]

object Chapter4 {

  /** 4.2 implement variance */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.nonEmpty) Some(xs.foldLeft(0.0)((x, y) => x+y)/xs.size)
    else None()

  def variance(xs: Seq[Double]): Option[Double] = {
    val f: Double => Option[Double] = m => mean(xs.map(x => math.pow(x-m, 2)))
    mean(xs).flatMap(f)
  }
}