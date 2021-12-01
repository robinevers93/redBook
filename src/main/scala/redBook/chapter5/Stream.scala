package redBook.chapter5

import redBook.chapter5.Stream.{cons, empty, unfold}

sealed trait Stream[+A]{

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1)) //h() and t() are not actually evaluated here, because cons() takes its arguments lazily
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = {
    def f: (A, => Boolean) => Boolean = (a,b) => p(a) || b
    foldRight(false)(f)
  }

  def forAll(p: A => Boolean): Boolean = {
    def f: (A, => Boolean) => Boolean = (a,b) => p(a) && b
    foldRight(true)(f)
  }

  def takeWhileAsFold(p: A => Boolean): Stream[A] = {
    def f: (A, => Stream[A]) => Stream[A] = (h,t) => if (p(h)) cons(h, t) else empty
    foldRight(empty: Stream[A])(f)
  }

  def headOptionAsFold: Option[A] = {
    def f : (A, => Option[A]) => Option[A] = (a,_) => Some(a)
    foldRight(None: Option[A])(f)
  }

  def map[B](f: A => B): Stream[B] = {
    def p: (A, => Stream[B]) => Stream[B] = (a, bs) => cons(f(a), bs)
    foldRight(empty: Stream[B])(p)
  }

  def filter(p: A => Boolean): Stream[A] = {
    def f: (A, => Stream[A]) => Stream[A] = (a, as) => if (p(a)) cons(a, as) else as
    foldRight(empty[A])(f)
  }

  def append[B >: A](b: => Stream[B]): Stream[B] = {
    def f: (A, => Stream[B]) => Stream[B] = (a, as) => cons(a, as)
    foldRight(b)(f)
  }

  def flatmap[B](f: A => Stream[B]): Stream[B] = {
    def p: (A, => Stream[B]) => Stream[B] = (a, bs) => f(a).append(bs)
    foldRight(empty: Stream[B])(p)
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)){
    case (Cons(h,t), m) if m > 1 => Some(h(), (t(), m-1))
    case (Cons(h,_), 1) => Some(h(), (empty, 0))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, bs)){
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zip[B](bs: Stream[B]): Stream[(A,B)] = //makes fast fibonacci easier
    zipWith(bs)((_,_))

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, bs){
    case(Empty, Empty) => None
    case(Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case(Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case(Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  def startsWith[A](as: Stream[A]): Boolean =
    zipAll(as).takeWhile(_._2.isDefined).forAll {
      case (h1, h2) => h1 == h2
    }

  def tails: Stream[Stream[A]] = unfold(this){
    case Empty => None
    case Cons(h, t) => Some(cons(h(),t()), t())
  }.append(empty)

  def scanRight[B](z:B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val bEvaluated = b
      val mappedValue = f(a, bEvaluated._1)
      (mappedValue, cons(mappedValue, bEvaluated._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] //case class parameters not allowed to be call-by-name i.e. have lazy arguments

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd //caching to avoid re-computation
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*)) //_* to pass each element of the tail as its own argument

  def slowConstant[A](a: A): Stream[A] =
    cons(a, slowConstant(a)) //not very efficient as slowConstant(a) is re-evaluated each time

  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = cons(a, stream) //more efficient as we only evaluate the tail once
    stream
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def unfoldFibs: Stream[Int] = unfold((0,1)){ case (s1, s2) => Some((s1, (s2, s1 + s2)))}

  def unfoldFrom(n: Int): Stream[Int] = unfold(n)(m => Some(m, m+1))

  def unfoldConstant[A](a:A): Stream[A] = unfold(a)(_ => Some(a, a))

  def unfoldOnes: Stream[Int] = unfold(1)(_ => Some(1,1))

  def fastFibs: Stream[Int] = {
    Stream(0,1).append(fastFibs.zip(fastFibs.drop(1)).map(x => x._1 + x._2))
  }

}
