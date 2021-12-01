package redBook.chapter7

import redBook.chapter7.Par.Par

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit, Future}

sealed trait NonBlockingFuture[A] { //non-blocking Future
  private[chapter7] def apply(cb: A => Unit): Unit //cb is the callback (continuation)
}

object NewPar{
  type NewPar[A] = ExecutorService => NonBlockingFuture[A]

  def run[A](es: ExecutorService)(p: NewPar[A]): A = {
    val ref = new AtomicReference[A] //this is mutable, but thread-safe
    val latch = new CountDownLatch(1) //allows threads to wait until countDown method is called a certain number of times
    p(es) {a => ref.set(a); latch.countDown()} //when value is received, set result and release latch
    latch.await() //wait until latch is released
    ref.get //return value of ref
  }

  def unit[A](a: A): NewPar[A] =
    _ => new NonBlockingFuture[A] {
      def apply(cb: A => Unit): Unit =
        cb(a) //value of type A is immediately available, so callback can be called immediately, with this value
    }

  def fork[A](a: => NewPar[A]): NewPar[A] =
    es => new NonBlockingFuture[A] {
      def apply(cb: A => Unit): Unit = //a(es) is a future of A, by applying it to cb, we evaluate a, since that itself is a callable, this is done asynchronously to prevent deadlocking
        eval(es)(a(es)(cb)) //eval forks off the evaluation of a, and returns immediately, once evaluated, Future[A] is passed to the callback
    }

  def eval(es: ExecutorService)(r: => Unit): Unit = //evaluates action r (which returns a unit) asynchronously
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })

}

object Par{
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def map2[A,B,C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = (s: ExecutorService) => {
    val af = a(s)
    val bf = b(s)
    Map2Future(af, bf, f)
  }
  def fork[A](a: => Par[A]): Par[A] =
    (s: ExecutorService) => s.submit(new Callable[A] {
      def call: A = a(s).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit())((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    val z: Par[List[A]] = Par.unit(List[A]())
    val op: (Par[A], Par[List[A]]) => Par[List[A]] =
      (pa, pas) => map2(pa, pas)(_::_)
    ps.foldRight(z)(op)
  }

  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val bs: List[Par[B]] = as.map(asyncF(f))
    sequence(bs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    sequence(as.foldRight(List[Par[A]]())((a, pas) => if(f(a)) unit(a)::pas else pas))

  def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val filteredAs = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(filteredAs))(_.flatten)
  }

  private case class UnitFuture[A](get: A) extends Future[A]{
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  private case class Map2Future[A,B,C](a: Future[A], b: Future[B], f:(A,B) => C) extends Future[C] {
    var c: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)
    override def isCancelled: Boolean =
      a.isCancelled || b.isCancelled
    override def isDone: Boolean =
      c.isDefined

    override def get(): C = c match {
      case Some(value) => value
      case None =>
        val cr = f(a.get, b.get)
        c = Some(cr)
        cr
    }
    override def get(timeout: Long, unit: TimeUnit): C = c match {
      case Some(value) => value
      case None =>
        val tic = System.currentTimeMillis
        val ar = a.get(timeout, unit)
        val toc = System.currentTimeMillis
        val timeA = toc-tic
        val br = b.get(timeout-timeA, unit)
        val cr = f(ar, br)
        c = Some(cr)
        cr
    }
  }
}

object parallelSum {

  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a,b) => a+b)

  def sumDAC(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

  def sumDACPar(s: ExecutorService)(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.run(s)(Par.unit(sumDACPar(s)(l))).get + Par.run(s)(Par.unit(sumDACPar(s)(r))).get
    }

  def sumDACPar2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sumDACPar2(l), sumDACPar2(r))(_+_)
    }

  def sumDacForked(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sumDacForked(l)), Par.fork(sumDacForked(r)))(_+_)
    }

}
