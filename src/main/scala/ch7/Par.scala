package ch7

import java.util.concurrent.TimeUnit

def sum(ints: IndexedSeq[Int]): Int =
  if ints.size <= 1 then ints.headOption.getOrElse(0)
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    sum(l) + sum(r)

trait ExecutorService:
  def submit[A](a: Callable[A]): Future[A]

trait Callable[A]:
  def call: A

trait Future[A]:
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean

opaque type Par[A] = ExecutorService => Future[A]

extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  private case class MapFuture[A, B, C](
      fa: Future[A],
      fb: Future[B],
      f: (A, B) => C
  ) extends Future[C]:
    var done = false
    var cancelled = false

    def isDone = done

    def get: C =
      val c = f(fa.get, fb.get)
      done = true
      c

    def get(timeout: Long, units: TimeUnit): C =
      val timeoutNanos = units.toNanos(timeout)
      val start = System.nanoTime
      val a = fa.get(timeout, units)
      val end = System.nanoTime
      val duration = end - start
      val b = fb.get(timeout - duration, TimeUnit.NANOSECONDS)
      done = true
      f(a, b)

    def isCancelled = cancelled

    def cancel(evenIfRunning: Boolean): Boolean =
      cancelled = true
      true

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        MapFuture(futureA, futureB, f)

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) =
    parList.map(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List(): List[A]))((pa, pas) => pa.map2(pas)(_ :: _))

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(
        new Callable[A]:
          def call = a(es).get
      )

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork:
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val fas: List[Par[List[A]]] =
        as.map(asyncF(a => if f(a) then List(a) else Nil))
      sequence(fas).map(_.flatten)

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l).map2(sum(r))(_ + _)

  def reduce[A](
      seq: IndexedSeq[A],
      f: (A, A) => A,
      initialValue: A
  ): Par[A] =
    def loop(seq: IndexedSeq[A]): Par[A] =
      if seq.size == 1 then Par.unit(seq.head)
      else
        val (l, r) = seq.splitAt(seq.size / 2)
        loop(l).map2(loop(r))(f)
    loop(seq :+ initialValue)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints, Math.max, Int.MinValue)

  def countWords(paragraphs: List[String]): Par[Int] =
    parMap(paragraphs)(s => if s.isEmpty() then s.split(' ').length else 0)
      .map(_.sum)
