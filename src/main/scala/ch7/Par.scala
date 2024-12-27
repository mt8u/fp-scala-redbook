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
    
  private case class MapFuture[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C]:
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
 
  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
    
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
