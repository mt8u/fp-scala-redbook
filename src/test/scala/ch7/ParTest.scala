package ch7
import scala.concurrent.ExecutionContext
import java.util.concurrent.TimeUnit

case class UnitFuture[A](get: A) extends Future[A]:
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false

class DummyExecutorService extends ExecutorService:
  def submit[A](a: Callable[A]): Future[A] =
    UnitFuture(a.call)

class ch7_Suite extends munit.FunSuite:

  test("sum"):
    assertEquals(sum(Vector(1, 2, 3)), 6)

  test("par sum"):
    val par = Par.sum(Vector(1, 2, 3))
    assertEquals(
      par.run(DummyExecutorService()).get,
      6
    )

  test("par reduce"):
    val par = Par.reduce(Vector(1, 2, 3), _ + _, 10)
    assertEquals(
      par.run(DummyExecutorService()).get,
      16
    )

  test("max"):
    val par = Par.max(Vector(1, 2, 4, 3))
    assertEquals(
      par.run(DummyExecutorService()).get,
      4
    )

  test("countWords"):
    val par =
      Par.countWords(
        List("je m'appelle", "gautier", "", "j'aime pas", "l'archi hexa")
      )
    assertEquals(
      par.run(DummyExecutorService()).get,
      7
    )
