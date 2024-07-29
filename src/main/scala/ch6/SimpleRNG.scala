package ch6

type Rand[+A] = RNG => (A, RNG)

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object SimpleRNG:
  // Implémentation fausse : pas random car
  // nonNegativeInt(Int.MinValue) == nonNegativeInt(Int.MinValue + 1)
  def nonNegativeInt_fausse_implem(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    n match
      case Int.MinValue => (Int.MaxValue, rng2)
      case _: Int       => (Math.abs(n), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)
    
  // Implémentation fausse car elle utilise la précédente
  def double_fausse_implem(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    ((-n.toDouble / Int.MinValue), rng2)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
    
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (n, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((n, d), rng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((n, d), rng1) = intDouble(rng)
    ((d, n), rng1)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    Range(0, count).foldLeft((List.empty[Int], rng)) {
      case ((lst, currRng), _) =>
        val (nextInt, nextRng) = currRng.nextInt
        (lst :+ nextInt, nextRng)
    }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def doubleViaMap: Rand[Double] = 
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
