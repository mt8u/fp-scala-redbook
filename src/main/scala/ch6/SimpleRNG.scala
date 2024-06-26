package ch6

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object SimpleRNG:
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    n match
      case Int.MinValue => (Int.MaxValue, rng2)
      case _: Int       => (Math.abs(n), rng2)
