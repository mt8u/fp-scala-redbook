package ch6

class ch6_Suite extends munit.FunSuite:

  case class SimpleRNGMinInt(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      (Int.MinValue, nextRNG)

  case class SimpleRNGZero(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = 1// (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      (0, nextRNG)
      
  test("nonNegativeInt"):
    val rng: RNG = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    assertEquals(n2, -1281479697)
    assertEquals(SimpleRNG.nonNegativeInt(rng2)._1, 1281479697)
    assertEquals(SimpleRNG.nonNegativeInt(SimpleRNGMinInt(42))._1, 2147483647)

  test("double"):
    val rng: RNG = SimpleRNG(42)
    assertEquals(SimpleRNG.double(SimpleRNGMinInt(0))._1, 0.9999999995343387)
    assertEquals(SimpleRNG.double(SimpleRNGZero(0))._1, 0.0)
    
