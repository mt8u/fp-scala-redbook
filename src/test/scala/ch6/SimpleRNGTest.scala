package ch6

class ch6_Suite extends munit.FunSuite:

  case class SimpleRNGMinInt(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      (Int.MinValue, SimpleRNGMinInt(seed))

  case class SimpleRNGZero(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      (0, SimpleRNGZero(seed))
      
  test("nonNegativeInt"):
    val rng: RNG = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    assertEquals(SimpleRNG.nonNegativeInt(rng2)._1, 1281479696)
    assertEquals(SimpleRNG.nonNegativeInt(SimpleRNGMinInt(42))._1, 2147483647)

  test("double"):
    val rng: RNG = SimpleRNG(42)
    assertEquals(SimpleRNG.double(SimpleRNGMinInt(0))._1, 0.9999999995343387)
    assertEquals(SimpleRNG.double(SimpleRNGZero(0))._1, 0.0)
    

  test("intDouble"):
    val rng: RNG = SimpleRNG(42)
    assertEquals(
      SimpleRNG.intDouble(SimpleRNGMinInt(0))._1, (-2147483648, 0.9999999995343387)
    )
    assertEquals(
      SimpleRNG.intDouble(SimpleRNGZero(0))._1, (0, 0.0)
    )

  test("doubleInt"):
    val rng: RNG = SimpleRNG(42)
    assertEquals(
      SimpleRNG.doubleInt(SimpleRNGMinInt(0))._1, (0.9999999995343387, -2147483648)
    )
    assertEquals(
      SimpleRNG.doubleInt(SimpleRNGZero(0))._1, (0.0, 0)
    )

  test("double3"):
    val rng: RNG = SimpleRNG(42)
    assertEquals(
      SimpleRNG.double3(SimpleRNGMinInt(0))._1,
      (0.9999999995343387,
        0.9999999995343387,
        0.9999999995343387)
    )
    assertEquals(
      SimpleRNG.double3(SimpleRNGZero(0))._1, (0.0, 0.0, 0.0)
    )
