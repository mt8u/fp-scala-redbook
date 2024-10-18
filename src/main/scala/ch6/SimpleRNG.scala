package ch6

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, newState) = underlying(s)
        (f(a), newState)
	
    def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] = 
      s =>
        val (a, nextS) = underlying(s)
        val (b, nextNextS) = state(nextS)
        (f(a, b), nextNextS)

    def flatMap[B](f: A => State[S, B]): State[S, B] = 
      s =>
        val (a, nextS) = underlying(s)
        f(a)(nextS)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = 
    ss.foldRight(unit(List()): State[S, List[A]])((sl, sa) => sl.map2(sa)((a, l) => a::l))
    
  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f
    

type Rand[+A] = RNG => (A, RNG)
// type Rand[A] = State[RNG, A]

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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (n, rng1) = ra(rng)
      map(rb)(f(n, _))(rng1)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldLeft((List[A](), rng)) { case ((l, rgen), ra) =>
        val (a, rgen2) = ra(rgen)
        (l :+ a, rgen2)
      }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence[Int](List.fill(count)(_.nextInt))(rng)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](rA: Rand[A], rB: Rand[B])(
      f: (A, B) => C
  ): Rand[C] = flatMap(rA)(a => map(rB)(f(a, _)))
