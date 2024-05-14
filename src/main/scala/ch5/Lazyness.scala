package ch5

import ch5.LazyList.{empty, unfold}

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  def take(n: Int): LazyList[A] =
    if n <= 0 then Empty
    else
      this match
        case Empty      => Empty
        case Cons(h, t) => Cons(h, () => t().take(n - 1))

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] =
    if n <= 0 then this
    else
      this match
        case Empty      => Empty
        case Cons(h, t) => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _                    => Empty

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _          => acc

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then LazyList.cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, b) => LazyList.cons(f(a), b))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def append[B >: A](l: => LazyList[B]): LazyList[B] =
    foldRight(l)((a, b) => LazyList.cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), (n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold((this)) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }

  def zipWith[B, C](that: LazyList[B], f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }


object LazyList:
  def cons[A](
      hd: => A,
      tl: => LazyList[A]
  ): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  def fibs(): LazyList[Int] =
    def loop(curr: Int, next: Int): LazyList[Int] =
      LazyList.cons(curr, loop(next, curr + next))
    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state)
      .map((a, s) => cons(a, unfold(s)(f)))
      .getOrElse(empty)

  def fibsViaUnfold(): LazyList[Int] = unfold((0,1))((c, n) => Some(c, (n, c + n)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(Some(a, _))

  val ones: LazyList[Int] = unfold(())(Some(1, _))


