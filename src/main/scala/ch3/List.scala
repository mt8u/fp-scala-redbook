package ch3;

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def sum(ints: List[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  // def tail[A](as: List[A]): List[A] = as match
  //   case Nil         => sys.error("list is empty")
  //   case Cons(_, xs) => xs

  // def setHead[A](as: List[A], newHead: A): List[A] =
  //   as match
  //     case Nil         => sys.error("list is empty")
  //     case Cons(_, xs) => Cons(newHead, xs)

  // def drop[A](as: List[A], n: Int): List[A] =
  //   if n <= 0 then as
  //   else
  //     as match
  //       case Nil         => as
  //       case Cons(_, xs) => drop(xs, n - 1)

  // def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
  //   as match
  //     case Nil         => Nil
  //     case Cons(x, xs) => if f(x) then dropWhile(xs, f) else as

  // def init[A](as: List[A]): List[A] =
  //   as match
  //     case Nil          => as
  //     case Cons(_, Nil) => Nil
  //     case Cons(x, xs)  => Cons(x, init(xs))

  /*
Implement the function tail for removing the first element of a List (note that the function takes constant time). You can use sys.error("message") to throw an exception if the List is Nil. In the next chapter, we’ll look at different ways of handling errors. Be careful to use the List enum and the Nil case defined here and not the built-in Scala List and Nil types.
   */

  def tail[A](as: List[A]): List[A] =
    as match
      case Nil        => sys.error("list is empty")
      case Cons(_, t) => t

  def setHead[A](h: A, as: List[A]): List[A] =
    as match
      case Nil        => sys.error("list is empty")
      case Cons(_, t) => Cons(h, t)

  def drop[A](as: List[A], n: Int): List[A] =
    if n == 0 then as
    else
      as match
        case Nil        => Nil
        case Cons(_, t) => drop(t, n - 1)

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => as

  def init[A](as: List[A]): List[A] =
    as match
      case Nil          => as
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _)


  def length[A](as: List[A]): Int =
    // @annotation.tailrec
    // def go(l: List[A], acc: Int): Int =
    //   l match
    //     case Nil => acc
    //     case Cons(_, t) => go(t, acc + 1)
    // go(as, 0)
    foldRight(as, 0, (_, acc) => acc + 1)

  // def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
  //   @annotation.tailrec
  //   def loop(as: List[A], acc: B): B =
  //     as match
  //       case Nil => acc
  //       case Cons(h, t) => loop(t, f(acc, h))
  //   loop(as, acc)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
      as match
        case Nil => acc
        case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b: B) => b, (g: B => B, current: A) => (b: B) => g(f(current, b)))(acc)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))
  // def append[A](a1: List[A], a2: List[A]): List[A] =
  //   foldLeft(reverse(a1), a2, Cons(cur, acc))

  def flat[A](ass: List[List[A]]): List[A] =
    foldRight(ass, Nil: List[A], append)

  def incrementBy1(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int], (n, acc) => Cons(n + 1, acc))

  def doubleToString(ns: List[Double]): List[String] =
    foldRight(ns, Nil: List[String], (d, acc) => Cons(d.toString, acc))

  // def map[A, B](as: List[A], f: A => B): List[B] =
    // ???

end List
