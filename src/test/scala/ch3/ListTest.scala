package ch3

import List.*

class ch3_1Suite extends munit.FunSuite:

  val list = List(1, 2, 3, 4, 5)

  test("pattern matching"):
    @annotation.nowarn
    val result = List(1, 2, 3, 4, 5) match
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    assertEquals(result, 3)

  test("tail"):
    assertEquals(tail(list), List(2, 3, 4, 5))
    assertEquals(tail(List(0, 1)), List(1))
    interceptMessage[java.lang.RuntimeException]("list is empty"):
      tail(Nil)

  test("set head"):
    // assertEquals()
    assertEquals(setHead(10, list), List(10, 2, 3, 4, 5))
    interceptMessage[java.lang.RuntimeException]("list is empty"):
      setHead(10, Nil)

  test("drop"):
    assertEquals(drop(list, 0), list)
    assertEquals(drop(Nil, 2), Nil)
    assertEquals(drop(list, 6), Nil)
    assertEquals(drop(list, 2), List(3, 4, 5))

  test("drop while"):
    assertEquals(dropWhile(Nil, (x: Int) => x < 3), Nil)
    assertEquals(dropWhile(list, (x: Int) =>  false), list)
    assertEquals(dropWhile(list, (x: Int) => true), Nil)
    assertEquals(dropWhile(list, _ < 3), List(3, 4, 5))

  test("init"):
    assertEquals(init(Nil), Nil)
    assertEquals(init(List(1)), Nil)
    assertEquals(init(List(1, 2)), List(1))
    assertEquals(init(list), List(1,2,3,4))

  test("fold right with Nil and Cons"):
    assertEquals(foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _)), List(1, 2, 3))

  test("length using fold right"):
    assertEquals(length(Nil), 0)
    assertEquals(length(List(1)), 1)
    assertEquals(length(list), 5)

  test("fold left"):
    assertEquals(foldLeft(List(1, 2, 3), 0, _ + _), 6)
    assertEquals(foldLeft(List(1, 2, 3), 1, _ * _), 6)

  test("reverse using fold left"):
    assertEquals(reverse(List(1, 2, 3)), List(3, 2, 1))

  test("fold right via fold left"):

  //  assertEquals(foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int], Cons(_, _)), List(1, 2, 3))
    assertEquals(foldRightViaFoldLeft(List(1, 2, 3), 0, _ + _), 6)
    //assertEquals(foldRightViaFoldLeft(List(1, 2, 3), 1, _ * _), 6)

  test("append"):
    assertEquals(append(List(1,2,3), List(4,5)), List(1,2,3,4,5))
    // assertEquals(Nil, List(4,5), List(4,5))

  test("flat"):
    assertEquals(flat(List(List(1,2), List(3, 4))), List(1,2,3,4))

  test("incrementBy1"):
    assertEquals(incrementBy1(List(1,2,3)), List(2,3,4))  

  test("doublesToString"):
    assertEquals(doubleToString(List(1d,2d,3.5)), List("1.0","2.0","3.5"))  

  // test("map"):
    // assertEquals(map(List(1, 2, 3), _ + 10), List(11, 12, 13))




// test("drop while"):
  //   assertEquals(dropWhile(Nil, _ => true), Nil)
  //   assertEquals(dropWhile(Nil, _ => false), Nil)
  //   assertEquals(dropWhile(list, _ => true), Nil)
  //   assertEquals(dropWhile(list, _ => false), list)
  //   assertEquals(dropWhile(list, n => n < 4), List(4, 5))

  // test("init"):
  //   assertEquals(init(Nil), Nil)
  //   assertEquals(init(list), List(1, 2, 3, 4))
