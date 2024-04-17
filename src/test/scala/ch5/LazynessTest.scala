package ch5

class ch5_Suite extends munit.FunSuite:

  val lazyList = LazyList(1, 2, 3)
  val emptyList: LazyList[Int] = LazyList.Empty

  test("toList"):
    assertEquals(lazyList.toList, List(1, 2, 3))
    assertEquals(LazyList.empty.toList, Nil)

  test("take"):
    assertEquals(lazyList.take(2).toList, LazyList(1, 2).toList)
    assertEquals(lazyList.take(5).toList, LazyList(1, 2, 3).toList)
    assertEquals(LazyList.Empty.take(2), LazyList.empty)

  test("drop"):
    assertEquals(lazyList.drop(2).toList, LazyList(3).toList)
    assertEquals(lazyList.drop(5), LazyList.empty)
    assertEquals(LazyList.Empty.drop(2), LazyList.empty)

  test("takeWhile"):
    assertEquals(LazyList.Empty.takeWhile(A => true), LazyList.empty)
    assertEquals(lazyList.takeWhile(a => a <= 2).toList, LazyList(1, 2).toList)

  test("forAll"):
    assertEquals(LazyList.Empty.forAll(A => true), true)
    assertEquals(
      lazyList.forAll(a => a <= 2),
      false
    )
    assertEquals(
      lazyList.forAll(a => a > 0),
      true
    )

  test("takeWhileWithFoldRight"):
    assertEquals(
      LazyList.Empty.takeWhileWithFoldRight(A => true),
      LazyList.empty
    )
    assertEquals(
      lazyList.takeWhileWithFoldRight(a => a <= 2).toList,
      LazyList(1, 2).toList
    )

  test("headOption"):
    assertEquals(
      LazyList.Empty.headOption,
      None
    )
    assertEquals(
      lazyList.headOption,
      Some(1)
    )

  test("map"):
    assertEquals(
      emptyList.map(_ + 10),
      LazyList.empty
    )
    assertEquals(
      lazyList.map(_.toString).toList,
      LazyList("1", "2", "3").toList
    )
