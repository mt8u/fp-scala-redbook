package ch5

class ch5_Suite extends munit.FunSuite:

  val lazyList = LazyList(1, 2, 3)

  test("toList"):
    assertEquals(lazyList.toList, List(1, 2, 3))
    assertEquals(LazyList.empty.toList, Nil)

  test("take"):
    assertEquals(lazyList.take(2).toList, LazyList(1, 2).toList)
    assertEquals(lazyList.take(5).toList, LazyList(1, 2, 3).toList)
    // assertEquals(LazyList.empty.toList, LazyList.empty.toList)

  test("drop"):
    assertEquals(lazyList.drop(2).toList, LazyList(3).toList)
    assertEquals(lazyList.drop(5).toList, LazyList.empty.toList)
    // assertEquals(LazyList.empty.toList, LazyList.empty.toList)
