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

  test("filter"):
    assertEquals(
      LazyList.Empty.filter(A => true),
      LazyList.empty
    )
    assertEquals(
      lazyList.filter(a => a <= 2).toList,
      LazyList(1, 2).toList
    )

  test("append"):
    assertEquals(
      emptyList.append(LazyList(1, 2)).toList,
      LazyList(1, 2).toList
    )
    assertEquals(
      lazyList.append(emptyList).toList,
      LazyList(1, 2, 3).toList
    )
    assertEquals(
      lazyList.append(LazyList(-1, -2)).toList,
      LazyList(1, 2, 3, -1, -2).toList
    )

  test("flatMap"):
    assertEquals(
      emptyList.flatMap(a => LazyList(a + 10)),
      LazyList.empty
    )
    assertEquals(
      lazyList.flatMap(a => LazyList(a + 10)).toList,
      LazyList(11, 12, 13).toList
    )
    assertEquals(
      lazyList.flatMap(a => LazyList.empty),
      LazyList.empty
    )
    assertEquals(
      lazyList.flatMap(a => LazyList(a + 10, a + 20)).toList,
      LazyList(11, 21, 12, 22, 13, 23).toList
    )

  test("continually"):
    assertEquals(
      LazyList.continually(5).take(5).toList,
      LazyList(5, 5, 5, 5, 5).toList
    )

  test("from"):
    assertEquals(
      LazyList.from(3).take(5).toList,
      LazyList(3, 4, 5, 6, 7).toList
    )

  test("fibs"):
    assertEquals(
      LazyList.fibs().take(7).toList,
      LazyList(0, 1, 1, 2, 3, 5, 8).toList
    )

  test("unfold"):
    val f = (n: Int) => if n < 4 then Some(n.toString, n + 1) else None
    assertEquals(
      LazyList.unfold(0)(f).toList,
      LazyList("0", "1", "2", "3").toList
    )

  test("fibsViaUnfold"):
    assertEquals(
      LazyList.fibsViaUnfold().take(7).toList,
      LazyList(0, 1, 1, 2, 3, 5, 8).toList
    )

  test("fromViaUnfold"):
    assertEquals(
      LazyList.fromViaUnfold(3).take(5).toList,
      LazyList(3, 4, 5, 6, 7).toList
    )

  test("continuallyViaUnfold"):
    assertEquals(
      LazyList.continuallyViaUnfold(5).take(5).toList,
      LazyList(5, 5, 5, 5, 5).toList
    )

  test("ones"):
    assertEquals(
      LazyList.ones.take(5).toList,
      LazyList(1, 1, 1, 1, 1).toList
    )

  test("mapViaUnfold"):
    assertEquals(
      lazyList.mapViaUnfold(_.toString).toList,
      LazyList("1", "2", "3").toList
    )
    assertEquals(
      emptyList.mapViaUnfold(_.toString).toList,
      Nil
    )
    assertEquals(
      LazyList(100, 2, 3, 10, 11, -11).mapViaUnfold(_.toString).toList,
      LazyList("100", "2", "3", "10", "11", "-11").toList
    )

  test("takeViaUnfold"):
    assertEquals(lazyList.takeViaUnfold(2).toList, LazyList(1, 2).toList)
    assertEquals(lazyList.takeViaUnfold(5).toList, LazyList(1, 2, 3).toList)
    assertEquals(LazyList.Empty.takeViaUnfold(2), LazyList.empty)

  test("takeWhileViaUnfold"):
    assertEquals(LazyList.Empty.takeWhileViaUnfold(A => true), LazyList.empty)
    assertEquals(
      lazyList.takeWhileViaUnfold(a => a <= 2).toList,
      LazyList(1, 2).toList
    )

  test("zipAll"):
    assertEquals(LazyList.Empty.zipAll(LazyList(1, 2, 3)), LazyList.empty)
    assertEquals(
      lazyList.zipAll(LazyList(3, 4, 6)).toList,
      LazyList(
        (Some(1), Some(3)),
        (Some(2), Some(4)),
        (Some(3), Some(6))
      ).toList
    )

  test("zipWith"):
    assertEquals(
      LazyList.Empty.zipWith(LazyList(1, 2, 3), (a: Int, b: Int) => a + b),
      LazyList.empty
    )
    assertEquals(
      lazyList.zipWith(LazyList(3, 4, 6), (a: Int, b: Int) => a + b).toList,
      LazyList(4, 6, 9).toList
    )

  test("startsWith"):
    assertEquals(
      LazyList.Empty.startsWith(LazyList(1, 2)),
      false
    )
    assertEquals(
      lazyList.startsWith(LazyList(1, 2)),
      true
    )
    assertEquals(
      lazyList.startsWith(LazyList(1, 1)),
      false
    )
    assertEquals(
      lazyList.startsWith(LazyList(1, 2, 3, 4)),
      false
    )