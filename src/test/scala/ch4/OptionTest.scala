package ch4

class ch4_Suite extends munit.FunSuite:


  val none = Option.None
  val some = Option.Some(1.0)

  test("map"):
    assertEquals(none.map(_.toString), Option.None)
    assertEquals(some.map(_.toString), Option.Some("1.0"))
    
  test("getOrElse"):
    assertEquals(none.getOrElse(0.0), 0.0)
    assertEquals(some.getOrElse(0.0), 1.0)

  test("flatMap"):
    assertEquals(none.flatMap(d => Option.Some(d.toString)), Option.None)
    assertEquals(some.flatMap(d => Option.Some(d.toString)), Option.Some("1.0"))
