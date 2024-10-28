package ch6

class ch6_Suite_2 extends munit.FunSuite:

  test("simulateMachine"):
    val machine = Machine(true, 0, 0)
    val finalState = simulateMachine(List(Input.Coin)).run(machine)
    assertEquals(finalState, ((0, 0), Machine(true, 0, 0)))
