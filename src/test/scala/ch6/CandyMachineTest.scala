package ch6

class simulate_candy_machine extends munit.FunSuite:

  test(
    "should returns O candy and zero coin when initial state has 0 candy and 0 coin"
  ):
    val machine = Machine(true, 0, 0)
    val finalState = simulateMachine(List(Input.Coin)).run(machine)
    assertEquals(finalState._1, (0, 0))

  test(
    "should returns 1 candy and 0 coin when initial state has 1 candy and 0 coin and no coin is inserted"
  ):
    val machine = Machine(true, 1, 0)
    val finalState = simulateMachine(List()).run(machine)
    assertEquals(finalState._1, (0, 1))

  test(
    "should unlocks the machine when initial state is locked with 1 candy and 0 coin, and a coin is inserted"
  ):
    val machine = Machine(true, 1, 0)
    val finalState = simulateMachine(List(Input.Coin)).run(machine)
    assertEquals(finalState, ((0, 1), Machine(false, 1, 0)))

  test(
    "should dispense a candy when the machine is unlocked with 1 candy and 0 coin, and the knob is turned"
  ):
    val machine = Machine(false, 1, 0)
    val finalState = simulateMachine(List(Input.Turn)).run(machine)
    assertEquals(finalState._1, (1, 0))
