package ch6

class simulate_candy_machine extends munit.FunSuite:

  test(
    "should returns O candy and zero coin when initial state has 0 candy and 0 coin"
  ):
    val machine = Machine(true, 0, 0)
    val finalState = machine.simulateMachine(List(Input.Coin)).run(machine)
    assertEquals(finalState._1, (0, 0))

  test(
    "should returns 1 candy and 0 coin when initial state has 1 candy and 0 coin and no coin is inserted"
  ):
    val machine = Machine(true, 1, 0)
    val finalState = machine.simulateMachine(List()).run(machine)
    assertEquals(finalState._1, (0, 1))

  test(
    "should unlocks the machine when initial state is locked with 1 candy and 0 coin, and a coin is inserted"
  ):
    val machine = Machine(true, 1, 0)
    val finalState = machine.simulateMachine(List(Input.Coin)).run(machine)
    assertEquals(finalState, ((0, 1), Machine(false, 1, 0)))

  test(
    "should dispense a candy when the machine is unlocked with 1 candy and 0 coin, and the knob is turned"
  ):
    val machine = Machine(false, 1, 0)
    val finalState = machine.simulateMachine(List(Input.Turn)).run(machine)
    assertEquals(finalState._1, (1, 0))
    
  test(
    "should lock the machine when a candy is dispensed"
  ):
    val machine = Machine(false, 1, 0)
    val finalState = machine.simulateMachine(List(Input.Turn)).run(machine)
    assertEquals(finalState._2.locked, true)

  test("Turning the knob on a locked machine does nothing"):
    val machine = Machine(true, 10, 5)
    val finalState = machine.simulateMachine(List(Input.Turn, Input.Turn)).run(machine)
    assertEquals(finalState, ((5, 10), Machine(true, 10, 5)))

  test("inserting a coin into an unlocked  does nothing"):
    val machine = Machine(false, 10, 5)
    val finalState = machine.simulateMachine(List(Input.Coin, Input.Coin)).run(machine)
    assertEquals(finalState, ((5, 10), Machine(false, 10, 5)))

    
  test("A machine that’s out of candy ignores all inputs"):
    val machines = List(Machine(false, 0, 5), Machine(true, 0, 5))
    val finalStates = machines.map(m => m.simulateMachine(List(Input.Coin, Input.Turn, Input.Coin)).run(m))
    assertEquals(finalStates, List(((5, 0), Machine(false, 0, 5)), ((5, 0), Machine(true, 0, 5))))

  test("A machine that’s out of candy ignores all inputs 2"):
    val machines = List(Machine(false, 0, 5), Machine(true, 0, 5))
    val finalStates = machines.map(m => m.simulateMachine(List(Input.Turn, Input.Coin)).run(m))
    assertEquals(finalStates, List(((5, 0), Machine(false, 0, 5)), ((5, 0), Machine(true, 0, 5))))
