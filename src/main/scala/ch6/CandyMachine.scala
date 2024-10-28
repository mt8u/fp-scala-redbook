package ch6

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  State(m => ((0, 0), m))
