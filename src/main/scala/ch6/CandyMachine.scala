package ch6

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  State(m =>
    inputs.foldRight(
      ((m.coins, m.candies), m): ((Int, Int), Machine)
    )((i, s) => {
      val ((coins, candies), mNext) = s
      i match
        case Input.Coin => ((coins, candies), Machine(false, candies, coins))
        case _ => s
    })
  )
