package ch6

import ch6.State.{unit, modify, get, set}

enum Input:
  case Coin, Turn

type Candies = Int
type Coins = Int

case class Machine(locked: Boolean, candies: Candies, coins: Coins):

  def simulateMachine(inputs: List[Input]): State[Machine, (Coins, Candies)] =
    inputs.foldRight(
      unit((coins, candies)): State[Machine, (Coins, Candies)]
    )((i, s) => {
      i match
        case Input.Coin => unlockMachine(s)
        case Input.Turn => dispenseACandy(s)
    })

  private def unlockMachine(
      state: State[Machine, (Coins, Candies)]
  ): State[Machine, (Coins, Candies)] =
    State{m => m.candies match
      case 0 => ((m.coins, m.candies), m)
      case _ => ((m.coins, m.candies), Machine(false, m.candies, m.coins))}

  private def dispenseACandy(
      state: State[Machine, (Coins, Candies)]
  ): State[Machine, (Coins, Candies)] =
    State(m =>
      m.locked match
        case false =>
          ((m.coins + 1, m.candies - 1), Machine(true, m.candies, m.coins))
        case true => ((m.coins, m.candies), m)
    )
