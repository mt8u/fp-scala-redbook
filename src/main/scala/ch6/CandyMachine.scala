package ch6

import ch6.State.unit

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

def unlockMachine(
    state: State[Machine, (Coins, Candies)]
): State[Machine, (Coins, Candies)] =
  State(m => ((m.coins, m.candies), Machine(false, m.candies, m.coins)))

def dispenseACandy(
    state: State[Machine, (Coins, Candies)]
): State[Machine, (Coins, Candies)] =
State(
  m =>
    m.locked match
    case false => ((m.coins + 1, m.candies - 1), Machine(false, m.candies, m.coins))
    case true  => ((m.coins, m.candies), m)
)
