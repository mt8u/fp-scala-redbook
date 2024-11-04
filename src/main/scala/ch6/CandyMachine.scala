package ch6

enum Input:
  case Coin, Turn

type Candies = Int
type Coins = Int

case class Machine(locked: Boolean, candies: Candies, coins: Coins)

def simulateMachine(inputs: List[Input]): State[Machine, (Coins, Candies)] =
  State(m =>
    inputs.foldRight(
      ((m.coins, m.candies), m): ((Coins, Candies), Machine)
    )((i, s) => {
      i match
        case Input.Coin => unlockMachine(s)
        case Input.Turn => dispenseACandy(s)
    })
  )

def unlockMachine(
    state: ((Coins, Candies), Machine)
): ((Coins, Candies), Machine) =
  val ((coins, candies), mNext) = state
  ((coins, candies), Machine(false, candies, coins))

def dispenseACandy(
    state: ((Coins, Candies), Machine)
): ((Coins, Candies), Machine) =
  val ((coins, candies), mNext) = state
  mNext.locked match
    case false => ((coins + 1, candies - 1), Machine(false, candies, coins))
    case true  => state
