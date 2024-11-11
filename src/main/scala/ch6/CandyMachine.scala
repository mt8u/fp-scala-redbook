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
    )((i, s) =>
      State(m => (i, m) match
        case (_, Machine(_, 0, coins)) => ((coins, m.candies), m)
        case (Input.Coin, Machine(true, candies, coins)) => ((coins, candies), Machine(false, candies, coins))
        case (Input.Turn, Machine(false, candies, coins)) => ((coins + 1, candies - 1), Machine(true, candies, coins))
        case _ => ((m.coins, m.candies), m)
      )
    )
  // APPELER DEPUIS simulateMachine, EN UTILISANT sequence, modify et get 
  // private def update(input: Input, machine: Machine): Machine =
  //   (input, machine) match
  //       case (_, Machine(_, 0, _)) => machine 
  //       case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins)
  //       case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies, coins)
  //       case _ => machine 
    
    
  
  // private def unlockMachine(
  //     state: State[Machine, (Coins, Candies)]
  // ): State[Machine, (Coins, Candies)] =
  //   State{m => m.candies match
  //     case 0 => ((m.coins, m.candies), m)
  //     case _ => ((m.coins, m.candies), Machine(false, m.candies, m.coins))}

  // private def dispenseACandy(
  //     state: State[Machine, (Coins, Candies)]
  // ): State[Machine, (Coins, Candies)] =
  //   State(m =>
  //     m.locked match
  //       case false if m.candies > 0 =>
  //         ((m.coins + 1, m.candies - 1), Machine(true, m.candies, m.coins))
  //       case _ => ((m.coins, m.candies), m)
  //   )
