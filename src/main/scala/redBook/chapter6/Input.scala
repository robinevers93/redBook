package redBook.chapter6

import redBook.chapter6.State.sequence

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object candyDispenserStateUpdater {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(input => State((machine: Machine) => getNewState(input, machine)))).map(_.last)



  private def getNewState(input: Input, machine: Machine): ((Int, Int), Machine) = {
    val newMachine = input match {
      case Coin if machine.locked => Machine(locked = false, machine.candies, machine.coins + 1)
      case Turn if !machine.locked => Machine(locked = true, machine.candies - 1, machine.coins)
      case _ => machine
    }

    ((newMachine.candies, newMachine.coins), newMachine)
  }
}