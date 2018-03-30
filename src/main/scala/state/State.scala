package state

case class State[S, +A](run: S => (A, S)) {

  import State._
  //[6.10] Generalize the functions unit, map, map2, flatMap, and sequence.
  //Add them as methods on the State case class where possible. Otherwise you should put them in a State companion object.

  def map[B](f: A => B): State[S, B] = flatMap(f.andThen(unit))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List.empty[A]))((st, acc) => st.map2(acc)(_ :: _))


  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}


//[6.11] Hard: To gain experience with the use of State, implement a finite state automaton that models a simple candy dispenser.
//The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
//It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many coins it contains.
/*
The rules of the machine are as follows:
  - Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  - A machine that’s out of candy ignores all inputs.
 */

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  import State._

  def update(input: Input): Machine => Machine =
    m => (m, input) match {
      case (Machine(true, cnds, cns), Coin) if cnds > 0 => Machine(false, cnds, cns + 1)
      case (Machine(false, cnds, cns), Turn) => Machine(true, cnds - 1, cns)
      case _ => m
    }

  /*
  The method simulateMachine should operate the machine based on the list of inputs
  and return the number of coins and candies left in the machine at the end.
  For example, if the input Machine has 10 coins and 5 candies,
  and a total of 4 candies are successfully bought, the output should be (14, 1).
   */

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val listOfUpdateActions: List[State[Machine, Unit]] =
      inputs.map(input => {
        val updateAction: Machine => Machine = update(input)
        modify[Machine](updateAction)
      })

    for {
      _ <- sequence(listOfUpdateActions) // State[Machine, List[Unit]]
      s <- get
    } yield (s.coins, s.candies)
  }

}