/*
cats.data.State
  - to pass additional state around as a part of computation
  - State instances reprenest atomic state operations
  - to model mutable state in a functional way
 */

/*
State[S,A] represent functinos of type S => (S, A)
  - S: type of state
  - A: type of result
*/
import cats.data.State

val a = State[Int, String] { state =>
  (state, s"The state is $state")
}

/*
State instance is a function that does
  1. transforms an input state to an output state
  2. computes a result

Run monad with an initial state, returns Eval
  - run
  - runS
  - runA
 */
val (state, result) = a.run(10).value

val justState = a.runS(19).value

val justResult = a.runA(10).value

// map, flatMap: to transform and compose

val step1 = State[Int, String]{ num =>
  val ans = num +1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int, String] { num=>
  val ans = num*2
  (ans, s"Result of step2: $ans")
}

// State is threaded from step to step
// we don't interact with it in `for` comprehension
val both = for {
  a <- step1
  b <- step2
} yield (a,b)

val (state, result) = both.run(20).value

// extract the state as result
val getDemo = State.get[Int]
getDemo.run(19).value

// updates the state and returns unit as the result
val setDemo = State.set[Int](30)
setDemo.run(10).value

// ignores the state, returns a supplied result
val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

// extracts state via a transformation function
val inspectDemo = State.inspect[Int, String](x => s"${x}!")
inspectDemo.run(10).value

// updates the state using an update function
val modifyDemo = State.modify[Int](_+1)
modifyDemo.run(10).value

// we can ignore the result of intermediate stages that only transforms state
import cats.data.State
import cats.data.State._

val program : State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a +1)
  b <- get[Int]
  _ <- modify[Int](_+1)
  c <- inspect[Int, Int](_*1000)
} yield (a, b, c)

val (state, result) = program.run(1).value // state: 3, result:(1,2,3000)

/*
Exercise: Post-Order Calculator
- implement simple intrepreter
- pass values of mutable registers along with the result
1 + 2 becomes 1 2 +
- traverse the symbols from left to right, carrying a stack of operands
  - a number: push it to stack
  - an operator: pop two operands and calculate, push teh result to stack
 */

import cats.data.State

type CalcState[A] = State[List[Int], A]

// each state represents a functional transformation from a stack to a pair of stack and a result
def evalOne(sym: String): CalcState[Int] = sym match {
  case "+" => operator(_+_)
  case "-" => operator(_-_)
  case "*" => operator(_*_)
  case "/" => operator(_/_)
  case num => operand(num.toInt)
}

def operand(num:Int): CalcState[Int] =
  State[List[Int], Int]{ stack =>
    (num :: stack, num)
  }

def operator(func: (Int, Int) => Int): CalcState[Int]=
  State[List[Int], Int]{
    case b :: a :: tail =>
      val ans = func(a,b)
      (ans::tail, ans)
    case _ =>
      sys.error("Fail!")
  }

evalOne("42").runA(Nil).value

val program = for {
  _ <- evalOne("1")
  _ <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program.runA(Nil).value

State[List[Int], Int]{ oldStack =>
  val newStack = someTransformation(oldStack)
  val result = someCalculation
  (newStack, result)
}
