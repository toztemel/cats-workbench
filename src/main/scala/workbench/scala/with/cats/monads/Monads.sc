/*
Monads:
  - anything with
    - a constructor
    - a flatMap method
  - ex: Option, list, Future
  - `for comprehensions`
*/

/*

    A monad is a mechanism for `sequencing computations`.

 */

/*
Functors:
- to sequence computations, ignoring some complication
- limited by: only allows complication once at the beginning of the sequence
- flatMap considers intermediate complications
  - Option.flatmap takes intermediate Option
  - List.flatmap takes intermediate List
 */
def parseInt(str: String): Option[Int] =
  scala.util.Try(str.toInt).toOption

def divide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a / b)

/*
These methods can fail and return None
flatMap allows us to ignore this when sequencing.
So, as we call flatMap,
- sequence continues
- returns fail-fast error
 */
def stringDivideBy(a: String, b: String): Option[Int] =
  parseInt(a).flatMap(aNum =>
    parseInt(b).flatMap(bNum =>
      divide(aNum, bNum)))


/*
- Every monad is also a functor
- having both flatMap and map, we can use for-comprehensions to clarify sequencing behavior
 */
def stringDivideBy2(a: String, b: String): Option[Int] =
  for {
    numA <- parseInt(a)
    numB <- parseInt(b)
    ans <- divide(numA, numB)
  } yield ans

/*
Lists
- think of lists as sets of intermediate results
- flatMap becomes a construct that calculates permutations and combinations
 */
for {
  x <- (1 to 3).toList
  y <- (4 to 5).toList
} yield (x, y)

/*
Futures
- sequences computations regardless of them being asynchronous

 */

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

def slow: Future[Int] = ???
def slower: Future[Int] = ???

// runs in sequence
def combine: Future[Int] =
  for {
    f1 <- slow
    f2 <- slower
  } yield f1 + f2

def combine2: Future[Int] =
  slow.flatMap(f1 =>
    slower.map(f2 => f1 + f2))

/*
Definition of a Monad
- pure: of type A => F[A]
    abstracts over constructors, to create new monadic context from a plain value
- flatMap: ogf type (F[A], A=>F[B]) => F[B]
    sequencing step, extracts value from a context and generate next context in sequence
- obeys these laws:
--  Left identity:  pure(a).flatMap(func) == func(a)
--  Right identity: m.flatMap(pure) == m
--  Associativity:  m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
 */
trait AMonad[F[_]] {
  def pure[A](value: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))

}

/*

Monads in Cats

- extends FlatMap, for flatMap()
- extends Applicative, for pure(), which extends Functor, for map()
 */

import cats.Monad
import cats.instances.option._
import cats.instances.list._

val opt1 = Monad[Option].pure(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
val opt3 = Monad[Option].map(opt2)(a => 100 + a)

val list1 = Monad[List].pure(3)
val list2 = Monad[List].flatMap(list1)(a => List(a, a * 10))
val list3 = Monad[List].map(list2)(a => a + 123)

/*
Default Instances

instances are provided in cats.instances

Monad[Future] requires implicit ExecutionContext
 */

import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global

val fm = Monad[Future]

/*
Monad Syntax
comes from three places
- cats.syntax.flatMap
- cats.syntax.functor
- cats.syntax.applicative
or, for short
- cats.implicits
 */

import cats.instances.option._
import cats.instances.list._
import cats.syntax.applicative._

1.pure[Option]
1.pure[List]

/*
Example with custom Monad
 */


import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
  a.flatMap(x => b.map(y => x * x + y * y))

def sumSquare2[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield x * x + y * y

import cats.instances.option._
import cats.instances.list._

sumSquare(Option(3), Option(4))
sumSquare2(Option(3), Option(4))

sumSquare(List(1, 2, 3), List(4, 5))
sumSquare2(List(1, 2, 3), List(4, 5))

/*
4.3 Identity Monad

Cat's flatMap and map syntax works well on Options, Lists, but not plain values
sumSquare(2,3) gives syntax error

To use sumSquare with either a monad or plain value:
we should abstract over monadic/non-monadic code

Cat provides `Id` type to bridge the gap
 */

import cats.Id

sumSquare(2: Id[Int], 3: Id[Int]) // returns Id[Int]

// definition of Id, a type alias
//type Id[A] = A

"Dave": Id[String]
123: Id[Int]
List(1, 2, 3): Id[List[Int]]

val a = Monad[Id].pure(3)
val b = Monad[Id].flatMap(a)(_ + 1)

import cats.syntax.functor._
import cats.syntax.flatMap._

for {
  x <- a
  y <- b
} yield x + y

/*
Example use case:
We can run
- asynchronously using Future in production
- synchronously using Id in test
 */

/*
Exercise: Monadic Secret Identities

 */

import cats.Id

def pure[A](value: A): Id[A] =
  value

def map[A, B](value: Id[A])(f: A => B): Id[B] =
  f(value)

def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] =
  f(value)

pure(123)
map(123)(_ * 2)
flatMap(123)(_ * 2)

/*
functors and monads as sequencing type classes
- allow us to sequence operations, ignoring complications
- for Id, no complication => map becames same as flatMap
 */

/*
4.4 Either

In scala 2.12 it became 'right biased'
- with map,flatMap operations
 */

import cats.syntax.either._

val ra = 3.asRight[String] // returns Either[String, Left]
Either.catchOnly[NumberFormatException]("foo".toInt) // Either[NumberFormatException, Int]
Either.catchNonFatal(sys.error("Badness")) // Either[Throwable, Nothing]

Either.fromTry(scala.util.Try("foo".toInt)) // Either[Throwable, Int]
Either.fromOption[String, Int](None, "Badness") // Either[String, Int]

/*
Transforming Eithers

  - orElse
  - getOrElse
 */

import cats.syntax.either._

"Error".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])

// ensure: to check whether right-value satisfies predicate
(-1).asRight[String].ensure("Must be non negative")(_ > 0)

// recover and recoverWith: provide error handling
"error".asLeft[Int].recover {
  case _: String => (-1)
} // Right(-1)

"error".asLeft[Int].recoverWith {
  case _: String => Right(-1)
} // Right(-1)

// leftMap and biMap methods
"foo".asLeft[Int].leftMap(_.reverse) // Left("oof")

6.asRight[String].bimap(_.reverse, _ * 7) // Right(42)

"bar".asLeft[Int].bimap(_.reverse, _ * 7) // Left("rab")

// swap : to exchange left for right
123.asRight[String].swap // Left(123)

/*
Error Handling
- Either is typical for fail-fast error handling
- sequence with flatMap
- if one computation fails, remaining computations do not run
 */
for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if (b == 0) "DIV0".asLeft[Int]
  else (a / b).asRight[String]
} yield c * 100

// Determine what type to use to represent errors

// too generic:
type Result[A] = Either[LoginError, A]

// more specific:
sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username: String) extends LoginError

final case class PasswordIncorrect(username: String) extends LoginError

case object UnexpectedError extends LoginError

case class User(username: String, password: String)

type LoginResult = Either[LoginError, User]

// Choose error-handling behaviour based on type:
def handleError(error: LoginError): Unit =
  error match {
    case UserNotFound(u) =>
      println(s"User not found $u")

    case PasswordIncorrect(u) =>
      println(s"Password incorrect: $u")

    case UnexpectedError =>
      println("Unexpected error")
  }

val result1: LoginResult = User("dave", "123").asRight
val result2: LoginResult = UserNotFound("dave").asLeft

result1.fold(handleError, println) // User(dave, 123)
result2.fold(handleError, println) // User not found: dave

/*
Checkout MonadError for error handling
  - abstracts over Either-like data types
  - provides operations for raising-handling errors

 */

/*
4.6 Eval Monad
  - allows us to abstract over different models of evaluation
  - eager and lazy / call-by-value and call-by-name
  - allows for a result to be memoized / call-by-need
  - stac-safe, use it in recursions
 */


/*
Call-by-value evaluation:
  - eager: computation of value of x happens at place where it is defined
  - memoized: computation is evaluated once. accessing recalls the stored value
 */
val x = {
  println("compute x")
  math.random()
} // computed and assigend
x // first access, same value
x // second access, same value

/*
Call-by-name
  - lazy: computation is evaluated at the point of use
  - not memoized: computation is evaluated each time it is used
 */
def y = {
  println("compute y")
  math.random()
}
y // first access, computes new value
y // second access, computes new value

/*
Call-by-need ( lazy vals)
  - lazy: computation runs when it is first used
  - memoized: subsequent accesses reused same cached value
 */
lazy val z = {
  println("compute z")
  math.random()
}
z
z

/*
Eval''s Model of Evaluation
  - it has 3 subtypes
    - Now
    - Always
    - Later
 */

import cats.Eval

// eager and memoized
val now = Eval.now(math.random() + 1000)
// lazy and not memoized. similar to def
val always = Eval.always(math.random() + 3000)
// lazy and memoized, similar to lazy val
val later = Eval.later(math.random() + 2000)

now.value
always.value
later.value

/*
  - map and flatMap add computations to a chain
  - chain is stored explicitly as a list of functions
  - functions run when we call 'value' method
  - semantics of the originating Eval instance is maintained
  - mapping functions are always called lazily on demand
 */
val greeting = Eval.
  always {
    println("step 1"); "Hello"
  }.
  map { str => println("step 2"); s"$str world" }
// Eval[String]

greeting.value
greeting.value


val ans = for {
  a <- Eval.now {
    println("set a"); 40
  }
  b <- Eval.always {
    println("set b"); 2
  }
} yield {
  println("add a and b")
  a + b
}
// set a
// ans: Eval[Int]

ans.value
// set b
// sum a and b
// result: INt = 42
ans.value
// set b
// sum a and b
// result: INt = 42

/*
 memoize : to memoize a chain of computation
 */
val saying = Eval
  .always{println("step 1"); "the cat"}
  .map{str => println("step 2"); s"$str sat on"}
  .memoize
  .map{str => println("step 3"); s"$str the mat"}

saying.value
// step 1 -2 -3
saying.value
// step 3

/*
 stack safety
  - map and flatMap methods are trampolined
  - we can nest calls arbitrarily without consumeing stack frames
 */
def factorial(n: BigInt): BigInt =
  if (n ==1) n else n * factorial(n-1)

//factorial(50000)
// StackOverflowError

def factorial2(n: BigInt): Eval[BigInt] =
  if (n ==1) Eval.now(n)
  else factorial2(n-1).map(_*n)

// factorial2(50000)
// StackOverflowError

def factorial3(n: BigInt): Eval[BigInt] =
  if (n ==1 ) Eval.now(n)
  else Eval.defer(factorial3(n-1).map(_*n))

factorial3(50000)
