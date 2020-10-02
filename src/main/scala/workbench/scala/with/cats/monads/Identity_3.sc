/*
4.3 Identity Monad

Cat's flatMap and map syntax works well on Options, Lists, but not plain values
sumSquare(2,3) gives syntax error

To use sumSquare with either a monad or plain value:
we should abstract over monadic/non-monadic code

Cat provides `Id` type to bridge the gap
 */

import cats.Id
import cats.Monad

def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield x * x + y * y


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
