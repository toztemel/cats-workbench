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
import scala.concurrent.Future

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
