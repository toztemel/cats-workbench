// https://www.scalawithcats.com/dist/scala-with-cats.html#writer-monad
/*
Writer Monad
  - lets us carry a log along with a computation
  - to record messages, errors, additional data
  - extract log alongside the final result
  - e.g. record sequences of steps in multi-threaded computations; log is tied to the result
 */

/*
Writer[W, A]
  - log of type W
  - result of type A
 */

import cats.Id
import cats.data.Writer
import cats.data.WriterT
import cats.instances.vector._

val res: WriterT[Id, Vector[String], Int] =
  Writer(Vector(
  "It was the best of times",
  "It was the worst of times"
), 1859)

/*
WriterT[Id, Vector[String], Int]
  - it's a `monad transformer`
  - WriterT[Id, W, A] as Writer[W,A] for convenience

type Writer[W,A] = WriterT[Id, W, A]

 */

// If we only have a result
import cats.instances.vector._
import cats.syntax.applicative._

type Logged[A] = Writer[Vector[String], A]

123.pure[Logged]

// if we have a log and no result
import cats.syntax.writer._

Vector("msg1", "msg2", "msg3").tell

// if we have both a result and a log
import cats.syntax.writer._

val a = Writer(Vector("msg1", "msg2"), 123)

val b = 123.writer(Vector("msg1, msg2"))

// to extract the result and log from a Writer
val aResult = a.value
val aLog = a.written

// to extract both at the same time
val (bLog, bResult) = b.run

/*
Composing and Transforming Writers
  - log is preserved during `map` and `flatMap`
  - `flatMap` appends the logs and result from the source Writer
  - use a log type with efficient append & concatenate operations
 */

val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x","y","z'"))
} yield a+b
writer1.run

// mapWritten: to transform the log
val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
writer2.run

/*
bimap, mapBoth: to transform both log and result
  - bimap: takes two function parameters
  - mapBath: takes a single function that accepts two parameters
 */
val writer3 = writer1.bimap(
  log => log.map(_.toUpperCase),
  res => res*100
)
writer3.run

val writer4 = writer1.mapBoth{(log, res) =>
  val log2 = log.map(_+"!")
  val res2 = res*100
  (log2, res2)
}
writer4.run

// reset: to clear the log
// swap: to swap log and result
val writer5 = writer1.reset
writer5.run

val writer6 = writer1.swap
writer6.run

/*
Exercise:
Computing factorials
 */

def slowly[A](body: => A)=
  try body finally Thread.sleep(100)

def factorial(n: Int): Int = {
  val ans = slowly(if(n==0) 1 else n* factorial(n-1))
  println(s"fact $n! = $ans")
  ans
}

import scala.concurrent._;

import scala.concurrent.ExecutionContext.Implicits._;

import scala.concurrent.duration._;

Await.result(Future.sequence(Vector(
  Future(factorial(5)),
  Future(factorial(5))
)), 5.seconds)

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._

42.pure[Logged]

import cats.syntax.writer._

Vector("a message").tell

41.pure[Logged].map(_+1)

def newFactorial(n: Int): Logged[Int] = {
  for {
    ans <- if (n == 0) {
      1.pure[Logged]
    } else {
      slowly(newFactorial(n - 1).map(_ * n))
    }
    _ <- Vector(s"fact $n $ans").tell
  } yield ans
}

def newFactorial2(n: Int): Writer[Vector[String], Int] = {
  val ans = slowly(
    if(n==0) 1.writer(Vector(s"fact 0! = 1"))
    else newFactorial2(n-1).bimap(
      log => log.appended(""),
      value => value * n
    )
  )
  //println(s"fact $n! = $ans")
  ans
}
Await.result(Future.sequence(Vector(
  Future(newFactorial(5)),
  Future(newFactorial(5))
)).map(_.map(_.written)), 5.seconds)

Await.result(Future.sequence(Vector(
  Future(newFactorial2(5)),
  Future(newFactorial2(5))
)).map(_.map(_.written)), 5.seconds)
