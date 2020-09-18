// https://www.scalawithcats.com/dist/scala-with-cats.html#functors

/*
- an abstraction to represent sequences of operations
within a context, eg. List, Option

- anything with a `map` method

Examples:
Option, List, Either
map to `transform` the content, keep the structure
a way of sequencing computations on values
 */

List(1, 2, 3).map(_ + 1)
List(1, 2, 3)
  .map(_ + 1)
  .map(_*2)
  .map(n => s"${n}!")

/*
Futures:
sequences asynchronous comutations by queeuing them
apply them as predecessors complete

 */
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val f = Future(123).map(_+1).map(_*2).map(n=>s"${n}!")
Await.result(f, 1.second)

/*
Functions
 */
val func1: I