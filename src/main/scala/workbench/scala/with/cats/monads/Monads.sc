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
def stringDivideBy(a: String, b:String): Option[Int]=
  parseInt(a).flatMap(aNum =>
    parseInt(b).flatMap(bNum =>
      divide(aNum, bNum)))


/*
- Every monad is also a functor
- having both flatMap and map, we can use for-comprehensions to clarify sequencing behavior
 */
def stringDivideBy2(a:String, b:String): Option[Int]=
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
} yield (x,y)

/*
Futures
- sequences computations regardless of them being asynchronous

 */
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

def slow:Future[Int]= ???
def slower: Future[Int] = ???

// runs in sequence
def combine: Future[Int] =
  for {
    f1 <- slow
    f2 <- slower
  } yield f1 + f2

def combine2 : Future[Int] =
  slow.flatMap(f1 =>
    slower.map(f2 => f1 + f2))
