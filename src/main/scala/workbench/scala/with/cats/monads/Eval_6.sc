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

/*
Exercise: Making foldRight stack safe
 */

// not safe
def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  as match {
    case head :: tail =>
      fn(head, foldRight(tail, acc)(fn))
    case Nil =>
      acc
  }
// safe
import cats.Eval
def foldRightEval[A, B](as: List[A], acc: Eval[B])
                       (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil =>
      acc
  }
def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc)) { (a, b) =>
    b.map(fn(a, _))
  }.value

foldRight2((1 to 100000).toList, 0L)(_ + _)
// res24: Long = 5000050000L