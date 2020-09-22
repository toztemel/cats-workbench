import cats.implicits.catsStdInstancesForFuture
// https://www.scalawithcats.com/dist/scala-with-cats.html#functors

/*
Functors:
- a class that encapsulates sequencing computations
- a functor is a type F[A] with an operation `map` wyth type (A=>B) => F[B]
- e.g. monads, applicative functors
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
  .map(_ * 2)
  .map(n => s"${n}!")

/*
Futures:
sequences asynchronous computations by queuing them
apply them as predecessors complete

 */

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val f = Future(123).map(_ + 1).map(_ * 2).map(n => s"${n}!")
Await.result(f, 1.second)

/*
Functions
- single argument functions are also functors
- mapping over a Function1 is a function composition/sequencing:
1. start with MyFunc[A]
2, usupply a function A=>B
3. get back MyFunc[B]
 */

import cats.instances.function._
import cats.syntax.functor._

val func1: Int => Double = _.toDouble
val func2: Double => Double = _ * 2

(func1 map func2)(1)
(func1 andThen func2)(1)
func2(func1(1))

val func3 = ((x:Int) => x.toDouble).
  map(_+1).
  map(_*2).
  map(x => s"${x}!")
func3(123)

/*
Cats implementation
- Identity: fa.map(a=>a) == fa
- Compostion: fa.map(g(f(_))) == fa.map(f).map(g)
 */

trait Functor[F[_]] {
  def map[A,B](fa:F[A])(f: A=>B): F[B]
}
/*
Higher Kinds and Type Constructors

List: a type constructor with one hole, takes one parameter
List[Int]: type, produced by applying a type parameter
List[A]: type


Functions are 'value constructors'
math.abs  // function. takes one parameter
math.abs(x) // value, produced by applying a value parameter

Type constructors are declaredy using 'underscores'
- specificer # of holes


// Declare F using underscore
def myMethod[F[_]] = {
  // Reference F without underscore
  val functor = Functor.apply[F]
}

 */


/*
Functors in Cats

 */

import cats.Functor
import cats.instances.list._
import cats.instances.option._

val list1= List(1,2,3)
val list2 = Functor[List].map(list1)(_*2)

val option1 = Option(123)
val option2 = Functor[Option].map(option1)(_.toString)

/*
lift method:
- converts function of type A=>B to a functor of F[A] => F[B]
 */
val func = (x:Int) => x+1

val liftedFunc = Functor[Option].lift(func)

liftedFunc(Option(1))

/*
as method:
- replaces with value inside the Functor with the given value
 */
Functor[List].as(list1, "As")


/*
Functor Syntax
 */
import cats.instances.function._
import cats.syntax.functor._

val f1 = (a:Int) => a+1
val f2 = (a:Int) => a*2
val f3 = (a:Int) => s"${a}!"
val f4 = f1.map(f2).map(f3)

f4(123)

/*
abstract over functors
- if start dosen't have a map method, compiler detects and wrap the expressions in a FunctorOps, with implicit Functor parameter
 */
import cats.instances.function._
import cats.syntax.functor._

def doMath[F[_]](start:F[Int])(implicit functor:Functor[F]): F[Int] =
  start.map(n => n+1*2)

import cats.instances.option._
import cats.instances.list._

doMath(Option(30))
doMath(List(1,2,3))

final case class Box[A](value:A)

val box = Box[Int](123)

// compiler error if cats is not implicitly available
//box.map(_+1)

/*
Instances for Custom Types
- simply define a map method
 */

implicit val optionFunctor: Functor[Option] =
  new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  }

/*
Inject dependencies into our instances
 */
import scala.concurrent.{Future, ExecutionContext}

implicit def futureFunctor(implicit ec:ExecutionContext): Functor[Future] =
  new Functor[Future] {
    override def map[A, B](fa: Future[A])(f: A => B) = fa.map(f)(ec)
  }

Functor[Future](futureFunctor(ExecutionContext.Implicits.global))
Functor[Future](futureFunctor)
Functor[Future]


/*
Exercise: Transforming tree with Functors
 */
sealed trait Tree[+A]
final case class Branch[A] (left: Tree[A], right:Tree[A]) extends Tree[A]
final case class Leaf[A](value:A) extends Tree[A]

// similar to List, recurse over the data structure
// apply the function to every Leaf
import cats.Functor

implicit val treeFunctor: Functor[Tree] =
  new Functor[Tree]{
    override def map[A, B](tree: Tree[A])(f: A => B) : Tree[B] =
      tree match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
  }

object Tree{
  def branch[A](l:Tree[A], r:Tree[A]): Tree[A] = Branch(l,r)
  def leaf[A](value:A): Tree[A] = Leaf(value)
}
//Branch(Leaf(10), Leaf(20)).map(_*2)
Tree.leaf(100).map(_*2)
Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_*2)