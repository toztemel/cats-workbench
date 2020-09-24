/*
3 aspects of implementation:

1. Type class
2. Instances
3. Interface
 */

/*
cats.kernel.Monoid extends cats.kernel.Semigroup
cats.Monoid extends cats.Semigroup

 */

import cats.Monoid
import cats.Semigroup

/*
standard Cats pattern with apply method
 */
import cats.instances.string._

val ss0 = Semigroup[String]
val ss = Semigroup.apply[String]
ss.combine("Hi ", "there")

val ms0 = Monoid.apply[String]
val ms = Monoid[String]
ms.combine("Hi ", "there")
ms.empty

import cats.Monoid
import cats.instances.int._

Monoid[Int].combine(1, 2)

import cats.Monoid
import cats.instances.int._
import cats.instances.option._

Monoid[Option[Int]].combine(Option(1), Option(2))
Monoid[Option[Int]].combine(Option(1), Option.empty[Int])

// to import everything

import cats._
import cats.implicits._

/*
Monoid Syntax
Combine method |+|
 */
import cats.instances.int._
import cats.instances.string._
import cats.syntax.semigroup._

"Hi" |+| "there" |+| Monoid[String].empty
1 |+| 2 |+| Monoid[Int].empty



/*
2.5.4 Exercise

 */
//1

import cats.Monoid
import cats.instances.int._
import cats.syntax.monoid._

def add(items: List[Int]): Int = {
  items.foldLeft(Monoid[Int].empty)(_ |+| _)
}

// 2

import cats.Monoid
import cats.syntax.monoid._

def add[A](items: List[A])(implicit m: Monoid[A]): A = {
  items.foldLeft(m.empty)(_ |+| _)
}
// with context bound syntax
def add2[A: Monoid](items: List[A]): A = {
  items.foldLeft(Monoid[A].empty)(_ |+| _)
}


import cats.instances.int._
import cats.instances.option._

add(List(1, 2, 3))
add(List(Some(1), None, Some(2), Some(3)))

// complier error for missing Monoid[Some[Int]]
// add(List(Some(1), Some(2), Some(3)))


// 3
case class Order(totalCost: Double, quantity: Double)

implicit val orderCombine: Monoid[Order] = new Monoid[Order] {
  override def empty = Order(0d, 0d)

  override def combine(x: Order, y: Order) =
    Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
}

val o1 = Order(1d, 2d)
val o2 = Order(3d, 4d)
import cats.syntax.semigroup._
o1 |+| o2

/*
Applications of Monoids
an abstraction of the concept of adding or combining

1. Big data
map - reduce
eg: Twitter Algebird, Summingbird projects

2. Distributed systems
for eventual consistency, Commutative Replicated Data Types (CRDTs)
merge operation is important
 */