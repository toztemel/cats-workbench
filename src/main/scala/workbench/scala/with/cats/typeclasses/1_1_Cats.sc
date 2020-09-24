import cats.Show
// import implicit default instances
import cats.instances.int._
import cats.instances.string._

val showInt = Show.apply[Int]
val showString = Show.apply[String]

val intAsString = showInt.show(123)
val strAsString = showString.show("abc")

// import interface syntax

import cats.syntax.show._

val shownInt = 123.show
val shownStr = "abc".show

// import eveyrting

import cats._
import cats.implicits._

/*
Define Custom Instances
 */

import java.util.Date

implicit val dateShow: Show[Date] =
  (t: Date) => s"${t.getTime}ms since epoch"

new Date().show

// UTilitiy
//implicit val dateShow2:Show[Date] =
//  Show.show(t => s"${t.getTime}ms since epoch" )


/*
Exercise 1.4.6 Cat Show
 */
final case class Cat(name: String, age: Int, color: String)

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

implicit val catShow: Show[Cat] =
  Show.show(c => {
    val name = c.name.show
    val age = c.age.show
    val colour = c.color.show
    s"$name is a $age year-old $colour cat"
  })

Cat("Minnak", 5, "Black-White").show


/*
Example: Eq
for type-safe equality instead of built-in == operator

Cats provides
=== for equality
=!= for inequality
 */

import cats.syntax.eq._
import cats.instances.int._

val eqInt = Eq[Int]
eqInt.eqv(123, 123)
// syntax error
// eqInt.eqv(123, "123")

import cats.syntax.eq._

123 === 123
123 =!= 234
// syntax error
//123 === "123"

Option(1) === Option.empty[Int]

import cats.syntax.option._

1.some === none[Int]
1.some =!= none[Int]

/*
Comparing Custom Types

 */

import java.util.Date
import cats.instances.long._

implicit val dateEq: Eq[Date] =
  Eq.instance[Date] { (date1, date2) =>
    date1.getTime === date2.getTime
  }

val x = new Date()
val y = new Date()

x === y
x =!= y

/*
Exercise: Equality for Cat
 */
import cats.Eq
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._

final case class Cat(name: String, age: Int, color: String)

implicit val catEqual: Eq[Cat] =
  Eq.instance[Cat] { (c1, c2) =>
    c1.name === c2.name &&
    c1.age === c2.age &&
    c1.color === c2.color
  }

val ares = Cat("Ares", 5, "Black-White")
val minnak = Cat("Minnak", 5, "White-Black")
val aresO = Option(ares)
val minnakO = Option(minnak)

ares === minnak
ares =!= minnak

import cats.instances.option._
aresO === minnakO
aresO =!= minnakO


/*
Variance
substitute one value for another

Covariance: F[B] is subtype of F[A] if B is subtype of A
useful for outputs

Contravariance: F[B] is a subtype of F[A] if A is a subtype of B
useful for inputs
*/

//covariant
trait F[+A]
trait List[+A]
trait Option[+A]

// Example
sealed trait Shape
case class Circle(radius: Double) extends Shape

val circles: List[Circle] = ???
val shapes: List[Shape] = circles

// Contravariant
trait F[-A]
trait Json
trait JsonWrite[-A] {
  def write(value:A):Json
}

/*
Example Contravariance

because Circle is a subtype of Shape
We can print a Circle with both JsonWriters
Thus, JsonWriter[Shape] is a subtype of JsonWriter[Circle]

We can't write a Shape with circleWriter
not all shapes are circles
 */


/*
Invariance
F[A] and F[B] are never subtypes of each other
 */
trait F[A]