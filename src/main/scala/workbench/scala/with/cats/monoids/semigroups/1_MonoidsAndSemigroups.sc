// https://www.scalawithcats.com/dist/scala-with-cats.html#sec:monoids

/*
To combine values:
- Integer addition
* closed: always produces Int
* identity: 0
* associative

- Integer multiplication
* identity: 1
* associative

- String - Sequence concatenation
* identity: ""
* associative
 */

/*
Monoid

Monoid of type A is
1. an operation combine(A,A) => A
2. an element empty of type A

 */
trait Monoid[A] {
  def combine(x: A, y: A): A

  def empty: A
}

def associativityLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
  m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
}

def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
  m.combine(m.empty, x) == x &&
    m.combine(x, m.empty) == x
}

/*
Semigroup

just combine part of a monoid, without empty

Eq:
- Non-Empty sequence concatenation
- Positive integer addition

 */
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit m: Monoid[A]) = m
}

/*
Exercise 2.3

Monoids for boolean
operator &&, identity true
operator ||, identity false
operator exclusive or, identity false
operator exclusive nor, identity true
 */
implicit val booleanAndMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean) = a && b

    def empty = true
  }

implicit val booleanOrMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean) = a || b

    def empty = false
  }

implicit val booleanEitherMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean) =
      (a && !b) || (!a && b)

    def empty = false
  }

implicit val booleanXnorMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    def combine(a: Boolean, b: Boolean) =
      (!a || b) && (a || !b)

    def empty = true
  }

/*
2.4 Exercise: Monoids for Set
1. operation: union, identity: Empty set
2. symmetric difference, identity: Empty set

Intersection is a semi-group
Set complement and difference are not associative!
 */
implicit def setUnionMonoid[A]: Monoid[Set[A]] =
  new Monoid[Set[A]] {
    def combine(a: Set[A], b: Set[A]) = a union b

    def empty = Set.empty[A]
  }
val intSetMonoid = Monoid[Set[Int]]
val strSetMonoid = Monoid[Set[String]]
intSetMonoid.combine(Set(1, 2), Set(2, 3))
strSetMonoid.combine(Set("A", "B"), Set("B", "C"))


//implicit def symDiffMonoid[A]: Monoid[Set[A]] =
//  new Monoid[Set[A]] {
//    def combine(a: Set[A], b: Set[A]): Set[A] =
//      (a diff b) union (b diff a)
//
//    def empty: Set[A] = Set.empty
//  }
//
implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
  new Semigroup[Set[A]] {
    def combine(a: Set[A], b: Set[A]) =
      a intersect b
  }