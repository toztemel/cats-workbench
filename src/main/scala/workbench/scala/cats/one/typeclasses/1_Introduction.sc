// https://www.scalawithcats.com/dist/scala-with-cats.html#sec:type-classes

/**
 * 1. type class itself
 * implemented using implicit values and parameters, optionally implicit classes *
 * 2. instances for particular types
 * 3. methods that use type classes
 *
 * * a. traits: type classes
 * * b. implicit values: type class instances
 * * c. implicit parameters: type class use
 * * d. implicit classes: optional utilities
 *
 */

/*
Type Class
Interface or API representing a functionality
trait with one type parameter

Ex: JSON serialization
 */

// supporting code to define Json AST
sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

final case object JsNull extends Json

// our type class to represent json serialization behavior
trait JsonWriter[A] {
  def write(value: A): Json
}

/*
 Type Class Instances
 Implementations of Type Class
 tagged with `implicit`
 */
final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val numberWriter: JsonWriter[Double]=
    (value:Double) => JsNumber(value)

  implicit val stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] =
    (value: Person) => JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))
}

/*
Type Class Use
any functianality/method that requires type class as implicit parameter

1. Interface Objects
2. Interface Syntax
 */

/*
1. Interface Objects
place methods in a singleton object
 */
object Json{
  def toJson[A](value:A)(implicit w:JsonWriter[A])=
    w.write(value)
}
import JsonWriterInstances._
Json.toJson(Person("Tayfun", "t@tayfun.com"))(personWriter)
Json.toJson(Person("Tayfun", "t@tayfun.com"))

/*
2. Interface Syntax
extension methods to extend existing types with interface methods
with syntax type class
 */

object JsonSyntax {
  implicit class JsonWriterOps[A](value:A) {
    def toJson(implicit w:JsonWriter[A]): Json =
      w.write(value)
  }
}

// to use it, import syntax with writer instances
import JsonWriterInstances._
import JsonSyntax._

Person("Tayfun", "t@email.com").toJson(personWriter)
Person("Tayfun", "t@email.com").toJson


/*
TO debug, use implicitly
def implicitly[A](implicit value:A): A = value

 */
import JsonWriterInstances._
val sWriter = implicitly[JsonWriter[String]]


// ==========

/*
Rules for effective working with implicit values and parameters
1. Packaging implicits
2. Implicit Scope
3. Recursive Implicit resolution
 */

/*
1. implicit definitiots must be placed inside an object or trait
Ex. JsonWriterInstances
Ex. JsonWriter companion object (plays into implicit scope)
 */

/*
2. Implicit scope: compiler searches for candidate instances

a. local or inherited definitionts
b. improted definitions
c. definitions in the companion object of the type class or parameter type

Multiple candidates => ambiguous implicit value error

For simplicity, package type class instances in 4 ways:
1. in an objects, like JsonWriterInstances. import into scope
2. in a trait. inherit into scope
3. in companion object of the type class. always in scope
4. in companion object of the parameter type. always in scope
 */

/*
3. Recursive Implicit Resouliton
a.k.a Type class composition

Define type class instances in two ways
1. concrete instances as implicit vals of required type
2. define implicit methods to construct instancesn from other type class instances

Ex: JsonWriter for Option
implicit val optionIntWriter:    JsonWriter[Option[Int]] = ???
implicit val optionPersonWriter: JsonWriter[Option[Person]] = ???
...

Abtract the code for handling A and Option[A] into a common constructor
- if option is Some(value), write value for writer of A
- if none, return JsNull
 */

implicit def optionWriter[A](implicit writer:JsonWriter[A]): JsonWriter[Option[A]] =
  new JsonWriter[Option[A]] {
    override def write(option: Option[A]): Json =
      option match {
        case Some(v) => writer.write(v)
        case None => JsNull
      }
  }
// compiler recursively searches for implicit writers
Json.toJson(Option("Some string"))
Json.toJson(Option("Some string"))(optionWriter[String])
Json.toJson(Option("Some string"))(optionWriter(stringWriter))

/*
  implicit def XX(implicit v:A)
vs.
  implicit def XX(v:A)

Implicit conversion pattern is different.
compiler gives warning.
implicit def optionWriter[A](writer: JsonWriter[A]): JsonWriter[Option[A]] = ???

 */


/*
Exercise:
Define a Printable type class
1. Type class Printable[A] with format method. Accept value of type A, return String

 */
trait Printable[A] {
  def format(v: A): String
}
object PrintableInstances {
  val stringPrintable: Printable[String] = (v: String) => v
  val intPrintable:Printable[Int] = (i:Int) => i.toString
}
object Printable {
  def format[A](v:A)(implicit p:Printable[A]):String = p.format(v)
  def print[A](v:A)(implicit p:Printable[A]):Unit = println(format(v))
}

// Example usage
final case class Cat(name:String, age:Int, color:String)
import PrintableInstances._
object Cat {
  implicit val catPrintable:Printable[Cat] =
    (c:Cat) =>
      s"""
         |${Printable.format(c.name)} is a
         |${Printable.format(c.age)} year-old
         |${Printable.format(c.color) } cat.
         |""".stripMargin
}
val minnak = Cat("Minnak", 5, "Black-White")
Printable.print(minnak)

/*
 Better Syntax
 */
object PrintableSyntax {
  implicit class PrintableOps[A](a:A) {
    def format(implicit p:Printable[A]): String = p.format(a)
    def print(implicit p:Printable[A]): Unit = println(format(p))
  }
}
import PrintableSyntax._
minnak.print


