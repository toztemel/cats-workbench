/*
4.4 Either

In scala 2.12 it became 'right biased'
- with map,flatMap operations
 */

import cats.syntax.either._

val ra = 3.asRight[String] // returns Either[String, Left]
Either.catchOnly[NumberFormatException]("foo".toInt) // Either[NumberFormatException, Int]
Either.catchNonFatal(sys.error("Badness")) // Either[Throwable, Nothing]

Either.fromTry(scala.util.Try("foo".toInt)) // Either[Throwable, Int]
Either.fromOption[String, Int](None, "Badness") // Either[String, Int]

/*
Transforming Eithers

  - orElse
  - getOrElse
 */

import cats.syntax.either._

"Error".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])

// ensure: to check whether right-value satisfies predicate
(-1).asRight[String].ensure("Must be non negative")(_ > 0)

// recover and recoverWith: provide error handling
"error".asLeft[Int].recover {
  case _: String => (-1)
} // Right(-1)

"error".asLeft[Int].recoverWith {
  case _: String => Right(-1)
} // Right(-1)

// leftMap and biMap methods
"foo".asLeft[Int].leftMap(_.reverse) // Left("oof")

6.asRight[String].bimap(_.reverse, _ * 7) // Right(42)

"bar".asLeft[Int].bimap(_.reverse, _ * 7) // Left("rab")

// swap : to exchange left for right
123.asRight[String].swap // Left(123)

/*
Error Handling
- Either is typical for fail-fast error handling
- sequence with flatMap
- if one computation fails, remaining computations do not run
 */
for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if (b == 0) "DIV0".asLeft[Int]
  else (a / b).asRight[String]
} yield c * 100

// Determine what type to use to represent errors

// too generic:
type Result[A] = Either[LoginError, A]

// more specific:
sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username: String) extends LoginError

final case class PasswordIncorrect(username: String) extends LoginError

case object UnexpectedError extends LoginError

case class User(username: String, password: String)

type LoginResult = Either[LoginError, User]

// Choose error-handling behaviour based on type:
def handleError(error: LoginError): Unit =
  error match {
    case UserNotFound(u) =>
      println(s"User not found $u")

    case PasswordIncorrect(u) =>
      println(s"Password incorrect: $u")

    case UnexpectedError =>
      println("Unexpected error")
  }

val result1: LoginResult = User("dave", "123").asRight
val result2: LoginResult = UserNotFound("dave").asLeft

result1.fold(handleError, println) // User(dave, 123)
result2.fold(handleError, println) // User not found: dave
