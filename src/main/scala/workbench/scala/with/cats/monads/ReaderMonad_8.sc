/*
cats.data.Reader
  - to sequence operations that depend on some input
  - composing functions of one argument
  - e.g. dependency injection with external config
  - write steps of program as Reader instances, chain them with map/flatMap, accept dependency as input
Most userful when:
  - constructing a program than can be represented by a function
  - defer injection of a known parameter or set of parameters
  - testing parts of program in isolation
 */

// apply constructor: creates Reader[A,B] from function A=>B

import cats.data.Reader

final case class Cat(name: String, favoriteFood: String)

val catName: Reader[Cat, String] = Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

// map, flatMap: set of readers accepting same type of config
// map: extends computation in the Reader by passing its result
val greetKitty: Reader[Cat, String] =
catName.map(name => s"Hello $name")

greetKitty.run(Cat("Ares", "junk food"))

// flatMap: combine readers that depend on same input type
val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

greetAndFeed(Cat("Garfield", "lasagne"))
greetAndFeed(Cat("Ares", "jund food"))

/*
Exercise: Simple login system
config: 2 databases: list of valid users, list of their passwords
 */
final case class Db(usernames: Map[Int, String],
                    passwords: Map[String, String])

type DbReader[A] = Reader[Db, A]

// lookup username for Int userId
def findUsername(userId: Int): DbReader[Option[String]] =
  new DbReader(db => db.usernames.get(userId))

// lookup password for a username String
def checkPassword(username:String, password: String): DbReader[Boolean] =
  new DbReader(db => db.passwords.get(username).contains(password))

// check password for a given user ID
import cats.syntax.applicative._

def checkLogin(userId: Int, password: String): DbReader[Boolean]=
  for {
    u <- findUsername(userId)
    p <- u.map(uname => checkPassword(uname, password)).getOrElse(false.pure[DbReader])
  } yield p

val users = Map(
  1 -> "tayfun",
  2 -> "ceren",
  3 -> "deniz"
)

val passwords = Map(
  "tayfun" -> "test",
  "ceren" -> "pass",
  "deniz" -> "secret"
)

val db = Db(users, passwords)

checkLogin(1, "test").run(db)

checkLogin(4, "test").run(db)

checkLogin(2, "pass_X").run(db)