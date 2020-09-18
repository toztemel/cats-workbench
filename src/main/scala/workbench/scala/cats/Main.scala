package workbench.scala.cats

import cats.instances.string._
import cats.instances.set._
import cats.kernel.Monoid
import cats.syntax.monoid._
import workbench.scala.cats.model._
import workbench.scala.cats.monoids.PermissionsMonoids._

object Main extends App {
  println("Hello " |+| "Cats!")

  val sa1 = ServiceActivity("activity1", Set.empty[CategoryCode], Set.empty[SegmentCode])
  val sa2 = ServiceActivity("activity2", Set.empty[CategoryCode], Set.empty[SegmentCode])
  val sa3 = ServiceActivity("activity3", Set(CategoryCode(1)), Set.empty[SegmentCode])
  val sa4 = ServiceActivity("activity4", Set(CategoryCode(2)), Set(SegmentCode(-99)))

  println(Monoid[ServiceActivity].combine(sa1, sa2))
  println(sa1 |+| sa3)
  println(sa4 |+| sa3)
  println(sa3 |+| sa4)

  val sas1 = Set(sa1, sa2)
  val sas2 = Set(sa3, sa4)
  println(sas1 |+| sas2)

}
