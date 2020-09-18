package workbench.scala.cats.monoids

import cats.Monoid
import cats.instances.string._
import cats.instances.set._
import cats.syntax.monoid._
import workbench.scala.cats.model.{CategoryCode, SegmentCode, ServiceActivity}

object PermissionsMonoids {

  implicit val monoid: Monoid[ServiceActivity] = new Monoid[ServiceActivity] {
    override def empty: ServiceActivity = new ServiceActivity("", Set.empty[CategoryCode], Set.empty[SegmentCode])

    override def combine(x: ServiceActivity, y: ServiceActivity): ServiceActivity =
      ServiceActivity(
        x.value |+| y.value,
        x.nlCategoryCode |+| y.nlCategoryCode,
        x.segmentCodes |+| y.segmentCodes
      )
  }

}
