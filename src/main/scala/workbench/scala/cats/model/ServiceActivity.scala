package workbench.scala.cats.model

import workbench.scala.cats.model.ServiceActivity.{CategoryCodes, SegmentCodes}

final case class CategoryCode(value: Int)

final case class SegmentCode(value: Int)

final case class ServiceActivity(value: String,
                                 nlCategoryCode: CategoryCodes,
                                 segmentCodes: SegmentCodes)

object ServiceActivity {

  type CategoryCodes = Set[CategoryCode]

  type SegmentCodes = Set[SegmentCode]

}
