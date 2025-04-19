// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

case class AllocationInput(
  category:    TimeAccountingCategory,
  scienceBand: ScienceBand,
  duration:    TimeSpan
)

object AllocationInput {

  val Binding: Matcher[AllocationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeAccountingCategoryBinding("category", rCategory),
        ScienceBandBinding("scienceBand", rBand),
        TimeSpanInput.Binding("duration", rDuration),
      ) => (rCategory, rBand, rDuration).mapN(apply)
    }

}

