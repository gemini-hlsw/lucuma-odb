// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceBand
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

case class AllocationInput(
  partner:     Partner,
  scienceBand: ScienceBand,
  duration:    TimeSpan
)

object AllocationInput {

  val Binding: Matcher[AllocationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        PartnerBinding("partner", rPartner),
        ScienceBandBinding("scienceBand", rBand),
        TimeSpanInput.Binding("duration", rDuration),
      ) => (rPartner, rBand, rDuration).mapN(apply)
    }

}

