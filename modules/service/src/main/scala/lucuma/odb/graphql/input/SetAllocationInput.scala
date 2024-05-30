// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

case class SetAllocationInput(
  programId: Program.Id,
  partner: Partner,
  duration: TimeSpan
)

object SetAllocationInput {

  val Binding: Matcher[SetAllocationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        PartnerBinding("partner", rPartner),
        TimeSpanInput.Binding("duration", rDuration),
      ) =>
        (rProgramId, rPartner, rDuration).mapN(apply)
    }

}
