// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.core.model.Partner
import lucuma.core.model.NonNegDuration
import lucuma.odb.graphql.util.Bindings._
import edu.gemini.grackle.Result

case class SetAllocationInput(
  programId: Program.Id,
  partner: Partner,
  duration: NonNegDuration,
)

object SetAllocationInput {

  val Binding =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        PartnerBinding("partner", rPartner),
        NonNegDurationInput.Binding("duration", rDuration),
      ) =>
        (rProgramId, rPartner, rDuration).mapN(apply)
    }

}