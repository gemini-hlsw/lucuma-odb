// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

final case class ObservationEditInput(
  observationId: Option[Observation.Id],
  programId:     Option[Program.Id]
)

object ObservationEditInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      ObservationIdBinding.Option("observationId", rObservationId),
      ProgramIdBinding.Option("programId", rProgramId)
    ) =>
      (rObservationId, rProgramId).mapN(apply)
  }

}