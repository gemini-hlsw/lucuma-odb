// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.ObscalcState
import lucuma.odb.graphql.binding.*

case class ObscalcUpdateInput(
  programId:     Option[Program.Id],
  observationId: Option[Observation.Id],
  oldState:      Option[WhereOptionEq[ObscalcState]],
  newState:      Option[WhereOptionEq[ObscalcState]]
)

object ObscalcUpdateInput:

  private val WhereOptionEqObscalcState = WhereOptionEq.inputBinding(ObscalcStateBinding)

  val Binding = ObjectFieldsBinding.rmap:
    case List(
      ProgramIdBinding.Option("programId", rProgramId),
      ObservationIdBinding.Option("observationId", rObservationId),
      WhereOptionEqObscalcState.Option("oldState", rOldState),
      WhereOptionEqObscalcState.Option("newState", rNewState)
    ) =>
      (rProgramId, rObservationId, rOldState, rNewState).parMapN(apply)