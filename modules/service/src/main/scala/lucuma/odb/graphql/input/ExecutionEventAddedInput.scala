// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.odb.data.ExecutionEventType
import lucuma.odb.graphql.binding.*

case class ExecutionEventAddedInput(
  programId:     Option[Program.Id],
  observationId: Option[Observation.Id],
  visitId:       Option[Visit.Id],
  eventType:     Option[WhereEq[ExecutionEventType]]
)

object ExecutionEventAddedInput:

  private val WhereEqExecutionEventType = WhereEq.inputBinding(ExecutionEventTypeBinding)

  val Binding = ObjectFieldsBinding.rmap:
    case List(
      ProgramIdBinding.Option("programId", rProgramId),
      ObservationIdBinding.Option("observationId", rObservationId),
      VisitIdBinding.Option("visitId", rVisitId),
      WhereEqExecutionEventType.Option("eventType", rEventType)
    ) =>
      (rProgramId, rObservationId, rVisitId, rEventType).parMapN(apply)