// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.TooTrigger
import lucuma.odb.graphql.binding.*

case class TooTriggerEditInput(
  programId:     Option[Program.Id],
  observationId: Option[Observation.Id],
  tooTriggerId:  Option[TooTrigger.Id]
)

object TooTriggerEditInput:

  private val TooTriggerIdBinding = gidBinding[TooTrigger.Id]("too trigger")

  val Binding = ObjectFieldsBinding.rmap:
    case List(
      ProgramIdBinding.Option("programId", rProgramId),
      ObservationIdBinding.Option("observationId", rObservationId),
      TooTriggerIdBinding.Option("tooTriggerId", rTooTriggerId)
    ) =>
      (rProgramId, rObservationId, rTooTriggerId).parMapN(apply)
