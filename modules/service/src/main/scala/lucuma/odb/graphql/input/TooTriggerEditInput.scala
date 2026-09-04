// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import lucuma.core.enums.TooActivation
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.TooTrigger
import lucuma.odb.graphql.binding.*

case class TooTriggerEditInput(
  programId:     Option[Program.Id],
  observationId: Option[Observation.Id],
  tooTriggerId:  Option[TooTrigger.Id],
  tooActivation: Option[WhereOrder[TooActivation]]
)

object TooTriggerEditInput:

  private val TooTriggerIdBinding = gidBinding[TooTrigger.Id]("too trigger")

  // Evaluated in memory against each event rather than compiled into SQL: a
  // subscription has no query to push a predicate into.
  private val WhereOrderTooActivation = WhereOrder.inputBinding(TooActivationBinding)

  val Binding = ObjectFieldsBinding.rmap:
    case List(
      ProgramIdBinding.Option("programId", rProgramId),
      ObservationIdBinding.Option("observationId", rObservationId),
      TooTriggerIdBinding.Option("tooTriggerId", rTooTriggerId),
      WhereOrderTooActivation.Option("tooActivation", rTooActivation)
    ) =>
      (rProgramId, rObservationId, rTooTriggerId, rTooActivation).parMapN(apply)
