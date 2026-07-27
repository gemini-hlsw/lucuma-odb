// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.StepStage
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

case class AddStepEventInput(
  stepId:         Step.Id,
  visitId:        Visit.Id,
  stepStage:      StepStage,
  time:           Option[Timestamp],
  idempotencyKey: Option[IdempotencyKey]
)

object AddStepEventInput:

  val Binding: Matcher[AddStepEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        StepIdBinding("stepId", rStepId),
        VisitIdBinding("visitId", rVisitId),
        StepStageBinding("stepStage", rStepStage),
        TimestampBinding.Option("time", rTime),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rStepId, rVisitId, rStepStage, rTime, rIdm).parMapN: (sid, vid, stage, time, idm) =>
          AddStepEventInput(sid, vid, stage, time, idm)