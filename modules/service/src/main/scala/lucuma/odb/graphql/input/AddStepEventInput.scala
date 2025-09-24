// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey
import lucuma.odb.graphql.binding.*

case class AddStepEventInput(
  stepId:         Step.Id,
  stepStage:      StepStage,
  idempotencyKey: Option[IdempotencyKey]
)

object AddStepEventInput:

  val Binding: Matcher[AddStepEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        StepIdBinding("stepId", rStepId),
        StepStageBinding("stepStage", rStepStage),
        ClientIdBinding.Option("clientId", rCid),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rStepId, rStepStage, rCid, rIdm).parMapN: (sid, stage, cid, idm) =>

          AddStepEventInput(sid, stage, idm orElse cid.map(c => IdempotencyKey(c.toUuid)))