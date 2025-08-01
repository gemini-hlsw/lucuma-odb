// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.StepStage
import lucuma.core.model.Client
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

case class AddStepEventInput(
  stepId:    Step.Id,
  stepStage: StepStage,
  clientId:  Option[Client.Id]
)

object AddStepEventInput:

  val Binding: Matcher[AddStepEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        StepIdBinding("stepId", rStepId),
        StepStageBinding("stepStage", rStepStage),
        ClientIdBinding.Option("clientId", rCid)
      ) =>
        (rStepId, rStepStage, rCid).parMapN: (sid, stage, cid) =>
          AddStepEventInput(sid, stage, cid)