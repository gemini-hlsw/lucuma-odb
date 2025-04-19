// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

case class AddStepEventInput(
  stepId:       Step.Id,
  stepStage:    StepStage
)

object AddStepEventInput {

  val Binding: Matcher[AddStepEventInput] =
    ObjectFieldsBinding.rmap {
      case List(
        StepIdBinding("stepId", rStepId),
        StepStageBinding("stepStage", rStepStage)
      ) =>
        (rStepId, rStepStage).parMapN { (sid, stage) =>
          AddStepEventInput(sid, stage)
        }
    }

}