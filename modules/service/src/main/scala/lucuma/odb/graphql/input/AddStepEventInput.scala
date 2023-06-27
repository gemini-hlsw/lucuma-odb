// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

case class AddStepEventInput(
  stepId:       Step.Id,
  sequenceType: SequenceType,
  stepStage:    StepStage
)

object AddStepEventInput {

  val Binding: Matcher[AddStepEventInput] =
    ObjectFieldsBinding.rmap {
      case List(
        StepIdBinding("stepId", rStepId),
        SequenceTypeBinding("sequenceType", rSequenceType),
        StepStageBinding("stepStage", rStepStage)
      ) =>
        (rStepId, rSequenceType, rStepStage).parMapN { (sid, stype, stage) =>
          AddStepEventInput(sid, stype, stage)
        }
    }

}