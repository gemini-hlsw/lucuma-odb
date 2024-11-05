// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.*

final case class SetObservationWorkflowStateInput(
  observationId: Observation.Id,
  state: ObservationWorkflowState
)

object SetObservationWorkflowStateInput {
  val Binding: Matcher[SetObservationWorkflowStateInput] =
    val ObservationWorkflowStateBinding: Matcher[ObservationWorkflowState] = enumeratedBinding
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rOid),
        ObservationWorkflowStateBinding("state", rState),
      ) => (rOid, rState).parMapN(apply)
    }
}
