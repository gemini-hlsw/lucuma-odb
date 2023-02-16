// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding._

final case class CloneObservationInput(
  observationId: Observation.Id,
  SET:           Option[ObservationPropertiesInput.Edit],
)

object CloneObservationInput {

 val Binding: Matcher[CloneObservationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObservationId),
        ObservationPropertiesInput.Edit.Binding.Option("SET", rSET),
      ) =>
        (rObservationId, rSET).mapN(CloneObservationInput.apply)
    }

}
