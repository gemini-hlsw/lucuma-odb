// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class RecordGmosSouthVisitInput(
  observationId: Observation.Id,
  static:        GmosSouth
)

object RecordGmosSouthVisitInput {

  val Binding: Matcher[RecordGmosSouthVisitInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObservationId),
        GmosSouthStaticInput.Binding("static", rGmosSouth)
      ) =>
        (rObservationId, rGmosSouth).parMapN(RecordGmosSouthVisitInput(_, _))
    }

} 