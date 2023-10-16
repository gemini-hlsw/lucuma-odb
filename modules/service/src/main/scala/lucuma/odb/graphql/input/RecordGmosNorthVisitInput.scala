// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.odb.graphql.binding.*

case class RecordGmosNorthVisitInput(
  observationId: Observation.Id,
  static:        GmosNorth
)

object RecordGmosNorthVisitInput {

  val Binding: Matcher[RecordGmosNorthVisitInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObservationId),
        GmosNorthStaticInput.Binding("static", rGmosNorth)
      ) =>
        (rObservationId, rGmosNorth).parMapN(RecordGmosNorthVisitInput(_, _))
    }

} 