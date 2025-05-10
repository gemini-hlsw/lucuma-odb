// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.odb.graphql.binding.*

case class RecordFlamingos2VisitInput(
  observationId: Observation.Id,
  static:        F2StaticConfig
)

object RecordFlamingos2VisitInput:

  val Binding: Matcher[RecordFlamingos2VisitInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding("observationId", rObservationId),
        Flamingos2StaticInput.Binding("flamingos2", rStatic)
      ) =>
        (rObservationId, rStatic).parMapN(RecordFlamingos2VisitInput(_, _))