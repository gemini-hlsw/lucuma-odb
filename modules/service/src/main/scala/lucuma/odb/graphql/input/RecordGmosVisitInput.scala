// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class RecordGmosVisitInput[A](
  observationId: Observation.Id,
  static:        A
)

object RecordGmosVisitInput {

  private def binding[A](staticMatcher: Matcher[A]): Matcher[RecordGmosVisitInput[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObservationId),
        staticMatcher("static", rStatic)
      ) =>
        (rObservationId, rStatic).parMapN(RecordGmosVisitInput(_, _))
    }

  val GmosNorthBinding: Matcher[RecordGmosVisitInput[GmosNorth]] =
    binding(GmosNorthStaticInput.Binding)

  val GmosSouthBinding: Matcher[RecordGmosVisitInput[GmosSouth]] =
    binding(GmosSouthStaticInput.Binding)

}