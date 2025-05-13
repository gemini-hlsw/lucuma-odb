// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class RecordVisitInput[A](
  observationId: Observation.Id,
  static:        A
)

object RecordVisitInput {

  private def binding[A](
    instrumentName: String,
    staticMatcher:  Matcher[A]
  ): Matcher[RecordVisitInput[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObservationId),
        staticMatcher(`instrumentName`, rStatic)
      ) =>
        (rObservationId, rStatic).parMapN(RecordVisitInput(_, _))
    }

  val Flamingos2Binding: Matcher[RecordVisitInput[F2StaticConfig]] =
    binding("flamingos2", Flamingos2StaticInput.Binding)

  val GmosNorthBinding: Matcher[RecordVisitInput[GmosNorth]] =
    binding("gmosNorth", GmosNorthStaticInput.Binding)

  val GmosSouthBinding: Matcher[RecordVisitInput[GmosSouth]] =
    binding("gmosSouth", GmosSouthStaticInput.Binding)

}