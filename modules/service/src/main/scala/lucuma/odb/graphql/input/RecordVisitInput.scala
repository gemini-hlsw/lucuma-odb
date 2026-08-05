// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

case class RecordVisitInput(
  observationId:  Observation.Id,
  clientTime:     Option[Timestamp],
  idempotencyKey: Option[IdempotencyKey]
)

object RecordVisitInput:

  private def binding[A](
    instrumentName: String,
    staticMatcher:  Matcher[A]
  ): Matcher[RecordVisitInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding("observationId", rObservationId),
        staticMatcher(`instrumentName`, rStatic),
        TimestampBinding.Option("clientTime", rClientTime),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rObservationId, rStatic, rClientTime, rIdm).parMapN((obs, _, clientTime, idm) => RecordVisitInput(obs, clientTime, idm))

  val Flamingos2Binding: Matcher[RecordVisitInput] =
    binding("flamingos2", Flamingos2StaticInput.Binding)

  val GmosNorthBinding: Matcher[RecordVisitInput] =
    binding("gmosNorth", GmosNorthStaticInput.Binding)

  val GmosSouthBinding: Matcher[RecordVisitInput] =
    binding("gmosSouth", GmosSouthStaticInput.Binding)

  val Igrins2Binding: Matcher[RecordVisitInput] =
    binding("igrins2", Igrins2StaticInput.Binding)

  val Binding: Matcher[RecordVisitInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding("observationId", rObservationId),
        TimestampBinding.Option("clientTime", rClientTime),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rObservationId, rClientTime, rIdm).parMapN(RecordVisitInput(_, _, _))