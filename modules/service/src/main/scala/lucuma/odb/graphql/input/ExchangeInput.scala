// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.SubaruInstrument
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.KeckInstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.SubaruInstrumentBinding
import lucuma.odb.sequence.exchange.Config

object ExchangeInput:

  type Create = Config
  val  Create = Config

  case class Edit(
    instrument:       Option[Either[KeckInstrument, SubaruInstrument]],
    totalRequestTime: Option[TimeSpan]
  ):
    def mode: Option[ExchangeObservingModeType] =
      instrument.map(_.fold(_ => ExchangeObservingModeType.ExchangeKeck, _ => ExchangeObservingModeType.ExchangeSubaru))

    def keckInstrument: Option[KeckInstrument] =
      instrument.flatMap(_.swap.toOption)

    def subaruInstrument: Option[SubaruInstrument] =
      instrument.flatMap(_.toOption)

    def toCreate: Result[Create] =
      Result.fromOption(
        (instrument, totalRequestTime).mapN((i, t) => Create(i, t)),
        "Cannot turn edit into create. Either `keckInstrument` or `subaruInstrument`, along with `totalRequestTime` must be defined."
      )

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        KeckInstrumentBinding.Option("keckInstrument", rKeck),
        SubaruInstrumentBinding.Option("subaruInstrument", rSubaru),
        TimeSpanInput.Binding("totalRequestTime", rTotalRequestTime)
      ) =>
        val rInstrument = (rKeck, rSubaru).parTupled.flatMap:
          case (Some(k), None) => Left(k).success
          case (None, Some(s)) => Right(s).success
          case _               => Matcher.validationFailure("Exactly one of 'keckInstrument' or 'subaruInstrument' must be provided.")
        (rInstrument, rTotalRequestTime).mapN(Create.apply)

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        KeckInstrumentBinding.Option("keckInstrument", rKeck),
        SubaruInstrumentBinding.Option("subaruInstrument", rSubaru),
        TimeSpanInput.Binding.Option("totalRequestTime", rTotalRequestTime)
      ) =>
        val rInstrument = (rKeck, rSubaru).parTupled.flatMap:
          case (Some(k), None) => Some(Left(k)).success
          case (None, Some(s)) => Some(Right(s)).success
          case (None, None)    => None.success
          case _               => Matcher.validationFailure("At most one of 'keckInstrument' or 'subaruInstrument' may be provided.")
        (rInstrument, rTotalRequestTime).mapN(Edit.apply)