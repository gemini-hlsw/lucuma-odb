// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object GeminiCallPropertiesInput:

  case class Create(
    cfpType:          GeminiCallForProposalsType,
    coordinateLimits: SiteCoordinateLimitsInput.Create,
    instruments:      List[Instrument],
    proprietary:      Option[NonNegInt],
    exchangePartners: List[ExchangePartner]
  )

  object Create:

    val Binding: Matcher[DateInterval => Create] =
      ObjectFieldsBinding.rmap:
        case List(
          GeminiCallForProposalsTypeBinding.Option("type", rType),
          SiteCoordinateLimitsInput.Create.Binding.Option("coordinateLimits", rLimits),
          InstrumentBinding.List.Option("instruments", rInstruments),
          NonNegIntBinding.Option("proprietaryMonths", rProprietary),
          ExchangePartnerBinding.List.Option("exchangePartners", rExchange)
        ) =>
          val rTypeʹ        = rType.flatMap(Result.fromOption(_, Matcher.validationProblem("'type' is required for Gemini calls.")))
          val rInstrumentsʹ = dedup("instruments",      rInstruments)(_.tag.toScreamingSnakeCase).map(_.toList.flatten)
          val rExchangeʹ    = dedup("exchangePartners", rExchange)(_.tag.toScreamingSnakeCase).map(_.toList.flatten)
          (
            rTypeʹ,
            rLimits,
            rInstrumentsʹ,
            rProprietary,
            rExchangeʹ
          ).parMapN: (cfpType, limits, instruments, proprietary, exchange) =>
            (active: DateInterval) =>
              Create(
                cfpType,
                limits.fold(SiteCoordinateLimitsInput.Create.default(active))(f => f(active)),
                instruments,
                proprietary,
                exchange
              )

  case class Edit(
    cfpType:          Option[GeminiCallForProposalsType],
    coordinateLimits: Option[SiteCoordinateLimitsInput.Edit],
    instruments:      Nullable[List[Instrument]],
    proprietary:      Option[NonNegInt],
    exchangePartners: Nullable[List[ExchangePartner]]
  )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          GeminiCallForProposalsTypeBinding.NonNullable("type", rType),
          SiteCoordinateLimitsInput.Edit.Binding.Option("coordinateLimits", rLimits),
          InstrumentBinding.List.Nullable("instruments", rInstruments),
          NonNegIntBinding.Option("proprietaryMonths", rProprietary),
          ExchangePartnerBinding.List.Nullable("exchangePartners", rExchange)
        ) =>
          val rInstrumentsʹ = dedup("instruments",      rInstruments)(_.tag.toScreamingSnakeCase)
          val rExchangeʹ    = dedup("exchangePartners", rExchange)(_.tag.toScreamingSnakeCase)
          (
            rType,
            rLimits,
            rInstrumentsʹ,
            rProprietary,
            rExchangeʹ
          ).parMapN(Edit.apply)