// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.PartnerLinkType
import lucuma.core.model.PartnerLink
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.graphql.binding.ExchangePartnerBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PartnerBinding
import lucuma.odb.graphql.binding.PartnerLinkTypeBinding


object PartnerLinkInput:

  private val InvalidResult: Result[Nothing] =
    OdbError.InvalidArgument(s"Specify either 'linkType' (as `${PartnerLinkType.HasNonPartner.tag.toScreamingSnakeCase}` or `${PartnerLinkType.HasUnspecifiedPartner.tag.toScreamingSnakeCase}`), 'partner', or 'exchangePartner'.".some).asFailure

  val Binding: Matcher[PartnerLink] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerLinkTypeBinding.Option("linkType", rLinkType),
        PartnerBinding.Option("geminiPartner", rPartner),
        ExchangePartnerBinding.Option("exchangePartner", rExchange)
      ) =>
        (rLinkType, rPartner, rExchange).parTupled.flatMap:
          case (None,    Some(p), None)    => PartnerLink.HasGeminiPartner(p).success
          case (None,    None,    Some(e)) => PartnerLink.HasExchangePartner(e).success
          case (None,    None,    None)    => InvalidResult
          case (Some(t), None,    None)    => t match {
            case PartnerLinkType.HasUnspecifiedPartner => PartnerLink.HasUnspecifiedPartner.success
            case PartnerLinkType.HasNonPartner         => PartnerLink.HasNonPartner.success
            case PartnerLinkType.HasGeminiPartner      => InvalidResult
            case PartnerLinkType.HasExchangePartner    => InvalidResult
          }
          case (Some(PartnerLinkType.HasGeminiPartner),   Some(p), None)    => PartnerLink.HasGeminiPartner(p).success
          case (Some(PartnerLinkType.HasExchangePartner), None,    Some(e)) => PartnerLink.HasExchangePartner(e).success
          case _                                                            => InvalidResult