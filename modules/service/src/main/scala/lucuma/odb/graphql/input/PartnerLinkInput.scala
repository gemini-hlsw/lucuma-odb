// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.PartnerLink
import lucuma.odb.data.PartnerLink.LinkType
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PartnerBinding
import lucuma.odb.graphql.binding.PartnerLinkTypeBinding


object PartnerLinkInput:

  private val InvalidResult: Result[Nothing] =
    OdbError.InvalidArgument(s"Specify either 'linkType' (as `${LinkType.HasNonPartner.tag.toScreamingSnakeCase}` or `${LinkType.HasUnspecifiedPartner.tag.toScreamingSnakeCase}`) or 'partner'.".some).asFailure

  val Binding: Matcher[PartnerLink] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerLinkTypeBinding.Option("linkType", rLinkType),
        PartnerBinding.Option("partner", rPartner)
      ) =>
        (rLinkType, rPartner).parTupled.flatMap:
          case (None,    None)        => InvalidResult
          case (None,    Some(p))     => PartnerLink.HasPartner(p).success
          case (Some(t), None)        => t match {
            case LinkType.HasUnspecifiedPartner => PartnerLink.HasUnspecifiedPartner.success
            case LinkType.HasNonPartner         => PartnerLink.HasNonPartner.success
            case LinkType.HasPartner            => InvalidResult
          }
          case (Some(t), Some(p))     => t match {
            case LinkType.HasUnspecifiedPartner => InvalidResult
            case LinkType.HasNonPartner         => InvalidResult
            case LinkType.HasPartner            => PartnerLink.HasPartner(p).success
          }