// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.odb.graphql.binding.*

object WherePartnerLink:

  def binding(path: Path): Matcher[Predicate] =
    val WherePartnerLinkType = WhereEq.binding[PartnerLinkType](path / "linkType", PartnerLinkTypeBinding)
    val WherePartner         = WhereOptionEq.binding[Partner](path / "geminiPartner", PartnerBinding)
    val WhereExchangePartner = WhereOptionEq.binding[ExchangePartner](path / "exchangePartner", ExchangePartnerBinding)

    ObjectFieldsBinding.rmap:
      case List(
        WherePartnerLinkType.Option("linkType", rLinkType),
        WherePartner.Option("geminiPartner", rGeminiPartner),
        WhereExchangePartner.Option("exchangePartner", rExchangePartner)
      ) =>
        (rLinkType, rGeminiPartner, rExchangePartner).parMapN: (linkType, geminiPartner, exchangePartner) =>
          and(List(linkType, geminiPartner, exchangePartner).flatten)