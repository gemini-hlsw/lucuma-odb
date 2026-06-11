// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereGeminiCallProperties:

  def binding(path: Path): Matcher[Predicate] =
    val WhereTypeBinding      = WhereEq.binding(path / "type", CallForProposalsTypeBinding)
    val WhereAllowsNonPartner = WhereBoolean.binding(path / "allowsNonPartnerPi", BooleanBinding)

    ObjectFieldsBinding.rmap:
      case List(
        WhereTypeBinding.Option("type", rType),
        WhereAllowsNonPartner.Option("allowsNonPartnerPi", rNonPartner)
      ) =>
        (rType, rNonPartner).parMapN: (cfpType, nonPartner) =>
          and(List(cfpType, nonPartner).flatten)