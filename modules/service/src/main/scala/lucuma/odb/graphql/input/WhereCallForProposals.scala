// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.CallForProposals
import lucuma.odb.graphql.binding.*

object WhereCallForProposals {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereIdBinding = WhereOrder.binding[CallForProposals.Id](path / "id", CallForProposalsIdBinding)

    lazy val WhereCfpBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereCfpBinding.List.Option("AND", rAND),
        WhereCfpBinding.List.Option("OR", rOR),
        WhereCfpBinding.Option("NOT", rNOT),

        WhereIdBinding.Option("id", rId),
      ) =>
        (rAND, rOR, rNOT, rId).parMapN { (AND, OR, NOT, id) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
          ).flatten)
        }
    }
  }

}

