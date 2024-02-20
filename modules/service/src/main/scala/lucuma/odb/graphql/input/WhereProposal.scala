// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.odb.graphql.binding._

object WhereProposal {

  def binding(path: Path): Matcher[Predicate] = {

    val WhereTitleBinding             = WhereOptionString.binding(path / "title")
    val WhereProposalReferenceBinding = WhereProposalReference.binding(path / "reference")

    lazy val WhereProposalBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereProposalBinding.List.Option("AND", rAND),
        WhereProposalBinding.List.Option("OR", rOR),
        WhereProposalBinding.Option("NOT", rNOT),
        WhereTitleBinding.Option("title", rTitle),
        WhereProposalReferenceBinding.Option("reference", rRef),
      ) =>
          (rIsNull, rAND, rOR, rNOT, rTitle, rRef).parMapN {
            (isNull, AND, OR, NOT, title, ref) =>
              and(List(
                isNull.map(IsNull(path / "program_id", _)),
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                title,
                ref
              ).flatten)
        }
    }
  }

}
