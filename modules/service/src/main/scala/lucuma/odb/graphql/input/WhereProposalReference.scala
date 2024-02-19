// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.odb.data.ProposalReference
import lucuma.odb.graphql.binding._

object WhereProposalReference {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderProposalReferenceLabel = WhereOrder.binding[ProposalReference](path / "label", ProposalReferenceBinding)

    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereOrderProposalReferenceLabel.Option("label", rRef)
      ) => (rIsNull, rRef).parMapN { (isNull, ref) =>
        and(List(
          isNull.map(IsNull(path / "id", _)),
          ref
        ).flatten)
      }
    }
  }

}
