// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding.*

case class UpdateCallsForProposalsInput(
  SET:            CallForProposalsPropertiesInput.Edit,
  WHERE:          Option[Predicate],
  LIMIT:          Option[NonNegInt],
  includeDeleted: Option[Boolean]
)

object UpdateCallsForProposalsInput {

  def binding(path: Path) = {
    val WhereCallForProposalsBinding = WhereCallForProposals.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        CallForProposalsPropertiesInput.Edit.Binding("SET", rSET),
        WhereCallForProposalsBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }
  }

}
