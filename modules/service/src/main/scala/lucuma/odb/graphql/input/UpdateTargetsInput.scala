// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.odb.graphql.binding._

final case class UpdateTargetsInput(
  SET:            TargetPropertiesInput.Edit,
  WHERE:          Option[Predicate],
  LIMIT:          Option[NonNegInt],
  includeDeleted: Option[Boolean]
)

object UpdateTargetsInput {

  def binding(path: Path): Matcher[UpdateTargetsInput] = {
    val WhereTargetBinding = WhereTarget.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        TargetPropertiesInput.EditBinding("SET", rSET),
        WhereTargetBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }
  }

}
