// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.odb.graphql.binding._

final case class UpdateGroupsInput(
  SET:   GroupPropertiesInput.Edit,
  WHERE: Option[Predicate],
  LIMIT: Option[NonNegInt],
)

object UpdateGroupsInput {

  def binding(path: Path): Matcher[UpdateGroupsInput] = {
    val WhereGroupBinding = WhereGroup.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        GroupPropertiesInput.EditBinding("SET", rSET),
        WhereGroupBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
      ) =>
        (rSET, rWHERE, rLIMIT).parMapN(apply)
    }
  }

}
