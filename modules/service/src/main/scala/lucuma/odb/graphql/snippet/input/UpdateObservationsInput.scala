// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.parallel._
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.odb.graphql.util.Bindings._

final case class UpdateObservationsInput(
  SET:            ObservationPropertiesInput,
  WHERE:          Option[List[Observation.Id]], // temporary, replace with WHERE clause
  LIMIT:          Option[NonNegInt] ,
  includeDeleted: Option[Boolean]
)

object UpdateObservationsInput {

  val Binding: Matcher[UpdateObservationsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationPropertiesInput.EditBinding("SET", rSET),
        ObservationIdBinding.List.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }

}
