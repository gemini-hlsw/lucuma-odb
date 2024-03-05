// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding._


//# Input for bulk updating multiple observations.  Select observations
//# with the 'WHERE' input and specify the changes in 'SET'.
//#
//input UpdateAsterismsInput {
//
//  # Describes the values to modify.
//  SET: EditAsterismsPatchInput!
//
//  # Filters the observations to be updated according to those that match the given constraints.
//  WHERE: WhereObservation
//
//  # Caps the number of results returned to the given value (if additional observations match the WHERE clause they will be updated but not returned).
//  LIMIT: NonNegInt
//}

final case class UpdateAsterismsInput(
  SET:            EditAsterismsPatchInput,
  WHERE:          Option[Predicate],
  LIMIT:          Option[NonNegInt],
  includeDeleted: Option[Boolean]
)

object UpdateAsterismsInput {

  def binding(path: Path): Matcher[UpdateAsterismsInput] = {
    val WhereObservationBinding = WhereObservation.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        EditAsterismsPatchInput.Binding("SET", rSET),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }
  }

}