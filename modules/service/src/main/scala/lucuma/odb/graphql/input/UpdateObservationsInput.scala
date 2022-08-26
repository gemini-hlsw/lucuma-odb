// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings.*

final case class UpdateObservationsInput(
  programId:      Program.Id,
  SET:            ObservationPropertiesInput,
  WHERE:          Option[Predicate],
  LIMIT:          Option[NonNegInt],
  includeDeleted: Option[Boolean]
) {

  def asterism: Nullable[NonEmptyList[Target.Id]] =
    SET.asterism

}

object UpdateObservationsInput {

  val Binding: Matcher[UpdateObservationsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid),
        ObservationPropertiesInput.EditBinding("SET", rSET),
        WhereObservation.Binding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rPid, rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }

}
