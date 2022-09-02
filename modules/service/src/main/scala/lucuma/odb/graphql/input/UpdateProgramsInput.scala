// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.odb.graphql.binding._

case class UpdateProgramsInput(
  SET: ProgramPropertiesInput.Edit,
  WHERE: Option[Predicate],
  LIMIT: Option[NonNegInt],
  includeDeleted: Option[Boolean],
)

object UpdateProgramsInput {

  val Binding =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramPropertiesInput.EditBinding("SET", rSET),
        WhereProgram.Binding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }

}
