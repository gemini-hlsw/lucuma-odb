// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Path
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

  def binding(path: Path) = {
    val WhereProgramBinding = WhereProgram.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ProgramPropertiesInput.EditBinding("SET", rSET),
        WhereProgramBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }
  }

}
