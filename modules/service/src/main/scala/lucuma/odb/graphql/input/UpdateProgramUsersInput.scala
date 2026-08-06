// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding.*

case class UpdateProgramUsersInput(
  SET:   ProgramUserPropertiesInput,
  WHERE: Option[Predicate],
  LIMIT: Option[NonNegInt]
)

object UpdateProgramUsersInput {

  def binding(path: Path): Matcher[UpdateProgramUsersInput] =
    val WhereProgramUsersBinding = WhereProgramUser.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ProgramUserPropertiesInput.Binding("SET", rSET),
        WhereProgramUsersBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rSET, rWHERE, rLIMIT).parMapN(apply)
    }

}
