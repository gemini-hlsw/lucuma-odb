// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding.*

final case class UpdateProgramNotesInput(
  SET:            ProgramNotePropertiesInput.Edit,
  WHERE:          Option[Predicate],
  LIMIT:          Option[NonNegInt],
  includeDeleted: Option[Boolean]
)

object UpdateProgramNotesInput:

  def binding(path: Path): Matcher[UpdateProgramNotesInput] =
    val WhereProgramNoteBinding = WhereProgramNote.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ProgramNotePropertiesInput.Edit.Binding("SET", rSET),
        WhereProgramNoteBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding.Option("includeDeleted", rIncludeDeleted)
      ) =>
        (rSET, rWHERE, rLIMIT, rIncludeDeleted).parMapN(apply)
    }
