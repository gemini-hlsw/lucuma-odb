// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.ProgramNote
import lucuma.odb.graphql.binding.*

object WhereProgramNote {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderProgramNoteId      = WhereOrder.binding[ProgramNote.Id](path / "id", ProgramNoteIdBinding)
    val WhereProgramBinding          = WhereProgram.binding(path / "program")
    val WhereTitleBinding            = WhereString.binding(path / "title")
    val WhereTextBinding             = WhereOptionString.binding(path / "text")
    val WhereIsPrivateBinding        = WhereBoolean.binding(path / "isPrivate", BooleanBinding)

    lazy val WhereProgramNoteBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramNoteBinding.List.Option("AND", rAND),
        WhereProgramNoteBinding.List.Option("OR", rOR),
        WhereProgramNoteBinding.Option("NOT", rNOT),
        WhereOrderProgramNoteId.Option("id", rId),
        WhereProgramBinding.Option("program", rProgram),
        WhereTitleBinding.Option("title", rTitle),
        WhereTextBinding.Option("text", rText),
        WhereIsPrivateBinding.Option("isPrivate", rIsPrivate)
      ) =>
          (rAND, rOR, rNOT, rId, rProgram, rTitle, rText, rIsPrivate).parMapN {
            (AND, OR, NOT, id, program, title, text, isPrivate) =>
              and(List(
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                id,
                program,
                title,
                text,
                isPrivate
              ).flatten)
        }
    }
  }

}
