// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.syntax.order.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Access.*
import lucuma.core.model.ProgramNote
import lucuma.core.model.User

class ProgramNotePredicates(path: Path):

  lazy val existence = ExistencePredicates(path / "existence")
  lazy val id        = LeafPredicates[ProgramNote.Id](path / "id")
  lazy val program   = ProgramPredicates(path / "program")

  def isVisibleTo(user: User): Predicate =
    And(
      program.isVisibleTo(user),
      Or(
        Eql(path / "isPrivate", Const(false)),
        if user.role.access >= Staff then True else False
      )
    )