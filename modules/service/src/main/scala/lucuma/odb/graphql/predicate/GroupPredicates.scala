// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.odb.service.GroupService

class GroupPredicates(path: Path) {
  lazy val id        = LeafPredicates[Group.Id](path / "id")
  lazy val parentId  = LeafPredicates[Group.Id](path / "parentId")
  lazy val program   = ProgramPredicates(path / "program")
  lazy val existence = ExistencePredicates(path / "existence")
  lazy val system    = LeafPredicates[Boolean](path / "system")

  /**
   * The groups the user may edit, as `updateGroups` does: those of programs
   * the user may edit, except system groups (see
   * `GroupService.mayEditSystemGroups`).
   */
  def isEditableBy(user: User): Predicate =
    and(List(
      program.isWritableBy(user),
      if GroupService.mayEditSystemGroups(user) then True else system.eql(false)
    ))
}
