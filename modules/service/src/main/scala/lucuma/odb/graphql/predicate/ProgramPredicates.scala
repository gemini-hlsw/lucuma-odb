// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Access.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.User

class ProgramPredicates(path: Path) {

  lazy val existence      = ExistencePredicates(path / "existence")
  lazy val id             = LeafPredicates[Program.Id](path / "id")
  lazy val referenceLabel = LeafPredicates[ProgramReference](path / "reference" / "label")
  lazy val proposal       = new ProposalPredicates(path / "proposal")

  def isVisibleTo(user: User): Predicate =
    user.role.access match {
      case Guest | Pi =>
        Contains(path / "users" / "userId", Const(user.id)) // user is linked
      case Ngo => ???
      case Staff | Admin | Service => True
    }

  def isWritableBy(user: User): Predicate =
    isVisibleTo(user) // TODO: not true for COI_RO

}
