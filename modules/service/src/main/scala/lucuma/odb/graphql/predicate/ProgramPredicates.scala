// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.Access._
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.ProgramReference

class ProgramPredicates(path: Path) {

  lazy val existence         = ExistencePredicates(path / "existence")
  lazy val id                = LeafPredicates[Program.Id](path / "id")
  lazy val programReference  = LeafPredicates[Option[ProgramReference]](path / "programReference")
  lazy val proposal          = new ProposalPredicates(path / "proposal")
  lazy val piUserId          = LeafPredicates[User.Id](path / "piUserId")

  def isVisibleTo(user: User): Predicate =
    user.role.access match {
      case Guest | Pi =>
        Or(
          Contains(path / "users" / "userId", Const(user.id)), // user is linked, or
          piUserId.eql(user.id)            // user is the PI
        )
      case Ngo => ???
      case Staff | Admin | Service => True
    }

  def isWritableBy(user: User): Predicate =
    isVisibleTo(user) // this is true for now

}
