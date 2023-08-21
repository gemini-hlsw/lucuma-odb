// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Staff
import lucuma.core.model.StandardRole.Admin
import lucuma.odb.data.Tag

class ProgramPredicates(path: Path) {

  lazy val existence   = ExistencePredicates(path / "existence")
  lazy val id          = LeafPredicates[Program.Id](path / "id")
  lazy val piUserId    = LeafPredicates[User.Id](path / "piUserId")
  lazy val allocations = AllocationPredicates(path / "allocations")

  def isVisibleTo(user: User): Predicate =
    user match
      case GuestUser(id)                    => piUserId.eql(user.id) // user is the PI
      case ServiceUser(_, _)                => True
      case StandardUser(_, Staff(_), _, _)  => True
      case StandardUser(_, Admin(_), _, _)  => True
      case StandardUser(_, Ngo(_, p), _, _) => allocations.partner.contains(Tag(p.tag)) // partner must have allocated time
      case StandardUser(id, Pi(_), _, _)    => 
        Or(
          Contains(path / "users" / "userId", Const(user.id)), // user is linked, or
          piUserId.eql(user.id)                                // user is the PI
        )          

  def isWritableBy(user: User): Predicate =
    isVisibleTo(user) // this is true for now

}
