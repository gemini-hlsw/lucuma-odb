// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.syntax.option.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.sql.Like
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.model.User

class ProgramPredicates(path: Path) {

  lazy val existence      = ExistencePredicates(path / "existence")
  lazy val id             = LeafPredicates[Program.Id](path / "id")
  lazy val referenceLabel = LeafPredicates[ProgramReference](path / "reference" / "label")
  lazy val proposal       = new ProposalPredicates(path / "proposal")

  def isVisibleTo(user: User): Predicate =
    user.role match {
      case GuestRole | StandardRole.Pi(_) =>
        Contains(path / "users" / "userId", Const(user.id.some)) // user is linked
      case StandardRole.Ngo(_, partner) =>
        // partner requests time or holds an allocation (see V1370)
        Like(path / "ngoPartners", s"%,${partner.tag},%", false)
      case StandardRole.Staff(_) | StandardRole.Admin(_) | ServiceRole(_) => True
    }

  def isWritableBy(user: User): Predicate =
    user.role match {
      case StandardRole.Ngo(_, _) => False // NGO access is read-only
      case _                      => isVisibleTo(user) // TODO: not true for COI_RO
    }

}
