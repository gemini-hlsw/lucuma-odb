// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.syntax.option.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.core.model.StandardRole
import lucuma.core.model.User

class ProgramUserPredicates(path: Path) {

  private val programIdPath: Path = path / "programId"
  private val userIdPath: Path    = path / "userId"

  lazy val id            = LeafPredicates[ProgramUser.Id](path / "id")
  lazy val program       = ProgramPredicates(path / "program")
  lazy val programId     = LeafPredicates[Program.Id](programIdPath)
  lazy val role          = LeafPredicates[ProgramUserRole](path / "role")
  lazy val userId        = LeafPredicates[User.Id](userIdPath)
  lazy val hasDataAccess = LeafPredicates[Boolean](path / "hasDataAccess")

  def isPi: Predicate =
    role.eql(ProgramUserRole.Pi)

  def isNotPi: Predicate =
    role.neql(ProgramUserRole.Pi)

  /** Program writers may edit any member; other members only their own entry. */
  def isWritableBy(user: User): Predicate =
    user.role match
      case StandardRole.Ngo(_, _) => False
      case _                      => Or(program.isWritableBy(user), Eql(userIdPath, Const(user.id.some)))

}