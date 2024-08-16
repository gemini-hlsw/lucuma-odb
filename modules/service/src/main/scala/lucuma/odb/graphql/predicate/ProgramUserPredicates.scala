// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Cursor
import grackle.Path
import grackle.Predicate
import grackle.Result
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole

class ProgramUserPredicates(path: Path) {

  private val programIdPath: Path = path / "programId"
  private val userIdPath: Path    = path / "userId"

  lazy val program   = ProgramPredicates(path / "program")
  lazy val programId = LeafPredicates[Program.Id](programIdPath)
  lazy val role      = LeafPredicates[ProgramUserRole](path / "role")
  lazy val userId    = LeafPredicates[User.Id](userIdPath)

  case class In(ids: List[(Program.Id, User.Id)]) extends Predicate {
    override def apply(c: Cursor): Result[Boolean] =
      for {
        p <- programIdPath.asTerm[Program.Id](c)
        u <- userIdPath.asTerm[User.Id](c)
      } yield ids.contains((p, u))

    override def children = Nil
  }

  def isPi: Predicate =
    role.eql(ProgramUserRole.Pi)

  def isNotPi: Predicate =
    role.neql(ProgramUserRole.Pi)

}