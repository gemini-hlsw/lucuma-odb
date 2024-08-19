// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Cursor
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Result
import grackle.Term
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

  case object keyTerm extends Term[(Program.Id, User.Id)] {
    def apply(c: Cursor): Result[(Program.Id, User.Id)] =
      for {
        p <- programIdPath.asTerm[Program.Id](c)
        u <- userIdPath.asTerm[User.Id](c)
      } yield (p, u)

    override def children =
      List(programIdPath, userIdPath)
  }

  object key {
    def eql(a: (Program.Id, User.Id)): Predicate =
      Eql(keyTerm, Const(a))

    def neql(a: (Program.Id, User.Id)): Predicate =
      NEql(keyTerm, Const(a))

    def in(as: List[(Program.Id, User.Id)]): Predicate =
      In(keyTerm, as)
  }

  def isPi: Predicate =
    role.eql(ProgramUserRole.Pi)

  def isNotPi: Predicate =
    role.neql(ProgramUserRole.Pi)

}