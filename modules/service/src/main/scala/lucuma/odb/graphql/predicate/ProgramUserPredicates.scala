// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import grackle.Predicate
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole

class ProgramUserPredicates(path: Path) {

  lazy val role = LeafPredicates[ProgramUserRole](path / "role")
  lazy val user = LeafPredicates[User.Id](path / "userId")

  def isPi: Predicate =
    role.eql(ProgramUserRole.Pi)

  def isNotPi: Predicate =
    role.neql(ProgramUserRole.Pi)

}