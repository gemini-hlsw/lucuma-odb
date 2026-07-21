// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package predicate

import grackle.Path
import lucuma.core.model.OrcidId
import lucuma.core.model.User
import lucuma.odb.data.UserType

class UserPredicates(path: Path) {

  lazy val id            = LeafPredicates[User.Id](path / "id")
  lazy val orcidId       = LeafPredicates[OrcidId](path / "orcidId")
  lazy val tpe           = LeafPredicates[UserType](path / "type")
  lazy val enabled       = EnabledPredicates(path / "enabled")

}