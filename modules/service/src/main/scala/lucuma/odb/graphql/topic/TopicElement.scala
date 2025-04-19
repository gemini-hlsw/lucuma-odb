// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Guest
import lucuma.core.model.Access.Ngo
import lucuma.core.model.Access.Pi
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.User

trait TopicElement:
  def users: List[User.Id]

  def canRead(u: User): Boolean =
    u.role.access match
      case Admin | Service | Staff => true
      case Ngo                     => ??? // TODO
      case Guest | Pi              => users.contains(u.id)