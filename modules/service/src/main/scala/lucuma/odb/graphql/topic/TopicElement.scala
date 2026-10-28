// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import lucuma.core.enums.Partner
import lucuma.core.model.GuestRole
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.model.User

trait TopicElement:
  def users: List[User.Id]
  def partners: List[Partner]

  def canRead(u: User): Boolean =
    u.role match
      case ServiceRole(_) | StandardRole.Admin(_) | StandardRole.Staff(_) => true
      case StandardRole.Ngo(_, partner)                                    => partners.contains(partner)
      case GuestRole | StandardRole.Pi(_)                                  => users.contains(u.id)
