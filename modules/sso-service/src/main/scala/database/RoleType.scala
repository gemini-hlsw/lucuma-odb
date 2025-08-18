// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.database

import lucuma.core.util.Enumerated

// lucuma_role_type
enum RoleType(private val tag: String) derives Enumerated:
  case Pi extends RoleType("Pi")
  case Ngo extends RoleType("Ngo")
  case Staff extends RoleType("Staff")
  case Admin extends RoleType("Admin")

object RoleType:
  def parse(s: String): Option[RoleType] =
    values.find(_.tag.equalsIgnoreCase(s))