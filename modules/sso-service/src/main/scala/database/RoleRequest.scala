// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.database

import lucuma.core.enums.Partner

sealed abstract class RoleRequest(
  val tpe: RoleType,
  val partnerOption: Option[Partner] = None
)
object RoleRequest {
  case object Pi extends RoleRequest(RoleType.Pi)
  case class  Ngo(partner: Partner) extends RoleRequest(RoleType.Ngo, Some(partner))
  case object Staff extends RoleRequest(RoleType.Staff)
  case object Admin extends RoleRequest(RoleType.Admin)
}
