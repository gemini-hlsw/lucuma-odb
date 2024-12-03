// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import lucuma.core.enums.Partner
import lucuma.core.model.GuestUser
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.core.util.Gid

object TestUsers:

  private def checkDigit(baseDigits: String): String =
    require(baseDigits.forall(c => c >= '0' && c <= '9'))
    val total = baseDigits.foldLeft(0): (acc, c) =>
      val digit = c - '0'
      (acc + digit) * 2
    val remainder = total % 11
    val result    = (12 - remainder) % 11
    if (result == 10) "X" else result.toString

  def orcidId(id: Long): OrcidId =
    val ds = f"$id%015d"
    val (a, b, c, d) = (ds.substring(0, 4), ds.substring(4, 8), ds.substring(8, 12), ds.subSequence(12, 15))
    val x = checkDigit(a + b + c + d)
    OrcidId.fromValue(s"$a-$b-$c-$d$x") match
      case Left(s)  => sys.error(s)
      case Right(o) => o

  def guest(id: Long) = GuestUser(Gid[User.Id].fromLong.getOption(id).get)
  def service(id: Long) = ServiceUser(Gid[User.Id].fromLong.getOption(id).get, s"service-$id")

  object Standard:

    def apply(
      id:         Long,
      role:       StandardRole,
      givenName:  Option[String] = None,
      familyName: Option[String] = None,
      creditName: Option[String] = None,
      email:      Option[String] = None
    ): StandardUser =
      StandardUser(
        id         = Gid[User.Id].fromLong.getOption(id).get,
        role       = role,
        otherRoles = Nil,
        profile    = OrcidProfile(
          orcidId  = orcidId(id),
          profile  = UserProfile(
            givenName  = givenName,
            familyName = familyName,
            creditName = creditName,
            email      = email
          )
        )
      )

    def pi(id: Long, roleId: Long) =
      Standard(id, StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(roleId).get))

    def ngo(id: Long, roleId: Long, partner: Partner) =
      Standard(id, StandardRole.Ngo(Gid[StandardRole.Id].fromLong.getOption(roleId).get, partner))

    def staff(id: Long, roleId: Long) =
      Standard(id, StandardRole.Staff(Gid[StandardRole.Id].fromLong.getOption(roleId).get))

    def admin(id: Long, roleId: Long) =
      Standard(id, StandardRole.Admin(Gid[StandardRole.Id].fromLong.getOption(roleId).get))