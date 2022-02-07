package lucuma.odb.graphql

import lucuma.core.model.GuestUser
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.Gid

object TestUsers {

  private def checkDigit(baseDigits: String): String = {
    require(baseDigits.forall(c => c >= '0' && c <= '9'))
    val total = baseDigits.foldLeft(0) { (acc, c) =>
      val digit = c - '0'
      (acc + digit) * 2
    }
    val remainder = total % 11
    val result    = (12 - remainder) % 11
    if (result == 10) "X" else result.toString
  }

  private def orcidId(id: Long): OrcidId = {
    val ds = f"$id%015d"
    val (a, b, c, d) = (ds.substring(0, 4), ds.substring(4, 8), ds.substring(8, 12), ds.subSequence(12, 15))
    val x = checkDigit(a + b + c + d)
    OrcidId.fromValue(s"$a-$b-$c-$d$x") match {
      case Left(s)  => sys.error(s)
      case Right(o) => o
    }
  }


  def guest(id: Long) = GuestUser(Gid[User.Id].fromLong.getOption(id).get)
  def service(id: Long) = ServiceUser(Gid[User.Id].fromLong.getOption(id).get, s"service-$id")

  object Standard {

    def pi(id: Long, roleId: Long) = StandardUser(
      id         = Gid[User.Id].fromLong.getOption(id).get,
      role       = StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(roleId).get),
      otherRoles = Nil,
      profile    = OrcidProfile(
        orcidId      = orcidId(id),
        givenName    = None,
        familyName   = None,
        creditName   = None,
        primaryEmail = None,
      )
    )

  }

}
