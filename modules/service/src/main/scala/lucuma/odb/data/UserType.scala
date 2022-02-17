package lucuma.odb.data

import lucuma.core.util.Enumerated
import lucuma.core.model.User
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser


sealed abstract class UserType(val tag: String) extends Product with Serializable

object UserType {

  case object Guest    extends UserType("guest")
  case object Standard extends UserType("standard")
  case object Service  extends UserType("service")

  implicit val EnumeratedUserType: Enumerated[UserType] =
    new Enumerated[UserType] {
      def all: List[UserType] = List(Guest, Standard, Service)
      def tag(a: UserType): String = a.tag
    }

  def fromUser(u: User): UserType =
    u match {
      case GuestUser(_)             => Guest
      case ServiceUser(_, _)        => Service
      case StandardUser(_, _, _, _) => Standard
    }

}