package lucuma.odb.data

import lucuma.core.util.Enumerated


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

}