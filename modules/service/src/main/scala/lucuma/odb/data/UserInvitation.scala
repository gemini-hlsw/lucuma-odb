// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosLong
import monocle.Prism

import scala.util.matching.Regex
import lucuma.core.util.Enumerated
import io.circe.Encoder
import io.circe.Decoder

/**
 * An invitation consists of an id (a positive Long) and a cleartext body (a 96-char lowercase hex 
 * string). Invitations have a canonical string representation `id.body` where `id` is in lowercase
 * hex, for example:
 * {{{
 * 10d.3b9e2adc5bffdac72f487a2760061bcfebc44037b69f2085c9a1ba10aa5d2d338421fc0d79f45cfd07666617ac4e2c89
 * }}}
 */
final case class UserInvitation(id: UserInvitation.Id, body: String)
object UserInvitation:

  enum Status(val tag: String):
    case Pending  extends Status("pending")
    case Redeemed extends Status("redeemed")
    case Declined extends Status("declined")
    case Revoked  extends Status("revoked")

  given Enumerated[Status] = Enumerated.derived

  type Id = PosLong
  object Id {

    /** Id from hex string. */
    val fromString: Prism[String, Id] =
      Prism[String, Id] { sid =>
        Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(sid, 16)).flatMap { n =>
          PosLong.from(n)
        } .toOption
      } { id =>
        id.value.toHexString
      }

  }

  private val R: Regex =
    raw"^([0-9a-f]{3,})\.([0-9a-f]{96})$$".r

  val fromString: Prism[String, UserInvitation] =
    Prism[String, UserInvitation] {
      case R(sid, body) => Id.fromString.getOption(sid).map(UserInvitation(_, body))
      case _            => None
     } { k =>
      s"${k.id.value.toHexString}.${k.body}"
    }

  given OrderUserInvitation: Order[UserInvitation] =
    Order.by(k => (k.id, k.body))

  given OrderingUserInvitation: Ordering[UserInvitation] =
    OrderUserInvitation.toOrdering

  given Encoder[UserInvitation] =
    Encoder.encodeString.contramap(fromString.reverseGet)

  given Decoder[UserInvitation] =
    Decoder.decodeString.emap(s => fromString.getOption(s).toRight(s"Invalid user invitation: $s"))