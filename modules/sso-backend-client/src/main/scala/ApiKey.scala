// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import cats.Order
import cats.effect.Concurrent
import cats.implicits.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosLong
import monocle.Prism
import org.http4s.DecodeResult
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.MalformedMessageBodyFailure
import org.http4s.ParseFailure
import org.http4s.QueryParamDecoder
import org.http4s.QueryParamEncoder

import scala.util.matching.Regex

/**
 * An API key consists of an id (a positive Long) and a cleartext body (a 96-char lowecase hex
 * string). API keys have a canonical string representation `id.body` where `id` is in lowercase
 * hex, for example:
 * {{{
 * 10d.3b9e2adc5bffdac72f487a2760061bcfebc44037b69f2085c9a1ba10aa5d2d338421fc0d79f45cfd07666617ac4e2c89
 * }}}
 */
final case class ApiKey(id: ApiKey.Id, body: String)
object ApiKey {

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

  val fromString: Prism[String, ApiKey] =
    Prism[String, ApiKey] {
      case R(sid, body) => Id.fromString.getOption(sid).map(ApiKey(_, body))
      case _            => None
     } { k =>
      s"${k.id.value.toHexString}.${k.body}"
    }

  implicit val OrderApiKey: Order[ApiKey] =
    Order.by(k => (k.id, k.body))

  implicit val OrderingApiKey: Ordering[ApiKey] =
    OrderApiKey.toOrdering

  implicit def entityEncoder[F[_]]: EntityEncoder[F, ApiKey] =
    EntityEncoder[F, String].contramap(fromString.reverseGet)

  implicit def entityDecoder[F[_]: Concurrent]: EntityDecoder[F, ApiKey] =
    EntityDecoder.text[F].map(fromString.getOption).flatMapR {
      case Some(ak) => DecodeResult.success(ak.pure[F])
      case None     => DecodeResult.failure(Concurrent[F].pure(MalformedMessageBodyFailure("Invalid API Key")))
    }

  implicit val queryParamDecoder: QueryParamDecoder[ApiKey] =
    QueryParamDecoder[String]
      .map(fromString.getOption)
      .emap(_.toRight(ParseFailure("<redacted>", "Invalid API Key")))

  implicit val queryParamEncoder: QueryParamEncoder[ApiKey] =
    QueryParamEncoder[String].contramap(fromString.reverseGet)

}