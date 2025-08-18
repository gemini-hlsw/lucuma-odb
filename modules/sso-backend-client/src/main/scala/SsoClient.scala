// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import cats.Monad
import cats.data.OptionT
import cats.effect.Clock
import cats.effect.Concurrent
import cats.effect.Ref
import cats.syntax.all.*
import lucuma.core.model.User
import org.http4s.*
import org.http4s.Credentials.Token
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.*

/** An SSO client that extracts user information of type `A` from API keys and JWTs. */
trait SsoClient[F[_], A] {

  /** Find authorization information in the specified request, if present and valid. */
  def find(req: Request[F]): F[Option[A]]

  /** Get authorization information from the specified Authorization header, if valid. */
  def get(authorization: Authorization): F[Option[A]]

  /**
   * Find authorization information and pass it to the specified response handler, responding with
   * `403 Forbidden` if the information is not present. This is a common use case for `find` and is
   * provided as a convenience.
   */
  def require(req: Request[F])(f: A => F[Response[F]]): F[Response[F]]

  /** Transform the result of `find` using the specified function. */
  def map[B](f: A => B): SsoClient[F, B]

  /** Filter the result of `find` using the specified function. */
  def filter(f: A => Boolean): SsoClient[F, A]

  /**
   * Filter and transform the result of `find` using the specified partial function.
   * {{{
   *
   * // Standard initial client.
   * val client: SsoClient[F, UserInfo] = ...
   *
   * // Client that only sees `StandardUser`.
   * val std: SsoClient[F, StandardUser] =
   *   client.collect {
   *     case UserInfo(s: StandardUser, _, _) => s
   *   }
   *
   * ...
   *
   * // Match but respond with 403 Forbidden unless there's a `StandardUser`
   * case GET -> Root / "something" / "restricted" =>
   *   std.require(req) { su =>
   *     val role = su.role
   *     ...
   *   }
   * }}}
   */
  def collect[B](f: PartialFunction[A, B]): SsoClient[F, B]

}


object SsoClient {

  /**
   * Complete set of user information, as retrieved from an HTTP request.
   * @param user   the user, extracted from JWT
   * @param claim  the entire JWT claim
   * @param header HTTP `Authorization: Bearer <jwt>` header containing the encoded JWT, which can
   *   be included in further requests made on behalf of the user.
   */
  case class UserInfo(user: User, claim: SsoJwtClaim, header: Authorization) {
    assert(claim.getUser == Right(user)) // sanity check
  }

  /**
   * Construct an initial SSO client that extracts `UserInfo` from `Authorization: Bearer` headers,
   * exchanging and caching API keys as required. Transformations of this client share the same
   * underlyiny API key cache. Note that the cache is never expunged; we rely on server cycling.
   */
  def initial[F[_]: Concurrent: Clock: Logger](
    httpClient:  Client[F],
    ssoRoot:     Uri,
    jwtReader:   SsoJwtReader[F],
    serviceJwt:  String,
    gracePeriod: FiniteDuration = 5.minutes,
  ): F[SsoClient[F, UserInfo]] =
    Ref[F].of(TreeMap.empty[ApiKey, UserInfo]).map { ref =>

      val Bearer = CIString("Bearer")

      def authorization(jwt: String): Authorization =
        Authorization(Credentials.Token(Bearer, jwt))

      def getCachedApiInfo(apiKey: ApiKey): F[Option[UserInfo]] =
        Clock[F].realTimeInstant.map(_.plusSeconds(gracePeriod.toSeconds)).flatMap { t =>
          ref.get.map(_.get(apiKey).filter(_.claim.expiration.isAfter(t)))
        }

      def exchangeRequest(apiKey: ApiKey): Request[F] =
        Request(
          method  = Method.GET,
          uri     = (ssoRoot / "api" / "v1" / "exchange-api-key").withQueryParam("key", apiKey),
          headers = Headers(authorization(serviceJwt))
        )

      def fetchApiInfo(apiKey: ApiKey): F[UserInfo] =
        for {
          jwt   <- httpClient.expect[String](exchangeRequest(apiKey))(using EntityDecoder.text[F])
          claim <- jwtReader.decodeClaim(jwt)
          user  <- claim.getUser.liftTo[F] // this really is a server error
          info   = UserInfo(user, claim, authorization(jwt))
          _     <- ref.update(m => m + (apiKey -> info))
        } yield info

      def getOrFetchApiInfo(apiKey: ApiKey): F[UserInfo] =
        OptionT(getCachedApiInfo(apiKey)).getOrElseF(fetchApiInfo(apiKey))

      def getApiKey(bearerAuthorization: String): Option[ApiKey] =
        ApiKey.fromString.getOption(bearerAuthorization)

      def getApiInfo(bearerAuthorization: String): F[Option[UserInfo]] =
        getApiKey(bearerAuthorization).traverse(getOrFetchApiInfo)

      def getJwtInfo(bearerAuthorization: String): F[Option[UserInfo]] = {
        for {
          claim <- jwtReader.decodeClaim(bearerAuthorization)
          user  <- claim.getUser.liftTo[F]
          info   = UserInfo(user, claim, authorization(bearerAuthorization))
        } yield info
      } .attempt.flatMap {
        case Right(ui) => Some(ui).pure[F]
        case Left(ex)  => Logger[F].warn(ex)(s"JWT validation failed for $bearerAuthorization").as(None)
      }

      def getUserInfo(bearerAuthorization: String): F[Option[UserInfo]] =
        (OptionT(getApiInfo(bearerAuthorization)) <+> OptionT(getJwtInfo(bearerAuthorization))).value

      new AbstractSsoClient[F, UserInfo] {

        def get(authorization: Authorization): F[Option[UserInfo]] =
          authorization.credentials match {
            case Token(Bearer, ba) => getUserInfo(ba)
            case _                 => none.pure[F]
          }

        def find(req: Request[F]): F[Option[UserInfo]] =
          req.headers.get[Authorization] match {
            case Some(a) => get(a)
            case None    => none.pure[F]
          }

      }

    }

  abstract class AbstractSsoClient[F[_]: Monad, A] extends SsoClient[F, A] { outer =>

    object FDsl extends Http4sDsl[F]
    import FDsl._

    final def require(req: Request[F])(f: A => F[Response[F]]): F[Response[F]] =
      OptionT(find(req)).cataF(Forbidden(), f)

    private def transform[B](f: Option[A] => Option[B]): SsoClient[F, B] =
      new AbstractSsoClient[F, B] {
        def find(req: Request[F]) = outer.find(req).map(f)
        def get(authorization: Authorization) = outer.get(authorization).map(f)
      }

    final def map[B](f: A => B): SsoClient[F, B] = transform(_ map f)
    final def filter(f: A => Boolean): SsoClient[F, A] = transform(_ filter f)
    final def collect[B](f: PartialFunction[A, B]): SsoClient[F, B] = transform(_ collect f)

  }

}
