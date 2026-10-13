// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import cats.Monad
import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Clock
import cats.effect.Concurrent
import cats.effect.Ref
import cats.syntax.all.*
import lucuma.core.model.User
import org.http4s.*
import org.http4s.Credentials.Token
import org.http4s.client.Client
import org.http4s.client.UnexpectedStatus
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import pdi.jwt.exceptions.JwtException
import pdi.jwt.exceptions.JwtLengthException

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.*

/** An SSO client that extracts user information of type `A` from API keys and JWTs. */
trait SsoClient[F[_], A] {

  /**
   * Find authorization information in the specified request, if present and valid. Otherwise
   * return a message that tells why access is denied.
   */
  def find(req: Request[F]): F[Either[String, A]]

  /**
   * Get authorization information from the specified Authorization header, if valid. Otherwise
   * return a message that tells why access is denied.
   */
  def get(authorization: Authorization): F[Either[String, A]]

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

  /** The message for a denied request, when no more specific message is available. */
  val AccessDenied: String = "Access denied"

  /** The message for a denied API key, which the SSO server refused to exchange. */
  val InvalidApiKey: String = s"$AccessDenied: invalid API key"

  /**
   * The message of `t`, safe to send to the client and to write to the log. The message of a
   * `JwtLengthException` contains the whole token, so we replace it.
   */
  private def safeMessage(t: Throwable): String =
    t match {
      case _: JwtLengthException => "Expected token to be composed of 2 or 3 parts separated by dots."
      case _                     => Option(t.getMessage).getOrElse(t.getClass.getName)
    }

  /** The message for a denied request: the `JwtException` message with a prefix, else `AccessDenied`. */
  def denialMessage(t: Throwable): String =
    t match {
      case e: JwtException => s"$AccessDenied: ${safeMessage(e)}"
      case _               => AccessDenied
    }

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

      def getOrFetchApiInfo(apiKey: ApiKey): F[Either[String, UserInfo]] =
        OptionT(getCachedApiInfo(apiKey))
          .getOrElseF(fetchApiInfo(apiKey))
          .map(_.asRight[String])
          .recoverWith {
            case UnexpectedStatus(Status.Forbidden, _, _) =>
              Logger[F].warn(s"SSO refused to exchange API key ${apiKey.id}").as(InvalidApiKey.asLeft[UserInfo])
            // Any other status is a server error. Remove the request URI, which contains the key.
            case UnexpectedStatus(status, method, uri) =>
              UnexpectedStatus(status, method, uri.removeQueryParam("key")).raiseError[F, Either[String, UserInfo]]
          }

      def getApiKey(bearerAuthorization: String): Option[ApiKey] =
        ApiKey.fromString.getOption(bearerAuthorization)

      def getJwtInfo(bearerAuthorization: String): F[Either[String, UserInfo]] = {
        for {
          claim <- jwtReader.decodeClaim(bearerAuthorization)
          user  <- claim.getUser.liftTo[F]
        } yield UserInfo(user, claim, authorization(bearerAuthorization))
      } .attempt.flatMap(_.leftTraverse { ex =>
        // Log neither the token nor the exception: the message of some exceptions contains the token.
        Logger[F].warn(s"JWT validation failed: ${safeMessage(ex)}").as(denialMessage(ex))
      })

      def getUserInfo(bearerAuthorization: String): F[Either[String, UserInfo]] =
        getApiKey(bearerAuthorization).fold(getJwtInfo(bearerAuthorization))(getOrFetchApiInfo)

      new AbstractSsoClient[F, UserInfo] {

        def get(authorization: Authorization): F[Either[String, UserInfo]] =
          authorization.credentials match {
            case Token(Bearer, ba) => getUserInfo(ba)
            case _                 => AccessDenied.asLeft[UserInfo].pure[F]
          }

      }

    }

  abstract class AbstractSsoClient[F[_]: Monad, A] extends SsoClient[F, A] { outer =>

    object FDsl extends Http4sDsl[F]
    import FDsl._

    /** Delegates to `get` if the request has an Authorization header, else denies access. */
    def find(req: Request[F]): F[Either[String, A]] =
      req.headers.get[Authorization].fold(AccessDenied.asLeft[A].pure[F])(get)

    final def require(req: Request[F])(f: A => F[Response[F]]): F[Response[F]] =
      EitherT(find(req)).foldF(Forbidden(_), f)

    private def transform[B](f: Either[String, A] => Either[String, B]): SsoClient[F, B] =
      new AbstractSsoClient[F, B] {
        override def find(req: Request[F]) = outer.find(req).map(f)
        def get(authorization: Authorization) = outer.get(authorization).map(f)
      }

    final def map[B](f: A => B): SsoClient[F, B] = transform(_ map f)
    final def filter(f: A => Boolean): SsoClient[F, A] = transform(_.filterOrElse(f, AccessDenied))
    final def collect[B](f: PartialFunction[A, B]): SsoClient[F, B] = transform(_.flatMap(a => f.lift(a).toRight(AccessDenied)))

  }

}
