// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import cats.data.*
import cats.effect.Concurrent
import cats.implicits.*
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.sso.client.util.JwtDecoder
import org.http4s.Credentials
import org.http4s.EntityDecoder
import org.http4s.InvalidMessageBodyFailure
import org.http4s.Request
import org.http4s.Response
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import pdi.jwt.exceptions.JwtException

trait SsoJwtReader[F[_]] { outer =>

  /**
   * Retrieve the JWT from the `Authorization: Bearer <jwt>` header, if present, raising an
   * error in `F` if otherwise.
   */
  def findClaim(req: Request[F]): F[Option[SsoJwtClaim]]

  /** Retrieve the JWT from the `Authorization: Bearer <jwt>` header, if present. */
  def attemptFindClaim(req: Request[F]): F[Option[Either[JwtException, SsoJwtClaim]]]

  /** Retrieve the User from the `Authorization: Bearer <jwt>` header, if present. */
  def findUser(req: Request[F]): F[Option[User]]

  /** Retrieve the StandardUser from the `Authorization: Bearer <jwt>` header, if present. */
  def findStandardUser(req: Request[F]): F[Option[StandardUser]]

  /** Retrieve the ServiceUser from the `Authorization: Bearer <jwt>` header, if present. */
  def findServiceUser(req: Request[F]): F[Option[ServiceUser]]

  /** Retrieve the JWT from the response body. */
  def findClaim(res: Response[F]): F[SsoJwtClaim]

  // import this!
  implicit def entityDecoder: EntityDecoder[F, SsoJwtClaim]


  def decodeClaim(jwt: String): F[SsoJwtClaim]

  def decodeUser(jwt: String): F[User]

  def decodeStandardUser(jwt: String): F[StandardUser]

}

object SsoJwtReader {

  private[client] val JwtCookie  = "lucuma-jwt"
  private[client] val lucumaUser = SsoJwtClaim.lucumaUser

  def apply[F[_]: Concurrent](jwtDecoder: JwtDecoder[F]): SsoJwtReader[F] =
    new SsoJwtReader[F] {

      implicit val entityDecoder: EntityDecoder[F, SsoJwtClaim] =
        EntityDecoder.text[F].flatMapR { token =>
          EitherT(jwtDecoder.attemptDecode(token))
            .map(SsoJwtClaim(_))
            .leftMap {
              case e: Exception => InvalidMessageBodyFailure(s"Invalid or missing JWT.", Some(e))
            }
        }

      val Bearer = CIString("Bearer")

      def decodeClaim(jwt: String): F[SsoJwtClaim] =
        jwtDecoder.decode(jwt).map(SsoJwtClaim(_))

      def decodeUser(jwt: String): F[User] =
        decodeClaim(jwt).map(_.getUser).flatMap(_.liftTo[F])

      def decodeStandardUser(jwt: String): F[StandardUser] =
        decodeUser(jwt).flatMap {
          case u: StandardUser => u.pure[F]
          case _ => Concurrent[F].raiseError(new RuntimeException("Not a standard user."))
        }

      def attemptFindClaim(req: Request[F]): F[Option[Either[JwtException, SsoJwtClaim]]] =
        findBearerAuthorization(req).flatMap {
          case None    => none.pure[F]
          case Some(c) => jwtDecoder.attemptDecode(c).map(_.map(SsoJwtClaim(_)).some)
        }

      def findBearerAuthorization(req: Request[F]): F[Option[String]]  =
        req.headers.get[Authorization].collect {
          case Authorization(Credentials.Token(Bearer, token)) => token
        } .pure[F]

      def findClaim(req: Request[F]): F[Option[SsoJwtClaim]] =
        OptionT(findBearerAuthorization(req))
          .flatMapF(token => jwtDecoder.decodeOption(token).map(_.map(SsoJwtClaim(_))))
          .value

      def findClaim(res: Response[F]): F[SsoJwtClaim] =
        res.as[SsoJwtClaim]

      def findUser(req: Request[F]): F[Option[User]] =
        findClaim(req).map(_.flatMap(_.getUser.toOption))

      def findServiceUser(req: Request[F]): F[Option[ServiceUser]] =
        findUser(req).map(_.collect { case u: ServiceUser => u })

      def findStandardUser(req: Request[F]): F[Option[StandardUser]] =
        findUser(req).map(_.collect { case u: StandardUser => u })


    }

}

