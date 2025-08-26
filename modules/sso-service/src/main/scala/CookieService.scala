// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.Applicative
import cats.MonadThrow
import cats.data.OptionT
import cats.syntax.all.*
import org.http4s.HttpDate
import org.http4s.Request
import org.http4s.RequestCookie
import org.http4s.Response
import org.http4s.ResponseCookie
import org.http4s.SameSite

import java.util.UUID

trait CookieReader[F[_]] {

  /** Find the session cookie in `req`, if any. */
  def findCookie(req: Request[F]): F[Option[RequestCookie]]

  /**
   * Find the session cookie in `req`, if any, and decode it as a SessionToken. If the cookie's content is
   * not a valid SessionToken an error will be raised in `F`.
   */
  def findSessionToken(req: Request[F]): F[Option[SessionToken]]

  /** Find the session cookie in `req`, if any. */
  def findCookie(res: Response[F]): F[Option[ResponseCookie]]

  def getCookie(res: Response[F]): F[ResponseCookie]

  /**
   * Find the session cookie in `res`, if any, and decode it as a SessionToken. If the cookie's content is
   * not a valid SessionToken an error will be raised in `F`.
   */
  def findSessionToken(res: Response[F]): F[Option[SessionToken]]

  def getSessionToken(res: Response[F]): F[SessionToken]

}

object CookieReader {

  private[service] val CookieName = "lucuma-refresh-token"

  def apply[F[_]: MonadThrow]: CookieReader[F] =
    new CookieReader[F] {

      def findCookie(req: Request[F]): F[Option[RequestCookie]] =
        req.cookies.find(_.name == CookieName).pure[F]

      def getCookie(res: Response[F]): F[ResponseCookie] =
        findCookie(res).flatMap(_.toRight(new RuntimeException(s"Missing cookie.")).liftTo[F])

      def findSessionToken(req: Request[F]): F[Option[SessionToken]] =
        OptionT(findCookie(req)).semiflatMap { c =>
          Either.catchOnly[IllegalArgumentException](SessionToken(UUID.fromString(c.content))).liftTo[F]
        } .value

      def findCookie(res: Response[F]): F[Option[ResponseCookie]] =
        res.cookies.find(_.name == CookieName).pure[F]

      def findSessionToken(res: Response[F]): F[Option[SessionToken]] =
        OptionT(findCookie(res)).semiflatMap { c =>
          Either.catchOnly[IllegalArgumentException](SessionToken(UUID.fromString(c.content))).liftTo[F]
        } .value

      def getSessionToken(res: Response[F]): F[SessionToken] =
        findSessionToken(res).flatMap(_.toRight(new RuntimeException(s"Missing or invalid session token.")).liftTo[F])

    }
}

trait CookieWriter[F[_]] {

  /** Construct a non-expiring session cookie containing the specified token. */
  def sessionCookie(token: SessionToken): F[ResponseCookie]

  /** Construct an empty and expired session cookie. */
  def removeCookie(res: Response[F]): F[Response[F]]

}

object CookieWriter {

  private[service] val CookieName = CookieReader.CookieName

  def apply[F[_]: Applicative](
    domain: String,
    secure: Boolean,
  ): CookieWriter[F] =
    new CookieWriter[F] {

      val emptyCookie: ResponseCookie =
        ResponseCookie(
          name     = CookieName,
          domain   = Some(domain),
          content  = "",
          sameSite = Some(SameSite.Strict),
          secure   = secure,
          httpOnly = secure,
          path     = Some("/"),
        )

      def sessionCookie(token: SessionToken): F[ResponseCookie] =
        emptyCookie.copy(
          content = token.value.toString(),
          expires = Some(HttpDate.MaxValue),
        ).pure[F]

      def removeCookie(res: Response[F]): F[Response[F]] =
        res.removeCookie(emptyCookie).pure[F]

    }

}

trait CookieService[F[_]]
  extends CookieReader[F]
     with CookieWriter[F]

object CookieService {
    def apply[F[_]: MonadThrow](
      domain: String,
      secure: Boolean,
    ): CookieService[F] =
      new CookieService[F] {
        val reader = CookieReader[F]
        val writer = CookieWriter[F](domain, secure)
        def findCookie(req: Request[F]): F[Option[RequestCookie]] = reader.findCookie(req)
        def getCookie(res: Response[F]): F[ResponseCookie] = reader.getCookie(res)
        def findSessionToken(req: Request[F]): F[Option[SessionToken]] = reader.findSessionToken(req)
        def findCookie(res: Response[F]): F[Option[ResponseCookie]] = reader.findCookie(res)
        def findSessionToken(res: Response[F]): F[Option[SessionToken]] = reader.findSessionToken(res)
        def sessionCookie(token: SessionToken): F[ResponseCookie] = writer.sessionCookie(token)
        def getSessionToken(res: Response[F]): F[SessionToken] = reader.getSessionToken(res)
        def removeCookie(res: Response[F]): F[Response[F]] = writer.removeCookie(res)
      }
}
