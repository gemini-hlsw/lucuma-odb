// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.implicits.*
import lucuma.common.middleware.CorsMiddleware
import lucuma.core.model.StandardRole
import lucuma.core.util.Gid
import lucuma.sso.client.*
import lucuma.sso.client.SsoMiddleware.traceUser
import lucuma.sso.service.database.Database
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.orcid.OrcidService
import natchez.Trace
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

object Routes {

  implicit def gidQueryParamDecoder[A: Gid]: QueryParamDecoder[A] =
    QueryParamDecoder[String].emap { s =>
      Gid[A].fromString.getOption(s).toRight(ParseFailure("<gid>", "Invalid GID"))
    }

  // This is the main event. Here are the routes we're serving.
  def apply[F[_]: Async: Logger: Trace](
    dbPool:    Resource[F, Database[F]],
    odb:       OdbClient[F],
    orcid:     OrcidService[F],
    jwtReader: SsoJwtReader[F],
    jwtWriter: SsoJwtWriter[F],
    cookies:   CookieService[F],
    publicUri: Uri,
    cookieDomain: String,
  ): HttpRoutes[F] = {
    object FDsl extends Http4sDsl[F]
    import FDsl._

    // The auth stage 2 URL is sent to ORCID, which redirects the user back. So we need to construct
    // a URL that makes sense to the user's browser!
    val Stage2Uri: Uri =
      publicUri.copy(path = Path.unsafeFromString("/auth/v1/stage2"))

    // Post-auth redirects come from the caller-supplied `state` value, thus it could be any domain.
    // Only allow relative URIs (same origin) or absolute URIs whose host is the cookie domain
    // or a subdomain of it
    // Otherwise the auth flow is an open redirect / phishing vector.
    def validRedirect(uri: Uri): Boolean =
      uri.host match
        case None    => true // relative, same-origin
        case Some(h) => CorsMiddleware.isAllowed(h, List(cookieDomain))

    // Some parameter matchers. The parameter names are NOT arbitrary! They are requied by ORCID.
    object OrcidCode   extends QueryParamDecoderMatcher[String]("code")
    object RedirectUri extends QueryParamDecoderMatcher[Uri]("state")
    object Key         extends QueryParamDecoderMatcher[ApiKey]("key")
    object Role        extends QueryParamDecoderMatcher[StandardRole.Id]("role")

    HttpRoutes.of[F] {

      // If the user has a refresh token, return a new JWT. Otherwise 403.
      case r@(POST -> Root / "api" / "v1" / "refresh-token") =>
        cookies.findSessionToken(r).flatMap {
          case None => Forbidden("Not logged in.")
          case Some(tok) =>
            dbPool.use { db =>
              db.findUserFromToken(tok).flatMap {
                case None    => Forbidden("Invalid session token.")
                case Some(u) => traceUser(u) *> jwtWriter.newJwt(u).flatMap(Ok(_))
              }
            }
        }

      // Authenticate as a guest. Response body is the new user's JWT, and a refresh token is set.
      case POST -> Root / "api" / "v1" / "auth-as-guest" =>
        dbPool.use { db =>
          db.createGuestUserAndSessionToken.flatMap { case (user, token) =>
            for {
              _   <- traceUser(user)
              jwt <- jwtWriter.newJwt(user)
              res <- Created(jwt)
              coo <- cookies.sessionCookie(token)
            } yield res.addCookie(coo)
          }
        }

      // Log out. TODO: If it's a guest user, delete that user.
      case POST -> Root / "api" / "v1" / "logout" =>
        // TODO: get the user so we can trace this!
        Ok("Logged out.").flatMap(cookies.removeCookie)

      // Start a new session wih the same user, but a different role. On success we replace the
      // session cookie and return a new JWT.
      case r@(GET -> Root / "auth" / "v1" / "set-role" :? Role(rid)) =>
        cookies.findSessionToken(r).flatMap {
          case None      => Forbidden("Not logged in.")
          case Some(tok) =>
            dbPool.use { db =>
              db.findStandardUserFromToken(tok).flatMap {
                case None    => Forbidden("Standard user required.")
                case Some(u) =>
                  if (u.role.id === rid) {
                    jwtWriter.newJwt(u).flatMap(Ok(_)) // nothing to do
                  } else {
                    u.otherRoles.find(_.id === rid) match
                      case None    => Forbidden("User does not own role.")
                      case Some(r) =>
                        for {
                          tok <- db.createStandardUserSessionToken(r.id)
                          coo <- cookies.sessionCookie(tok)
                          usr <- db.getStandardUserFromToken(tok)
                          jwt <- jwtWriter.newJwt(usr)
                          res <- Ok(jwt)
                        } yield res.addCookie(coo)
                  }
              }
            }
        }

      // Create an API key
      case r@(POST -> Root / "api" / "v1" / "create-api-key" :? Role(rid)) =>
        jwtReader.findStandardUser(r).flatMap {
          case None    => Forbidden("Standard user required.")
          case Some(u) =>
            traceUser(u) *> {
              if (u.role.id === rid || u.otherRoles.exists(_.id === rid))
                dbPool.use(_.createApiKey(u.role.id)).flatMap(Created(_))
              else
                Forbidden("Role is not owned by user.")
            }
        }

      // Exchange an API key for a JWT
      case r@(GET -> Root / "api" / "v1" / "exchange-api-key" :? Key(apiKey)) =>
        jwtReader.findServiceUser(r).flatMap {
          case None     => Forbidden("Service user required.")
          case Some(su) =>
            traceUser(su) *> dbPool.use(_.findStandardUserFromApiKey(apiKey)).flatMap {
              case None => Forbidden("Invalid API Key")
              case Some(u) =>
                traceUser(u, "lucuma.apikey") *>
                Logger[F].info(s"API key ${apiKey.id} for ${u.displayName} (${u.id}) exchanged by ${su.displayName}.") *>
                jwtWriter.newJwt(u, Some(3.hours)).flatMap(Ok(_))
            }
        }

      // Authentication Stage 1: Send the user to ORCID.
      case GET -> Root / "auth" / "v1" / "stage1" :? RedirectUri(redirectUrl) =>
        if (!validRedirect(redirectUrl))
          BadRequest("Invalid redirect URI.")
        else
          for {
            uri <- orcid.authenticationUri(Stage2Uri, Some(redirectUrl.toString))
            res <- Found(Location(uri))
          } yield res

      // Authentication Stage 2: The user has returned from ORCID.
      // Insert/update a standard user, set a refresh token, and redirect to wherever the user asked to go in Stage 1.
      case r@(GET -> Root / "auth" / "v1" / "stage2" :? OrcidCode(code) +& RedirectUri(redir)) if !validRedirect(redir) =>
        BadRequest("Invalid redirect URI.")

      case r@(GET -> Root / "auth" / "v1" / "stage2" :? OrcidCode(code) +& RedirectUri(redir)) =>
        dbPool.use { db =>
          for {
            access   <- orcid.getAccessToken(Stage2Uri, code) // when this fails we get a 400 back so we need to handle that case somehow
            person   <- orcid.getPerson(access)
            otoken   <- cookies.findSessionToken(r)
            oguest   <- otoken.flatTraverse(db.findGuestUserFromToken)
            _        <- oguest.traverse(traceUser(_))
            token    <-
              oguest match {
                case None =>
                  db.canonicalizeUser(access, person, RoleRequest.Pi)
                case Some(guestUser) =>
                  // We used to be a guest but we are now a standard user, so either promote the guest user or 
                  // chown the guest's programs, depending on whether the standard user is new or not.
                  db.promoteGuestUser(access, person, guestUser.id, RoleRequest.Pi).flatMap {
                    case (None, tok) => tok.pure[F]
                    case (Some(existing), tok) =>
                      db.findStandardUserFromToken(tok).flatMap:
                        case Some(standardUser) => odb.transferOwnership(guestUser, standardUser).as(tok)
                        case None => Async[F].raiseError(RuntimeException(s"Unpossible: promoteGuestUser returned a non-standard token for $existing"))
                  }
              }
            cookie   <- cookies.sessionCookie(token)
            res      <- Found(Location(redir))
          } yield res.addCookie(cookie)
        }

    }
  }

}
