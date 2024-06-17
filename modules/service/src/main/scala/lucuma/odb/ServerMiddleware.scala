// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.*
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import lucuma.core.model.User
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.HttpRoutes
import org.http4s.Uri.Scheme
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.ErrorAction
import org.typelevel.log4cats.Logger

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.*


/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware {

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** A middleware that adds distributed tracing. */
  def natchez[F[_]: MonadCancelThrow: Trace]: Middleware[F] =
    NatchezMiddleware.server[F]

  /** A middleware that logs request and response. Headers are redacted in staging/production. */
  def logging[F[_]: Async]: Middleware[F] =
    org.http4s.server.middleware.Logger.httpRoutes[F](
      logHeaders        = true,
      logBody           = false,
    )

  /** A middleware that reports errors during requets processing. */
  def errorReporting[F[_]: MonadThrow: Logger]: Middleware[F] = routes =>
    ErrorAction.httpRoutes.log(
      httpRoutes              = routes,
      messageFailureLogAction = Logger[F].error(_)(_),
      serviceErrorLogAction   = Logger[F].error(_)(_)
    )

  /** A middleware that adds CORS headers. The origin must match the cookie domain. */
  def cors[F[_]: Monad](domain: List[String]): Middleware[F] =
    CORS.policy
      .withAllowCredentials(true)
      .withAllowOriginHost(u => domain.exists(u.host.value.endsWith))
      .withMaxAge(1.day)
      .apply

  /**
   * A middleware that updates the user table when it sees a user it hasn't seen before, or when
   * a known user has changed.
   */
  def userCache[F[_]: Monad: Ref.Make](
    client: SsoClient[F, User],
    userService: UserService[F],
  ): F[Middleware[F]] =
    Ref[F].of(TreeMap.empty[User.Id, User]).map { ref => routes =>
      Kleisli { req =>
        OptionT.liftF(client.find(req)).flatMap {
          case None       => routes(req) // no user, nothing to do
          case Some(user) =>
            OptionT.liftF(ref.modify { cache =>
              // Update the user if not present in the cache, or if it has changed since the last time we saw it.
              // The cache will grow in an unbounded fashion, which is fine. Not all that many users.
              val update = Services.asSuperUser(userService.canonicalizeUser(user).whenA(cache.get(user.id).forall(_ =!= user)))
              (cache + (user.id -> user), OptionT.liftF(update) *> routes(req))
            }).flatten
        }
      }
    }

  /**
   * Add the user to the trace, if known.
   */
  def traceUser[F[_]: Monad: Trace](
    client: SsoClient[F, User]
  ): Middleware[F] = routes =>
    Kleisli { req =>
      val putFields: F[Unit] =
        client.find(req).flatMap {
          case None    => Monad[F].unit
          case Some(u) =>
            Trace[F].put(
              "user.id"          -> u.id.toString,
              "user.access"      -> u.role.access.tag,
              "user.displayName" -> u.displayName,
            )
        }
      OptionT.liftF(putFields) >> routes(req)
    }

  /** A middleware that composes all the others defined in this module. */
  def apply[F[_]: Async: Trace: Logger](
    domain: List[String],
    client: SsoClient[F, User],
    userService: UserService[F],
  ): F[Middleware[F]] =
    userCache(client, userService).map { userCache =>
      List[Middleware[F]](
        cors(domain),
        logging,
        natchez,
        traceUser(client),
        errorReporting,
        userCache,
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
    }
}
