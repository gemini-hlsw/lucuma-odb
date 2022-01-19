// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import natchez.Trace
import cats._
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._
import org.http4s.server.middleware.ErrorAction
import org.typelevel.log4cats.Logger
import org.http4s.server.middleware.CORS
import natchez.http4s.NatchezMiddleware
import scala.concurrent.duration._
import lucuma.sso.client.SsoClient
import lucuma.core.model.User
import cats.data.Kleisli
import cats.data.OptionT
import scala.collection.immutable.TreeMap
import lucuma.odb.service.UserService


/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware {

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** A middleware that adds distributed tracing. */
  def natchez[F[_]: MonadCancel[*[_], Throwable]: Trace]: Middleware[F] =
    NatchezMiddleware.server[F]

  /** A middleware that logs request and response. Headers are redacted in staging/production. */
  def logging[F[_]: Async]: Middleware[F] =
    org.http4s.server.middleware.Logger.httpRoutes[F](
      logHeaders        = true,
      logBody           = false,
    )

  /** A middleware that reports errors during requets processing. */
  def errorReporting[F[_]: MonadError[*[_], Throwable]: Logger]: Middleware[F] = routes =>
    ErrorAction.httpRoutes.log(
      httpRoutes              = routes,
      messageFailureLogAction = Logger[F].error(_)(_),
      serviceErrorLogAction   = Logger[F].error(_)(_)
    )

  /** A middleware that adds CORS headers. The origin must match the cookie domain. */
  def cors[F[_]: Monad](domain: String): Middleware[F] =
    CORS.policy
      .withAllowCredentials(true)
      .withAllowOriginHost(_.host.value.endsWith(domain))
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
              val update = userService.canonicalizeUser(user).whenA(cache.get(user.id).forall(_ === user))
              (cache + (user.id -> user), OptionT.liftF(update) *> routes(req))
            }).flatten
        }
      }
    }

  /** A middleware that composes all the others defined in this module. */
  def apply[F[_]: Async: Trace: Logger](
    config: Config,
    client: SsoClient[F, User],
    userService: UserService[F],
  ): F[Middleware[F]] =
    userCache(client, userService).map { userCache =>
      List[Middleware[F]](
        cors(config.domain),
        logging,
        natchez,
        errorReporting,
        userCache,
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
    }
}