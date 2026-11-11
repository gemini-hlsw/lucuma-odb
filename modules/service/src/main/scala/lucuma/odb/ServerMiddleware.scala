// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.*
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.*
import cats.effect.std.MapRef
import cats.implicits.*
import lucuma.common.middleware.CorsMiddleware
import lucuma.common.middleware.ErrorReportingMiddleware
import lucuma.common.middleware.LoggingMiddleware
import lucuma.common.middleware.TracingMiddleware
import lucuma.core.model.User
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.otel.HttpInstrumentation
import lucuma.sso.client.SsoClient
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware {

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * A middleware that updates the user table when it sees a user it hasn't seen before, or when
   * a known user has changed.
   */
  def userCache[F[_]: Concurrent](
    client: SsoClient[F, User],
    userService: UserService[F],
  ): F[Middleware[F]] =
    MapRef[F, User.Id, User].map { cache => routes =>
      Kleisli { req =>
        val updateUser: F[Unit] =
          client.find(req).flatMap(_.traverse_ { user =>
            // Update the user if not present in the cache, or if it has changed since the last time we saw it.
            // Cache only after the write succeeds, so a failed write is retried on the next request.
            // The cache will grow in an unbounded fashion, which is fine. Not all that many users.
            cache(user.id).get.flatMap { cached =>
              (Services.asSuperUser(userService.canonicalizeUser(user)) *> cache(user.id).set(user.some))
                .whenA(cached.forall(_ =!= user))
            }
          })
        OptionT.liftF(updateUser) *> routes(req)
      }
    }

  // Maps dynamic route paths to stable routes
  private def routeClassifier[F[_]]: RouteClassifier = {
    val dsl = Http4sDsl[F]
    import dsl.*
    RouteClassifier.of[F] {
      // AttachmentRoutes.scala
      case GET    -> Root / "attachment" / "url" / _ => "/attachment/url/{id}"
      case GET    -> Root / "attachment" / _         => "/attachment/{id}"
      case POST   -> Root / "attachment"             => "/attachment"
      case PUT    -> Root / "attachment" / _         => "/attachment/{id}"
      case DELETE -> Root / "attachment" / _         => "/attachment/{id}"
      // GraphQLRoutes.scala
      case GET    -> Root / "export" / _             => "/export/{name}"
      case GET    -> Root / "odb"                    => "/odb"
      case POST   -> Root / "odb"                    => "/odb"
      // EmailWebhookRoutes.scala
      case POST   -> Root / "mailgun"                => "/mailgun"
      // SchedulerRoutes.scala
      case POST   -> Root / "scheduler" / "atoms"    => "/scheduler/atoms"
    }
  }

  /**
   * HTTP metrics and spans. Apply to the final app, after `orNotFound`. Recording headers can be
   * expensive with many calls, but most traffic is WebSocket.
   */
  def instrumentation[F[_]: Async: TracerProvider: MeterProvider: ContextManager]: F[Endo[HttpApp[F]]] =
    HttpInstrumentation.server[F](routeClassifier[F], recordHeaders = true)

  /** A middleware that composes all the others defined in this module, except instrumentation. */
  def apply[F[_]: Async: Tracer: Logger](
    corsOverHttps: Boolean,
    domain: List[String],
    client: SsoClient[F, User],
    userService: UserService[F],
  ): Resource[F, Middleware[F]] =
    Resource.eval(userCache(client, userService)).map { userCache =>
      List[Middleware[F]](
        CorsMiddleware.cors(corsOverHttps, domain),
        LoggingMiddleware.logging[F](),
        TracingMiddleware.traceUser(client),
        ErrorReportingMiddleware.errorReporting,
        userCache,
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
    }
}
