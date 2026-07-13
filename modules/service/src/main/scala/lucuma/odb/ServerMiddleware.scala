// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.*
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import lucuma.common.middleware.CorsMiddleware
import lucuma.common.middleware.ErrorReportingMiddleware
import lucuma.common.middleware.LoggingMiddleware
import lucuma.common.middleware.MetricsMiddleware
import lucuma.common.middleware.TracingMiddleware
import lucuma.core.model.User
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware as OtelServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.collection.immutable.TreeMap

/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware {

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

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

  /** A middleware that composes all the others defined in this module. */
  def apply[F[_]: Async: Tracer: TracerProvider: MeterProvider: Logger](
    corsOverHttps: Boolean,
    domain: List[String],
    client: SsoClient[F, User],
    userService: UserService[F],
  ): Resource[F, Middleware[F]] =
    val spanDataProvider =
      ServerSpanDataProvider
        .openTelemetry(TracingMiddleware.redactor)
        .withRouteClassifier(routeClassifier)
        // These could be a bit expensive if there are lots of calls
        // OTOH a lot of traffic is via websockets
        .optIntoHttpRequestHeaders(HeaderRedactor.default)
        .optIntoHttpResponseHeaders(HeaderRedactor.default)

    (
      Resource.eval(userCache(client, userService)),
      Resource.eval(OtelServerMiddleware.builder[F](spanDataProvider).build),
      Resource.eval(OtelMetrics.serverMetricsOps[F]())
    ).mapN { (userCache, otel, metricsOps) =>
      List[Middleware[F]](
        CorsMiddleware.cors(corsOverHttps, domain),
        LoggingMiddleware.logging[F](),
        MetricsMiddleware.httpMetrics(metricsOps),
        otel.asHttpRoutesMiddleware,
        TracingMiddleware.traceUser(client),
        ErrorReportingMiddleware.errorReporting,
        userCache,
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
    }
}
