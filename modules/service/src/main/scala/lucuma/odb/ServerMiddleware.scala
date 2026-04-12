// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.*
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import lucuma.core.model.User
import lucuma.odb.otel.given
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import org.http4s.HttpRoutes
import org.http4s.Query
import org.http4s.Uri
import org.http4s.headers.Upgrade
import org.http4s.Uri.Scheme
import org.http4s.dsl.Http4sDsl
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware as OtelServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.ErrorAction
import org.http4s.server.middleware.Metrics
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.*

/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware {

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

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
  def cors[F[_]: Monad](corsOverHttps: Boolean, domain: List[String]): Middleware[F] =
    CORS.policy
      .withAllowCredentials(true)
      .withAllowOriginHost(u => (!corsOverHttps || (u.scheme === Scheme.https)) && domain.exists(u.host.value.endsWith))
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
  def traceUser[F[_]: Monad: Tracer](
    client: SsoClient[F, User]
  ): Middleware[F] = routes =>
    Kleisli { req =>
      val putFields: F[Unit] =
        client.find(req).flatMap {
          case None    =>
            Monad[F].unit
          case Some(u) =>
            Tracer[F].withCurrentSpanOrNoop:
              _.addAttributes(Attributes.from(u))
        }
      OptionT(putFields >> routes(req).value)
    }

  val redactor: UriRedactor = new UriRedactor:
    def redactPath(path: Uri.Path): Uri.Path = path
    def redactQuery(query: Query): Query =
      if (query.isEmpty) query
      else Query(redact.REDACTED -> None)
    def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment] =
      Some(fragment)

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
        .openTelemetry(redactor)
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
      // Metrics middleware wrapped skips WebSocket requests as they can be very long lived
      val httpMetrics: Middleware[F] = routes =>
        val withMetrics = Metrics[F](metricsOps)(routes)
        Kleisli(req => if (req.headers.get[Upgrade].isDefined) routes(req) else withMetrics(req))

      List[Middleware[F]](
        cors(corsOverHttps, domain),
        logging,
        httpMetrics,
        otel.asHttpRoutesMiddleware,
        traceUser(client),
        errorReporting,
        userCache,
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
    }
}
