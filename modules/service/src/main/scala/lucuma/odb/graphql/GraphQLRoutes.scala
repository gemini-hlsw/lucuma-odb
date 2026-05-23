// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Parallel
import cats.data.OptionT
import cats.effect.*
import cats.effect.std.SecureRandom
import cats.implicits.*
import cats.kernel.Order
import fs2.Stream
import grackle.Operation
import grackle.Result
import grackle.skunk.SkunkMonitor
import io.circe.Json
import lucuma.catalog.clients.GaiaClient
import lucuma.core.model.User
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes as LucumaGraphQLRoutes
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.otel.given
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.odb.util.Cache
import lucuma.sso.client.SsoClient
import org.http4s.Header
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.http4s.headers.`Content-Type`
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer
import skunk.Session
import skunk.SqlState

import scala.concurrent.duration.*

object GraphQLRoutes {

  given Order[Authorization] =
    Order.by(Header[Authorization].value)

  /**
   * Construct a source of `HttpRoutes` tailored to the requesting user. Routes will be cached
   * based on the `Authorization` header and discarded when `ttl` expires.
   */
  def apply[F[_]: {Async, Parallel, Tracer as T, Logger as L, LoggerFactory, SecureRandom}](
    gaiaClient:           GaiaClient[F],
    itcClient:            ItcClient[F],
    commitHash:           CommitHash,
    goaUsers:             Set[User.Id],
    ssoClient:            SsoClient[F, User],
    pool:                 Resource[F, Session[F]],
    monitor:              SkunkMonitor[F],
    ttl:                  FiniteDuration,
    userSvc:              UserService[F],
    enums:                Enums,
    ptc:                  TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:           Client[F],
    horizonsClient:       HorizonsClient[F],
    emailConfig:          Config.Email,
    introspectionService: GraphQLService[F]
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    OdbMapping.Topics(pool).flatMap { topics =>

      // Sometimes we get invalid cursors on startup; this works around the error by doing the thing again.
      extension [A](fa: F[A]) def retryOnInvalidCursorName: F[A] =
        fa.recoverWith {
          case SqlState.InvalidCursorName(_) =>
            warn"Invalid cursor; retrying (once)." >> fa
        }

      // Log a message with the user
      def info(user: User, message: String): F[Unit] =
        info"${user.id}/${user.displayName}: $message"

      def error(user: User, message: String, t: Throwable): F[Unit] =
        L.error(t)(s"${user.id}/${user.displayName}: $message")

      def debug(user: User, message: String): F[Unit] =
        Logger[F].debug(s"${user.id}/${user.displayName}: $message")

      Cache.timed[F, Authorization, Option[GraphQLService[F]]](ttl).map { cache => wsb =>
        LucumaGraphQLRoutes.forService[F](
          {
            case None    => introspectionService.some.pure[F] // only allow introspection
            case Some(a) =>
              cache.get(a).flatMap {
                case Some(opt) =>
                  debug"Cache hit for $a".as(opt) // it was in the cache
                case None    =>           // It was not in the cache
                  debug"Cache miss for $a" *>
                  T.span("newServiceInstance").surround:
                    {
                      for {
                        user <- OptionT(ssoClient.get(a))
                        props = Attributes.from(user)

                        // If the user has never hit the ODB using http then there will be no user
                        // entry in the database. So go ahead and [re]canonicalize here to be sure.
                        _    <- OptionT.liftF(Services.asSuperUser(userSvc.canonicalizeUser(user).retryOnInvalidCursorName))

                        _    <- OptionT.liftF(info(user, s"New service instance."))
                        map   = OdbMapping(pool, monitor, user, topics, gaiaClient, itcClient, commitHash, goaUsers, enums, ptc, httpClient, horizonsClient, emailConfig)
                        svc   = new GraphQLService(map, props.toList*) {
                                  override def query(
                                    request:       Operation,
                                    document:      String,
                                    operationName: Option[String]
                                  ): F[Result[Json]] =
                                    T.spanBuilder("graphql-query")
                                      .withSpanKind(SpanKind.Server)
                                      .build
                                      .surround:
                                        super.query(request, document, operationName).retryOnInvalidCursorName
                                          .handleError(Result.InternalError.apply)
                                          .flatTap {
                                            case Result.InternalError(t) => error(user, s"Internal error: ${t.getClass.getSimpleName}: ${t.getMessage}", t)
                                            case _                       => debug(user, s"Query (success).")
                                          }

                                  override def subscribe(
                                    request:       Operation,
                                    document:      String,
                                    operationName: Option[String]
                                  ): Stream[F, Result[Json]] =
                                    val spanResource =
                                      T.spanBuilder("graphql-subscription")
                                        .withSpanKind(SpanKind.Server)
                                        .build
                                        .resource
                                    Stream.resource(spanResource).flatMap: res =>
                                      super.subscribe(request, document, operationName)
                                        // use `res.trace` to make it the current context for each inner effect
                                        .translate(res.trace)
                                }
                      } yield svc
                    } .widen[GraphQLService[F]]
                      .value
                      .flatTap(os => cache.put(a, os))
              }
          },
          wsb,
          "odb"
        )
      }
    }

  // The metadata service is gone and new versions of explore don't need it, however
  // in order for the service workers in old versions of explore to keep working until
  // the point where they can update themselves, we need to provide this endpoint...
  def dummyMetadata[F[_]: Temporal]: HttpRoutes[F] =
    val dummyEnumMetadata: String = """export const enumMetadata ='{"filterTypeMeta":[{"tag":"BROAD_BAND","shortName":"Broad-Band","longName":"Broad-Band Filter"},{"tag":"COMBINATION","shortName":"Combination","longName":"Combination Filter"},{"tag":"ENGINEERING","shortName":"Engineering","longName":"Engineering Filter"},{"tag":"NARROW_BAND","shortName":"Narrow-Band","longName":"Narrow-Band Filter"},{"tag":"SPECTROSCOPIC","shortName":"Spectroscopic","longName":"Spectroscopic Filter"}],"proposalStatusMeta":[{"tag":"NOT_SUBMITTED","name":"Not Submitted"},{"tag":"SUBMITTED","name":"Submitted"},{"tag":"ACCEPTED","name":"Accepted"},{"tag":"NOT_ACCEPTED","name":"Not Accepted"}]}'"""
    val dsl = new Http4sDsl[F]{}; import dsl._
    HttpRoutes.of[F]:
      case GET -> Root / "export" / "enumMetadata" =>
        Ok(dummyEnumMetadata)
          .map(_.withContentType(`Content-Type`(MediaType.application.json)))
}
