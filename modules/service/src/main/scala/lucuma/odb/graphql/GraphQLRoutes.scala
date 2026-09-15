// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Parallel
import cats.effect.*
import cats.effect.std.SecureRandom
import cats.implicits.*
import fs2.Stream
import grackle.Mapping
import grackle.Operation
import grackle.Result
import grackle.Schema
import grackle.skunk.SkunkMonitor
import io.circe.Json
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.goa.GoaClient
import lucuma.common.middleware.UserContext
import lucuma.core.model.User
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.RequestContext
import lucuma.graphql.routes.Routes as LucumaGraphQLRoutes
import lucuma.graphql.routes.RoutesConfig
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.mapping.ConeCandidatesMapping
import lucuma.odb.graphql.mapping.UserEnv
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.sso.client.SsoClient
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.SpanFinalizer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import skunk.Session
import skunk.SqlState

import java.nio.file.Files
import java.nio.file.Path as NIOPath
import scala.concurrent.duration.*

object GraphQLRoutes {

  /** Finalization strategy for GraphQL *subscription* spans: a client disconnect (cancellation) is
    * a normal lifecycle event, not an error.
    */
  private val subscriptionFinalizer: SpanFinalizer.Strategy = {
    case Resource.ExitCase.Errored(e) =>
      SpanFinalizer.recordException(e) |+| SpanFinalizer.setStatus(StatusCode.Error)
  }

  /**
   * Construct a source of `HttpRoutes`. One `GraphQLService` serves every request; the user of a
   * request reaches the mapping through the `Env` that the authenticator supplies, and the
   * authenticator caches its results for `ttl`.
   */
  def apply[F[_]: {Async as F, Parallel, Tracer as T, Logger as L, LoggerFactory, SecureRandom}](
    gaiaClient:           GaiaClient[F],
    itcClient:            ItcClient[F],
    commitHash:           CommitHash,
    goaUsers:             Set[User.Id],
    ssoClient:            SsoClient[F, User],
    pool:                 Resource[F, Session[F]],
    monitor:              SkunkMonitor[F],
    ttl:                  FiniteDuration,
    userSvc:              UserService[F],
    ptc:                  TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:           Client[F],
    horizonsClient:       HorizonsClient[F],
    goaClient:            GoaClient[F],
    emailConfig:          Config.Email,
    schema:               Schema,
    validateMapping:      Boolean
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    OdbMapping.Topics(pool).flatMap { topics =>

      // One mapping for the whole server. Each request supplies its user through the env.
      val odbMapping: Mapping[F] & ConeCandidatesMapping[F] =
        OdbMapping(pool, monitor, topics, gaiaClient, itcClient, commitHash, goaUsers, ptc, httpClient, horizonsClient, goaClient, emailConfig, schema, shouldValidate = false)

      // Validate here, not in `GraphQLService.apply`: that validates on the calling thread, and the
      // ODB mapping can overflow the default thread stack. `OdbMapping.validate` uses a thread with
      // an 8 MB stack. When a Grackle release fixes the stack overflow, use `GraphQLService.apply`.
      val validateOnce: Resource[F, Unit] =
        Resource.eval:
          OdbMapping.validate(odbMapping).whenA(validateMapping)

      // Sometimes we get invalid cursors on startup; this works around the error by doing the thing again.
      extension [A](fa: F[A]) def retryOnInvalidCursorName: F[A] =
        fa.recoverWith {
          case SqlState.InvalidCursorName(_) =>
            warn"Invalid cursor; retrying (once)." >> fa
        }

      // Log a message with the user of the request.
      def label(u: User): String =
        s"${u.id}/${u.displayName}"

      def describe(ctx: RequestContext): String =
        UserEnv.fromEnv(ctx.env).toOption.fold("<anonymous>")(label)

      def error(ctx: RequestContext, message: String, t: Throwable): F[Unit] =
        L.error(t)(s"${describe(ctx)}: $message")

      def debug(ctx: RequestContext, message: String): F[Unit] =
        L.debug(s"${describe(ctx)}: $message")

      // Unvalidated, because `validateOnce` validates the mapping with a larger thread stack.
      val service: GraphQLService[F] =
        new GraphQLService[F](odbMapping) {

          override def query(
            ctx:           RequestContext,
            request:       Operation,
            document:      String,
            operationName: Option[String]
          ): F[Result[Json]] =

            def runQuery(req: Operation): F[Result[Json]] =
              super.query(ctx, req, document, operationName).retryOnInvalidCursorName

            // SC-9240: elaboration turns a `targetCoordinates` cone into a
            // placeholder predicate, because the candidate lookup it needs
            // is an F effect. Resolve those to `id IN (...)` here, where we
            // are in F, so the whole WHERE pushes down to one SQL statement.
            // Queries without a cone are returned untouched. The lookup
            // streams through the same pooled sessions as the query itself,
            // so it gets the same invalid-cursor retry.
            def resolveAndRun: F[Result[Json]] =
              UserEnv.traverse(UserEnv.fromEnv(ctx.env)):
                ConeFilter.resolve(request.query)(odbMapping.configurationRequestConeCandidates(_), odbMapping.observationConeCandidates(_)).retryOnInvalidCursorName.flatMap:
                  case Result.Success(q)       => runQuery(request.copy(query = q))
                  case Result.Warning(ps, q)   => runQuery(request.copy(query = q)).map(r => Result.Warning(ps, ()).flatMap(_ => r))
                  case f: Result.Failure       => F.pure(f)
                  case e: Result.InternalError => F.pure(e)

            T.spanBuilder("graphql-query")
              .withSpanKind(SpanKind.Server)
              .build
              .use: span =>
                F.timed(
                  resolveAndRun
                    .handleError(Result.InternalError.apply)
                    .flatTap {
                      case Result.InternalError(t) => error(ctx, s"Internal error: ${t.getClass.getSimpleName}: ${t.getMessage}", t)
                      case _                       => debug(ctx, s"Query (success).")
                    }
                ).flatMap: (elapsed, result) =>
                  val slow = elapsed > OdbMapping.slowQueryThreshold
                  val markSlow =
                    span.addAttribute(Attribute("graphql.slow_query", true)).whenA(slow)
                  val dumpGql: F[Unit] =
                    OdbMapping.dumpDir.filter(_ => slow).map: dir =>
                      F.blocking:
                        val hash = Integer.toHexString(document.hashCode)
                        val path = NIOPath.of(dir, s"odb-query-$hash.gql")
                        if !Files.exists(path) then {Files.writeString(path, document);()}
                    .getOrElse(F.unit)
                  markSlow *> dumpGql.as(result)

          override def subscribe(
            ctx:           RequestContext,
            request:       Operation,
            document:      String,
            operationName: Option[String]
          ): Stream[F, Result[Json]] =
            val spanResource =
              T.spanBuilder("graphql-subscription")
                .withSpanKind(SpanKind.Server)
                .modifyState(_.withFinalizationStrategy(subscriptionFinalizer))
                .build
                .resource
            Stream.resource(spanResource).flatMap: res =>
              super.subscribe(ctx, request, document, operationName)
                // use `res.trace` to make it the current context for each inner effect
                .translate(res.trace)
        }

      // Resolves the user of a request. If the user has never hit the ODB using http then there
      // will be no user entry in the database, so go ahead and [re]canonicalize here to be sure.
      val authenticator =
        UserContext.authenticator(
          ssoClient,
          u => Services.asSuperUser(userSvc.canonicalizeUser(u).retryOnInvalidCursorName) *> info"${label(u)}: Authenticated."
        )

      for {
        _    <- validateOnce
        auth <- Resource.eval(authenticator.cached(ttl))
      } yield wsb =>
        LucumaGraphQLRoutes.forService[F](service, auth, wsb, RoutesConfig(graphQLPath = "odb"))

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
          .map(_.withContentType(`Content-Type`(MediaType.application.javascript)))
}
