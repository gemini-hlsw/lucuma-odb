// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import _root_.skunk.Session
import cats.Show
import cats.effect.*
import cats.syntax.all.*
import fs2.io.file.Path
import grackle.*
import grackle.Result.Failure
import grackle.Result.Success
import grackle.Result.Warning
import grackle.skunk.SkunkMonitor
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
import lucuma.odb.graphql.schema.SchemaSource
import lucuma.odb.graphql.schema.SchemaStitcher
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.trace.Tracer
import resource.server.graphql.ResourceMapping

class GraphQlRoutes[F[_]: {Async, Tracer}](
) extends Http4sDsl[F] {

  private given Logger[F] = Slf4jLogger.getLogger[F]

  def service(
    wsb:     WebSocketBuilder2[F],
    pool:    Resource[F, Session[F]],
    monitor: SkunkMonitor[F],
    schema:  Schema
  ): HttpRoutes[F] =
    Routes.forService(
      _ =>
        GraphQLService[F](
          ResourceMapping(
            pool,
            monitor
          )(schema)
        ).some.pure[F],
      wsb
    )
}

object GraphQlRoutes:
  def loadSchema[F[_]: Sync: Logger]: F[Schema] =
    given Show[Problem] = Show.fromToString
    SchemaStitcher(Path("graphql/resource.graphql"), SchemaSource.fromResource).build
      .flatMap:
        case Result.Success(schema)           =>
          Logger[F]
            .info("Loaded GraphQL schema")
            .as(schema)
        case Result.Warning(problems, schema) =>
          Logger[F]
            .warn(s"Loaded schema with problems: ${problems.mkString_(",")}")
            .as(schema)
        case Result.Failure(problems)         =>
          Sync[F].raiseError[Schema](
            new Throwable(
              s"Unable to load schema because: ${problems.mkString_(",")}"
            )
          )
        case Result.InternalError(error)      =>
          Sync[F].raiseError[Schema](
            new Throwable(s"Unable to load schema because: ${error.getMessage}")
          )
