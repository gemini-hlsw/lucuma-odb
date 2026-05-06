// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import _root_.skunk.Session
import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import fs2.io
import fs2.io.IOException
import fs2.text
import grackle.*
import grackle.Result.Failure
import grackle.Result.Success
import grackle.Result.Warning
import grackle.skunk.SkunkMonitor
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
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
    resourceAsStream("/graphql/resource.graphql")
      .through(text.utf8.decode)
      .compile
      .string
      .flatMap: schemaStr =>
        Schema(schemaStr) match
          case Result.Success(schema)           =>
            Logger[F]
              .info("Loaded GraphQL schema")
              .as(schema)
          case Result.Warning(problems, schema) =>
            Logger[F]
              .warn(s"Loaded schema with problems: ${problems.map(_.message).toList.mkString(",")}")
              .as(schema)
          case Result.Failure(problems)         =>
            Sync[F].raiseError[Schema](
              new Throwable(
                s"Unable to load schema because: ${problems.map(_.message).toList.mkString(",")}"
              )
            )
          case Result.InternalError(error)      =>
            Sync[F].raiseError[Schema](
              new Throwable(s"Unable to load schema because: ${error.getMessage}")
            )

  private def resourceAsStream[F[_]: Sync](name: String): Stream[F, Byte] =
    Stream.eval(Sync[F].blocking(Option(getClass.getResourceAsStream(name)))).flatMap {
      case Some(resource) => io.readInputStream(Sync[F].pure(resource), 8192)
      case None           => Stream.raiseError[F](new IOException(s"Resource $name not found"))
    }
