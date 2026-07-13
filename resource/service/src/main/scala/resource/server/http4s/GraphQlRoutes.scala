// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import _root_.skunk.Session
import cats.ApplicativeThrow
import cats.effect.*
import cats.syntax.all.*
import grackle.*
import grackle.skunk.SkunkMonitor
import lucuma.common.middleware.UserAttributes.given
import lucuma.core.model.User
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
import lucuma.odb.graphql.schema.SchemaStitcher
import lucuma.sso.client.SsoClient
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.trace.Tracer
import resource.server.graphql.ResourceMapping

class GraphQlRoutes[F[_]: {Async, Tracer}](
) extends Http4sDsl[F] {

  private given Logger[F] = Slf4jLogger.getLogger[F]

  def service(
    wsb:       WebSocketBuilder2[F],
    pool:      Resource[F, Session[F]],
    monitor:   SkunkMonitor[F],
    schema:    Schema,
    ssoClient: SsoClient[F, User]
  ): HttpRoutes[F] =
    Routes.forService(
      authorization =>
        authorization
          .flatTraverse(ssoClient.get)
          .map: userOpt =>
            userOpt.map: user =>
              GraphQLService[F](
                ResourceMapping(pool, monitor)(schema),
                Attributes.from(user).toList*
              ),
      wsb
    )
}

object GraphQlRoutes:
  def loadSchema[F[_]: ApplicativeThrow: Logger]: F[Schema] =
    SchemaStitcher.load("graphql/resource.graphql")
