// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql

import cats.effect.*
import grackle.skunk.SkunkMonitor
import lucuma.common.middleware.UserContext
import lucuma.core.model.StandardUser
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes as LucumaGraphQLRoutes
import lucuma.sso.client.SsoClient
import lucuma.sso.service.graphql.mapping.SsoMapping
import natchez.Trace
import org.http4s.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import skunk.Session

object GraphQLRoutes {

  /** The GraphQL service of SSO, which serves every request. */
  def service[F[_]: Async: Trace: Tracer: Logger](pool: Resource[F, Session[F]]): Resource[F, GraphQLService[F]] =
    for {
      schema   <- Resource.eval(SsoMapping.loadSchema[F])
      channels <- SsoMapping.Channels(pool)
      service  <- Resource.eval(GraphQLService[F](SsoMapping(channels, pool, SkunkMonitor.noopMonitor[F], schema)))
    } yield service

  def apply[F[_]: Async: Tracer: Logger](
    client:  SsoClient[F, StandardUser],
    service: GraphQLService[F],
    wsb:     WebSocketBuilder2[F],
  ): HttpRoutes[F] =
    LucumaGraphQLRoutes.forService[F](service, UserContext.authenticator(client), wsb)

}
