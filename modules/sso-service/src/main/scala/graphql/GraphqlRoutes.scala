// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql

import cats.effect.*
import cats.implicits.*
import grackle.Schema
import grackle.skunk.SkunkMonitor
import lucuma.common.middleware.IntrospectionMapping
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

  def apply[F[_]: Async: Trace: Tracer: Logger](
    client:   SsoClient[F, StandardUser],
    pool:     Resource[F, Session[F]],
    channels: SsoMapping.Channels[F],
    monitor:  SkunkMonitor[F],
    wsb:      WebSocketBuilder2[F],
    schema: Schema,
  ): HttpRoutes[F] =
    val introspectionService = new GraphQLService(IntrospectionMapping[F](schema)).some
    LucumaGraphQLRoutes.forService[F](
      oa =>
        oa.flatTraverse(client.get)
          .map:
            case None       =>
              introspectionService // no auth: only schema introspection is allowed
            case Some(user) =>
              new GraphQLService(SsoMapping(channels, pool, monitor, schema)(user)).some,
      wsb
    )

}
