// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect._
import cats.implicits._
import edu.gemini.grackle.skunk.SkunkMonitor
import lucuma.graphql.routes.GrackleGraphQLService
import lucuma.graphql.routes.{ Routes => LucumaGraphQLRoutes }
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s._
import org.typelevel.log4cats.Logger
import skunk.Session
import org.http4s.server.websocket.WebSocketBuilder2
import cats.data.OptionT
import lucuma.core.model.User

object GraphQLRoutes {

  def apply[F[_]: Async: Trace: Logger](
    client:   SsoClient[F, User],
    pool:     Resource[F, Session[F]],
    channels: OdbMapping.Channels[F],
    monitor:  SkunkMonitor[F],
    wsb:      WebSocketBuilder2[F],
  ): HttpRoutes[F] =
    LucumaGraphQLRoutes.forService[F](
      oa => {
        for {
          auth <- OptionT.fromOption[F](oa)
          user <- OptionT(client.get(auth))
          map  <- OptionT.liftF(OdbMapping(channels, pool, monitor).map(_(user)))
        } yield new GrackleGraphQLService(map)
      } .widen.value,
      wsb
    )

}


