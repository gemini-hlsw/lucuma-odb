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
import cats.kernel.Order
import org.http4s.headers.Authorization
import lucuma.graphql.routes.GraphQLService
import scala.collection.immutable.TreeMap

object GraphQLRoutes {

  implicit val x: Order[Authorization] =
    Order.by(Header[Authorization].value)

  def cache[F[_]: Ref.Make]: F[Ref[F, Map[Authorization, Option[GraphQLService[F]]]]] =
    Ref[F].of(TreeMap.empty)

  def apply[F[_]: Async: Trace: Logger](
    client:   SsoClient[F, User],
    pool:     Resource[F, Session[F]],
    channels: OdbMapping.Channels[F],
    monitor:  SkunkMonitor[F],
    wsb:      WebSocketBuilder2[F],
    cache:    Ref[F, Map[Authorization, Option[GraphQLService[F]]]]
  ): HttpRoutes[F] =
    LucumaGraphQLRoutes.forService[F](
      {
        case None    => none.pure[F]  // No auth, no service (for now)
        case Some(a) =>
          cache.get.map(_.get(a)).flatMap {
            case Some(m) => m.pure[F] // It was in the cache
            case None    =>           // It was not in the cache
              {
                for {
                  user <- OptionT(client.get(a))
                  map  <- OptionT.liftF(OdbMapping(channels, pool, monitor, user))
                  svc   = new GrackleGraphQLService(map)
                } yield svc
              } .widen[GraphQLService[F]]
                .value
                .flatTap(os => cache.update(m => m + (a -> os)))
          }
      },
      wsb
    )

}


