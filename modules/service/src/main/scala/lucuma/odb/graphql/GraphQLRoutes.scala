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
import lucuma.odb.util.Cache
import scala.concurrent.duration._

object GraphQLRoutes {

  implicit val x: Order[Authorization] =
    Order.by(Header[Authorization].value)

  /**
   * Construct a source of `HttpRoutes` tailored to the requesting user. Routes will be cached
   * based on the `Authorization` header and discarded when `ttl` expires.
   */
  def apply[F[_]: Async: Trace: Logger](
    client:   SsoClient[F, User],
    pool:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    ttl:      FiniteDuration,
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    Cache.timed[F, Authorization, Option[GraphQLService[F]]](ttl).map { cache => wsb =>
      LucumaGraphQLRoutes.forService[F](
        {
          case None    => none.pure[F]  // No auth, no service (for now)
          case Some(a) =>
            cache.get(a).flatMap {
              case Some(opt) =>
                Logger[F].info(s"Cache hit for $a").as(opt) // it was in the cache
              case None    =>           // It was not in the cache
                Logger[F].info(s"Cache miss for $a") *>
                {
                  for {
                    user <- OptionT(client.get(a))
                    map  <- OptionT.liftF(OdbMapping(pool, monitor, user))
                    svc   = new GrackleGraphQLService(map)
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


