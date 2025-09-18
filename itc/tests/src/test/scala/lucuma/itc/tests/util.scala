// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.model.Attachment
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
import lucuma.itc.cache.RedisEffectfulCache
import lucuma.itc.input.customSed.CustomSed
import lucuma.itc.input.customSed.CustomSedDatResolver
import lucuma.itc.service.Itc
import lucuma.itc.service.ItcMapping
import lucuma.itc.service.config.Config
import lucuma.itc.service.config.ExecutionEnvironment
import lucuma.itc.service.config.MetricsConfig
import natchez.Trace
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.Uri
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

def app(
  itc: Itc[IO]
)(using Logger[IO], Trace[IO]): IO[WebSocketBuilder2[IO] => HttpApp[IO]] =
  routesForWsb(itc).map { f => (wsb: WebSocketBuilder2[IO]) =>
    f(wsb).orNotFound
  }

given CustomSed.Resolver[IO] = new CustomSedDatResolver[IO] {
  val DummyId   = Attachment.Id.fromLong(1).get
  val InvalidId = Attachment.Id.fromLong(2).get

  override protected def datLines(id: Attachment.Id): IO[fs2.Stream[IO, String]] =
    IO:
      id match
        case DummyId   =>
          fs2.Stream.emits:
            List(
              "# Should skip comments and empty lines",
              "500.0 1.0",
              "600.0 2.0 ignore this",
              "",
              "700.0 3.0"
            )
        case InvalidId =>
          fs2.Stream.emit("someText someOtherText")
        case _         =>
          fs2.Stream.empty
}

def routesForWsb(
  itc: Itc[IO]
)(using Logger[IO], Trace[IO]): IO[WebSocketBuilder2[IO] => HttpRoutes[IO]] =
  val testConfig = Config(
    environment = ExecutionEnvironment.Local,
    port = 6060,
    redisUrl = None,
    odbBaseUrl = Uri.unsafeFromString("http://localhost"),
    odbServiceToken = "",
    honeycomb = None,
    inHeroku = false,
    metrics = MetricsConfig(graphite = None, frequency = 60.seconds),
    cacheTtlDays = 7
  )

  for
    cache  <- RedisEffectfulCache[IO](new NoOpRedis[IO, Array[Byte], Array[Byte]]())
    itcMap <- ItcMapping[IO](ExecutionEnvironment.Local, cache, itc, testConfig)
  yield (wsb: WebSocketBuilder2[IO]) =>
    Routes.forService(_ => IO.pure(GraphQLService(itcMap).some), wsb)

def routes(itc: Itc[IO])(using Logger[IO], Trace[IO]): IO[HttpRoutes[IO]] =
  for
    wsb <- WebSocketBuilder2[IO]
    rts <- routesForWsb(itc)
  yield rts(wsb)
