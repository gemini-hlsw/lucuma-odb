// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.example

import cats.*
import cats.effect.*
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import fs2.io.net.Network
import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoMiddleware
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.NatchezMiddleware
import natchez.http4s.implicits.*
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Server
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.ErrorAction
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.annotation.unused
import scala.concurrent.duration.*

object Main extends IOApp {

  val host: Host =
    Host.fromString("0.0.0.0").getOrElse(sys.error("unpossible: invalid host"))

  implicit val logger: Logger[IO] =
    Slf4jLogger.getLoggerFromName("lucuma-sso")

  // A normal server.
  def serverResource[F[_]: Async: Network](
    port: Port,
    app:  HttpApp[F]
  ): Resource[F, Server] =
    EmberServerBuilder
      .default[F]
      .withHost(host)
      .withHttpApp(app)
      .withPort(port)
      .build

  // Our routes use an `SsoClient` that knows how to extract a `User`.
  def routes[F[_]: Monad](
    userClient: SsoClient[F, User]
  ): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F]; import dsl._
    HttpRoutes.of[F] {
      case r@(GET -> Root / "echo" / greeting) =>
        userClient.require(r) { user =>
          Ok(s"$greeting ${user.displayName}")
        } // otherwise we will respond 403 Forbidden
    }
  }

  // Our routes, with middleware
  def wrappedRoutes[F[_]: Async: Trace: Network: Logger](
    cfg: Config
  ): Resource[F, HttpRoutes[F]] =
    cfg.ssoClient[F].map { ssoClient =>
      val userClient = ssoClient.map(_.user) // we only want part of the UserInfo
      NatchezMiddleware.server(SsoMiddleware(userClient)(routes[F](userClient)))
    }

  def entryPoint[F[_]: Sync](cfg: Config): Resource[F, EntryPoint[F]] =
    Honeycomb.entryPoint("backend-example") { cb =>
      Sync[F].delay {
        cb.setWriteKey(cfg.hcWriteKey)
        cb.setDataset(cfg.hcDataset)
        cb.build()
      }
    }

  def log[F[_]: Async](@unused r: Request[F], t: Throwable): F[Unit] =
    Async[F].delay(t.printStackTrace())

  def cors[F[_]: Monad](routes: HttpRoutes[F], domain: String): HttpRoutes[F] =
    CORS.policy
      .withAllowCredentials(true)
      .withAllowOriginHost(_.host.value.endsWith(domain))
      .withMaxAge(1.day)
      .apply(routes)

  // Our main program as a resource.
  def runR(
    cfg: Config
  ): Resource[IO, Server] =
    for {
      ep      <- entryPoint[IO](cfg)
      routes  <- ep.liftR(wrappedRoutes(cfg))
      httpApp  = ErrorAction.httpRoutes(cors(routes, "lucuma.xyz"), log[IO]).orNotFound
      server  <- serverResource(cfg.port, httpApp)
    } yield server

  // Specialize to IO and we're done.
  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg <- Config.fromCiris.load[IO]
      _   <- runR(cfg).useForever
    } yield ExitCode.Success

}