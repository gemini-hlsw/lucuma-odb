// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import buildinfo.BuildInfo
import cats.effect.*
import cats.effect.std.Console
import cats.effect.syntax.all.*
import com.comcast.ip4s.*
import fs2.*
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.*
import grackle.Schema
import grackle.skunk.SkunkMonitor
import lucuma.otel.OtelSetup
import natchez.Trace
import org.http4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import resource.model.config.*
import skunk.*

import ServerMiddleware.Middleware

object ResourceMain extends IOApp.Simple {

  override def run: IO[Unit] = webServer[IO].useForever

  def webServer[F[_]: {Async, LiftIO, Compression, Console, Files, Network}]: Resource[F, Server] =
    for
      given Logger[F]         = Slf4jLogger.getLoggerFromName[F]("resource")
      conf                   <- ResourceConfiguration.fromCiris.load[F].toResource
      _                      <- printBanner(conf).toResource
      otel                   <- OtelSetup.resource(
                                  "Resource Server",
                                  BuildInfo.gitHeadCommit.getOrElse("000000"),
                                  conf.otel
                                )
      given Trace[F]          = otel.trace
      given Tracer[F]         = otel.tracer
      given TracerProvider[F] = otel.tracerProvider
      given MeterProvider[F]  = otel.meterProvider
      pool                   <- databasePool(conf.database)
      schema                 <- GraphQlRoutes.loadSchema[F].toResource
      r                       = routes(pool, SkunkMonitor.noopMonitor[F], schema)
      middleware             <- ServerMiddleware().toResource
      s                      <- server(conf, r, middleware)
    yield s

  def routes[F[_]: {Async, Files, Tracer}](
    pool:    Resource[F, Session[F]],
    monitor: SkunkMonitor[F],
    schema:  Schema
  )(wsb: WebSocketBuilder2[F]): HttpRoutes[F]         = Router[F](
    "/"         -> new StaticRoutes().service,
    "/resource" -> new GraphQlRoutes().service(wsb, pool, monitor, schema)
  )

  def server[F[_]: {Async, Network}](
    conf:       ResourceConfiguration,
    app:        WebSocketBuilder2[F] => HttpRoutes[F],
    middleware: Middleware[F]
  ): Resource[F, Server] = EmberServerBuilder
    .default[F]
    .withHost(ipv4"0.0.0.0")
    .withPort(conf.port)
    .withHttpWebSocketApp(wsb => middleware(app(wsb)).orNotFound)
    .build

  def databasePool[F[_]: {Temporal, Trace, Console, Network}](
    config: DatabaseConfiguration
  ): Resource[F, Resource[F, Session[F]]] =
    Session.pooled[F](
      host = config.host.renderString,
      port = config.port.value,
      user = config.user,
      password = Some(config.password),
      database = config.database,
      ssl = SSL.Trusted.withFallback(true),
      max = config.maxConnections
    )

  private def printBanner[F[_]: Logger](conf: ResourceConfiguration): F[Unit] = {
    val runtime    = Runtime.getRuntime
    val memorySize = java.lang.management.ManagementFactory
      .getOperatingSystemMXBean()
      .asInstanceOf[com.sun.management.OperatingSystemMXBean]
      .getTotalMemorySize()

    val banner = """
▄▄▄▄▄▄▄                                             
███▀▀███▄                                           
███▄▄███▀ ▄█▀█▄ ▄█▀▀▀ ▄███▄ ██ ██ ████▄ ▄████ ▄█▀█▄ 
███▀▀██▄  ██▄█▀ ▀███▄ ██ ██ ██ ██ ██ ▀▀ ██    ██▄█▀ 
███  ▀███ ▀█▄▄▄ ▄▄▄█▀ ▀███▀ ▀██▀█ ██    ▀████ ▀█▄▄▄ 

"""

    val msg =
      s"""Starting Resource Server
          | environment          : ${conf.environment}
          | port                 : ${conf.port}
          | version (git commit) : ${BuildInfo.gitHeadCommit.getOrElse("----")}
          | tracing              : ${conf.otel.fold("No-op (silent)")(c =>
          s"OpenTelemetry (OTLP) endpoint=${c.endpoint} environment=${c.environment}"
        )}
          |
          | cores                : ${runtime.availableProcessors()}
          | current JVM memory   : ${runtime.totalMemory() / 1024 / 1024} MB
          | maximum JVM memory   : ${runtime.maxMemory() / 1024 / 1024} MB
          | total RAM            : ${memorySize / 1024 / 1024} MB
          | java version         : ${System.getProperty("java.version")}
          |""".stripMargin

    Logger[F].info(banner + msg)
  }

}
