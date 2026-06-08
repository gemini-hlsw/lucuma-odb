// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import buildinfo.BuildInfo
import cats.effect.*
import cats.effect.std.Console
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.*
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.*
import grackle.Schema
import grackle.skunk.SkunkMonitor
import lucuma.otel.OtelSetup
import natchez.Trace
import org.flywaydb.core.Flyway
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

object ResourceMain extends IOApp.Simple {

  override def run: IO[Unit] = webServer[IO].useForever

  def webServer[F[_]: {Async, LiftIO, Compression, Console, Files, Network}]: Resource[F, Server] =
    for
      given Logger[F] = Slf4jLogger.getLoggerFromName[F]("resource")
      conf           <- ResourceConfiguration.fromCiris.load[F].toResource
      _              <- printBanner(conf).toResource

      _ <- resetDatabase(conf.database).toResource
      _ <- migrateDatabase(conf.database.jdbcUrl, conf.database).toResource

      otel                   <- OtelSetup.resource(
                                  "lucuma-resource",
                                  BuildInfo.gitHeadCommit.getOrElse("000000"),
                                  conf.otel
                                )
      given Trace[F]          = otel.trace
      given Tracer[F]         = otel.tracer
      given TracerProvider[F] = otel.tracerProvider
      given MeterProvider[F]  = otel.meterProvider
      r                      <- routesResource[F](conf.database)
      s                      <- server(conf, r)
    yield s

  def routes[F[_]: {Async, Files, Tracer}](
    pool:            Resource[F, Session[F]],
    monitor:         SkunkMonitor[F],
    schema:          Schema
  )(wsb: WebSocketBuilder2[F]): HttpRoutes[F] = Router[F](
    "/"         -> new StaticRoutes().service,
    "/resource" -> new GraphQlRoutes().service(wsb, pool, monitor, schema)
  )

  def routesResource[
    F[_]: {Async, Trace, Tracer, TracerProvider, MeterProvider, Logger, Console, Network, Files,
      Compression}
  ](
    databaseConfig: DatabaseConfiguration
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for
      pool       <- databasePool(databaseConfig)
      schema     <- GraphQlRoutes.loadSchema[F].toResource
      r           = routes(pool, SkunkMonitor.noopMonitor[F], schema)
      middleware <- ServerMiddleware().toResource
    yield wsb => middleware(r(wsb))

  def server[F[_]: {Async, Network}](
    conf: ResourceConfiguration,
    app:  WebSocketBuilder2[F] => HttpRoutes[F]
  ): Resource[F, Server] = EmberServerBuilder
    .default[F]
    .withHost(ipv4"0.0.0.0")
    .withPort(conf.port)
    .withHttpWebSocketApp(wsb => app(wsb).orNotFound)
    .build

  def databasePool[F[_]: {Temporal, Trace, Console, Network}](
    config: DatabaseConfiguration
  ): Resource[F, Resource[F, Session[F]]] =
    Session.pooled[F](
      host = config.host.renderString,
      port = config.port.value,
      user = config.user,
      password = config.password.some,
      database = config.database,
      ssl = SSL.Trusted.withFallback(true),
      strategy = Strategy.SearchPath,
      max = config.maxConnections
    )

  def singleSession[F[_]: {Temporal, Console, Network}](
    config:   DatabaseConfiguration,
    database: Option[String] = None
  ): Resource[F, Session[F]] =
    import natchez.Trace.Implicits.noop

    Session.single[F](
      host = config.host.renderString,
      port = config.port.value,
      user = config.user,
      password = config.password.some,
      database = database.getOrElse(config.database),
      ssl = SSL.Trusted.withFallback(true),
      strategy = Strategy.SearchPath
    )

  private def printBanner[F[_]: {Logger as L}](conf: ResourceConfiguration): F[Unit] = {
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

    L.info(banner + msg)
  }

  /**
   * Drop and recreate the database.
   */
  def resetDatabase[F[_]: {Temporal, Console, Network, Logger}](
    config: DatabaseConfiguration
  ): F[Unit] =
    import skunk.*
    import skunk.implicits.*

    val drop   = sql"""DROP DATABASE IF EXISTS "#${config.database}"""".command
    val create = sql"""CREATE DATABASE "#${config.database}"""".command

    (Logger[F].warn(s"Resetting database '${config.database}'") *>
      singleSession(config, "postgres".some).use: s =>
        s.execute(drop) *>
          s.execute(create).void).whenA(config.resetDatabase)

  /**
   * A startup action that runs database migrations using Flyway.
   */
  def migrateDatabase[F[_]: {Sync, Logger}](
    jdbcUrl: String,
    config:  DatabaseConfiguration
  ): F[Unit] =
    Sync[F]
      .delay {
        Flyway
          .configure()
          .loggers("slf4j")
          .dataSource(jdbcUrl, config.user, config.password)
          .baselineOnMigrate(true)
          .load()
          .migrate()
      }
      .flatMap: result =>
        Logger[F].info(
          s"Database migration completed with ${result.migrationsExecuted} migrations applied"
        )
      .unlessA(config.skipMigration)

}
