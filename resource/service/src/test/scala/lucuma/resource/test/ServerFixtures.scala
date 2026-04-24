// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.test

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import lucuma.resource.ResourceBaseSuite
import munit.catseffect.IOFixture
import natchez.Trace
import org.http4s.*
import org.http4s.Uri.Host
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import resource.model.config.DatabaseConfiguration
import resource.server.http4s.ResourceMain

import scala.concurrent.duration.*

trait ServerFixtures extends munit.CatsEffectSuite with ResourceBaseSuite:

  given Trace[IO]          = Trace.Implicits.noop
  given TracerProvider[IO] = TracerProvider.noop
  given Tracer[IO]         = Tracer.Implicits.noop
  given MeterProvider[IO]  = MeterProvider.noop

  override def munitFixtures = super.munitFixtures ++ List(serverFixture)

  lazy val serverFixture: IOFixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  private lazy val server: Resource[IO, Server] =
    for
      a <- ResourceMain.routesResource[IO](databaseConfig).map(_.map(_.orNotFound))
      s <- EmberServerBuilder
             .default[IO]
             .withHost(ipv4"0.0.0.0")
             .withPort(port"0")
             .withHttpWebSocketApp(a)
             .withShutdownTimeout(2.seconds)
             .build
    yield s

  protected lazy val databaseConfig: DatabaseConfiguration =
    DatabaseConfiguration(
      maxConnections = 10,
      host = Host.unsafeFromString("localhost"),
      port = port"5432",
      user = "jimmy",
      password = "banana",
      database = "lucuma-odb"
    )
