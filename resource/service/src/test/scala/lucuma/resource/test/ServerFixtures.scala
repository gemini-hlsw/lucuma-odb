// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.test

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import lucuma.resource.ResourceBaseSuite
import munit.catseffect.IOFixture
import natchez.Trace
import org.http4s.*
import org.http4s.Uri.Host
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import org.testcontainers.utility.DockerImageName
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import resource.model.config.DatabaseConfiguration
import resource.server.http4s.ResourceMain

import scala.concurrent.duration.*

trait ServerFixtures extends munit.CatsEffectSuite with ResourceBaseSuite with TestContainerForAll:

  given Trace[IO]          = Trace.Implicits.noop
  given TracerProvider[IO] = TracerProvider.noop
  given Tracer[IO]         = Tracer.Implicits.noop
  given MeterProvider[IO]  = MeterProvider.noop
  given Meter[IO]          = Meter.Implicits.noop

  override def munitFixtures = super.munitFixtures ++ List(serverFixture)

  override val containerDef =
    PostgreSQLContainer.Def(DockerImageName.parse("postgres:18"))

  var container: PostgreSQLContainer = null.asInstanceOf[PostgreSQLContainer]

  override def afterContainersStart(c: Containers): Unit =
    container = c.asInstanceOf[PostgreSQLContainer]

  lazy val serverFixture: IOFixture[Server] = ResourceSuiteLocalFixture("server", server)

  def session = ResourceMain.singleSession[IO](databaseConfig)

  protected def databaseConfig: DatabaseConfiguration =
    DatabaseConfiguration(
      maxConnections = 10,
      host = Host.unsafeFromString(container.host),
      port = Port.fromInt(container.mappedPort(POSTGRESQL_PORT)).get,
      user = container.username,
      password = container.password,
      database = container.databaseName,
      resetDatabase = false,
      skipMigration = false
    )

  private def server: Resource[IO, Server] =
    for
      config <- IO(databaseConfig).toResource
      _      <- ResourceMain.migrateDatabase[IO](container.jdbcUrl, config).toResource
      a      <- ResourceMain.routesResource[IO](config, true, Seq("unused")).map(_.map(_.orNotFound))
      s      <- EmberServerBuilder
                  .default[IO]
                  .withHost(ipv4"0.0.0.0")
                  .withPort(port"0")
                  .withHttpWebSocketApp(a)
                  .withShutdownTimeout(2.seconds)
                  .build
    yield s
