// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.test

import cats.effect.*
import ciris.Secret
import com.comcast.ip4s.*
import com.dimafeng.testcontainers.GenericContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import lucuma.common.config.DatabaseConnection
import lucuma.common.config.DatabaseSsl
import lucuma.common.config.Environment
import lucuma.otel.OtelServices
import lucuma.otel.OtelServices.given
import lucuma.otel.OtelSetup
import lucuma.resource.ResourceBaseSuite
import munit.catseffect.IOFixture
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.builder.ImageFromDockerfile
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import resource.model.config.DatabaseConfiguration
import resource.server.http4s.ResourceMain
import skunk.Session

import java.nio.file.Paths
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.MapHasAsJava

object ResourceTestDb:
  val dbImageEnv: Map[String, String] = Map(
    "POSTGRES_USER"     -> PostgreSQLContainer.defaultUsername,
    "POSTGRES_PASSWORD" -> PostgreSQLContainer.defaultPassword,
    "POSTGRES_DB"       -> PostgreSQLContainer.defaultDatabaseName
  )

  /**
   * CI prebuilds this image with a cached docker build and points RESOURCE_TEST_DB_IMAGE at it (see
   * githubWorkflowBuildPreamble in build.sbt). When the variable is not set, build the image
   * locally.
   */
  val dbImage: GenericContainer.DockerImage =
    sys.env.get("RESOURCE_TEST_DB_IMAGE") match
      case Some(prebuilt) =>
        prebuilt
      case None           =>
        val dockerPrefix = Paths.get("resource", "service")
        val dockerSuffix = Paths.get("src", "Dockerfile")
        val dockerPath   =
          if (Paths.get(".").toAbsolutePath.normalize.endsWith(dockerPrefix))
            dockerSuffix
          else
            dockerPrefix.resolve(dockerSuffix)

        new ImageFromDockerfile("lucuma-resource-test-db")
          .withDockerfile(dockerPath)
          .withBuildArgs(dbImageEnv.asJava)

trait DatabaseFixtures
    extends munit.CatsEffectSuite
    with ResourceBaseSuite
    with TestContainerForAll:

  /**
   * A PostgreSQL container from an image with all migrations already applied at image-build time
   * (see [[ResourceTestDb.dbImage]]), so the fixture does not run Flyway.
   */
  override val containerDef: GenericContainer.Def[GenericContainer] =
    new GenericContainer.Def(
      GenericContainer(
        ResourceTestDb.dbImage,
        env = ResourceTestDb.dbImageEnv,
        exposedPorts = Seq(POSTGRESQL_PORT),
        waitStrategy = Wait
          .forLogMessage(".*database system is ready to accept connections.*", 1)
          .withStartupTimeout(java.time.Duration.ofSeconds(15))
      )
    ) {}

  var container: GenericContainer = null.asInstanceOf[GenericContainer]

  override def afterContainersStart(c: Containers): Unit =
    container = c.asInstanceOf[GenericContainer]

  def session: Resource[IO, Session[IO]] =
    given TracerProvider[IO] = TracerProvider.noop
    given MeterProvider[IO]  = MeterProvider.noop
    databaseConfig.connection.sessionBuilder[IO]("lucuma-resource-test").single

  protected def databaseConfig: DatabaseConfiguration =
    DatabaseConfiguration(
      connection = DatabaseConnection(
        host = Host.fromString(container.host).get,
        port = Port.fromInt(container.mappedPort(POSTGRESQL_PORT)).get,
        database = PostgreSQLContainer.defaultDatabaseName,
        user = PostgreSQLContainer.defaultUsername,
        password = Secret(PostgreSQLContainer.defaultPassword),
        ssl = DatabaseSsl.Prefer
      ),
      maxConnections = 10,
      resetDatabase = false,
      skipMigration = true // migrations are baked into the container image
    )

trait ServerFixtures extends DatabaseFixtures:

  override def munitFixtures = super.munitFixtures ++ List(serverFixture)

  lazy val serverFixture: IOFixture[Server] = ResourceSuiteLocalFixture("server", server)

  private def server: Resource[IO, Server] =
    for
      config                 <- IO(databaseConfig).toResource
      given OtelServices[IO] <- OtelSetup.noop[IO]("test").toResource
      a                      <- ResourceMain
                                  .routesResource[IO](config, Environment.Production, Seq("unused"), TestSso.ssoClient)
      s                      <- EmberServerBuilder
                                  .default[IO]
                                  .withHost(ipv4"0.0.0.0")
                                  .withPort(port"0")
                                  .withHttpWebSocketApp(a)
                                  .withShutdownTimeout(2.seconds)
                                  .build
    yield s
