// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package simulator

import cats.Monad
import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*
import com.dimafeng.testcontainers.GenericContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import fs2.io.net.Network
import grackle.skunk.SkunkMonitor
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.service.config.Config
import lucuma.sso.service.config.DatabaseConfig
import lucuma.sso.service.config.Environment
import lucuma.sso.service.config.OrcidConfig
import lucuma.sso.service.database.Database
import lucuma.sso.service.graphql.GraphQLRoutes
import lucuma.sso.service.graphql.mapping.SsoMapping
import lucuma.sso.service.orcid.OrcidService
import munit.Suite
import munit.diff.console.AnsiColors
import natchez.Trace.Implicits.noop
import org.http4s.HttpRoutes
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.middleware.CookieJar
import org.http4s.implicits.*
import org.http4s.server.Router
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.builder.ImageFromDockerfile
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.metrics.Meter.Implicits.noop
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

import java.nio.file.Paths
import scala.jdk.CollectionConverters.MapHasAsJava

trait SsoSimulator extends TestContainerForAll { self: Suite =>

  val debug = true

  val jlogger: org.slf4j.Logger =
    org.slf4j.LoggerFactory.getLogger("lucuma-sso-test-container")

  var container: Containers = null
  override def afterContainersStart(c: GenericContainer): Unit =
    container = c
    
  override val containerDef: GenericContainer.Def[GenericContainer] =
    val env = Map(
      "POSTGRES_USER"     -> PostgreSQLContainer.defaultUsername,
      "POSTGRES_PASSWORD" -> PostgreSQLContainer.defaultPassword,
      "POSTGRES_DB"       -> PostgreSQLContainer.defaultDatabaseName
    )

    // in CI, while running tests from sbt cli, or using vscode test explorer with bloop, the tests
    // start in the root directory of the project. In that case, the dockerfile is in the
    // modules/service/src directory.
    // However, using vscode test explorer with sbt, the tests start in 'modulues/service'.
    // We'll handle both cases here.
    val dockerPrefix = Paths.get("modules", "sso-service")
    val dockerSuffix = Paths.get("src", "Dockerfile")
    val dockerPath = if (Paths.get(".").toAbsolutePath.normalize.endsWith(dockerPrefix))
      dockerSuffix
    else
      dockerPrefix.resolve(dockerSuffix)

    val image = new ImageFromDockerfile("lucuma-sso-test-db")
      .withDockerfile(dockerPath)
      .withBuildArgs(env.asJava)

    val dbContainer = GenericContainer(
      image,
      env = env,
      exposedPorts = Seq(POSTGRESQL_PORT),
      waitStrategy = Wait
        .forLogMessage(".*database system is ready to accept connections.*", 1)
        .withStartupTimeout(java.time.Duration.ofSeconds(15))
    )
    if (debug) {
      dbContainer.container.withLogConsumer { f =>
        jlogger.debug(s"${AnsiColors.CYAN}${f.getUtf8String().trim()}${AnsiColors.Reset}")
      }: Unit
    }
    new GenericContainer.Def(dbContainer) {}


  object SsoSimulator {

    val config = 
      Config.local(null, None)
        .copy(scheme = Uri.Scheme.https) // no ORCID config since we're faking ORCID
        .copy(database = DatabaseConfig(
          host     = container.containerIpAddress,
          port     = container.mappedPort(POSTGRESQL_PORT),
          database = PostgreSQLContainer.defaultDatabaseName,
          user     = PostgreSQLContainer.defaultUsername,
          password = Some(PostgreSQLContainer.defaultPassword),
        ))

    // An OdbClient that just generates the request and returns.
    private def odbClient[F[_]: Monad](
      ssoJwtWriter: SsoJwtWriter[F],
      odbRootUri: Uri,
      serviceUser: ServiceUser
    ): OdbClient[F] =
      new OdbClient[F]:
        def transferOwnership(from: GuestUser, to: StandardUser): F[Unit] =
          chownRequest(from, to, ssoJwtWriter, odbRootUri, serviceUser).void

    // The exact same routes and database used by SSO, but a fake ORCID back end
    private def httpRoutes[F[_]: Async: Console: Logger: Network]: Resource[F, (Resource[F, Database[F]], OrcidSimulator[F], HttpRoutes[F], SsoJwtReader[F], SsoJwtWriter[F])] =
      for {
        sim     <- Resource.eval(OrcidSimulator[F])
        pool    <- FMain.databasePoolResource[F](config.database)
        chans   <- SsoMapping.Channels(pool)
        dbPool   = pool.map(Database.fromSession(_))
        svcUser <- dbPool.evalMap(_.getSsoServiceUser)
        schema  <- Resource.eval(SsoMapping.loadSchema[F])
      } yield (dbPool, sim, Routes[F](
          dbPool    = dbPool,
          orcid     = OrcidService(OrcidConfig.orcidHost(Environment.Production), "unused", "unused", sim.client),
          odb       = odbClient[F](config.ssoJwtWriter, config.odbRootUri, svcUser),
          jwtReader = config.ssoJwtReader,
          jwtWriter = config.ssoJwtWriter,
          publicUri = config.publicUri,
          cookies   = CookieService[F]("lucuma.xyz", true),
          cookieDomain = "lucuma.xyz",
        ) <+> GraphQLRoutes(
          LocalSsoClient(config.ssoJwtReader, dbPool).collect { case su: StandardUser => su },
          pool,
          chans,
          SkunkMonitor.noopMonitor[F],
          null, // !!!
          schema
        ), config.ssoJwtReader, config.ssoJwtWriter)

    /** An Http client that hits an SSO server backed by a simulated ORCID server. */
    def apply[F[_]: Async: Logger: Console: Network]: Resource[F, (Resource[F, Database[F]], OrcidSimulator[F], Client[F], SsoJwtReader[F], SsoJwtWriter[F])] = {
      httpRoutes[F].flatMap { case (pool, sim, routes, reader, writer) =>
        val client = Client.fromHttpApp(Router("/" -> routes).orNotFound)
        val clientʹ = Client[F] { req =>
          for {
            _   <- Resource.eval(Logger[F].debug(s"""Request(method=${req.method}, uri=${req.uri}, headers=${req.headers})"""))
            res <- client.run(req)
            _   <- Resource.eval(Logger[F].debug(s"""Response(status=${res.status}, headers=${res.headers})"""))
          } yield res
        }
        Resource.eval(CookieJar.impl[F](clientʹ)).map { client =>
          (pool, sim, client, reader, writer)
        }
      }
    } 

  }
}
