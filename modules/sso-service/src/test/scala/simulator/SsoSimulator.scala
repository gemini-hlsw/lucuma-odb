// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package simulator

import cats.Monad
import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.io.net.Network
import grackle.skunk.SkunkMonitor
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.service.config.Config
import lucuma.sso.service.config.Environment
import lucuma.sso.service.config.OrcidConfig
import lucuma.sso.service.database.Database
import lucuma.sso.service.graphql.GraphQLRoutes
import lucuma.sso.service.graphql.SsoMapping
import lucuma.sso.service.orcid.OrcidService
import natchez.Trace.Implicits.noop
import org.http4s.HttpRoutes
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.middleware.CookieJar
import org.http4s.implicits.*
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.metrics.Meter.Implicits.noop
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

object SsoSimulator {

  /** An OdbClient that just generates the request and returns. */              
  def odbClient[F[_]: Monad](
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
      config   = Config.local(null, None).copy(scheme = Uri.Scheme.https) // no ORCID config since we're faking ORCID
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
