// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats.Applicative
import cats.Functor
import cats.Parallel
import cats.data.Kleisli
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.data.RedisCodec
import dev.profunktor.redis4cats.log4cats.*
import fs2.compression.Compression
import fs2.io.net.Network
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
import lucuma.itc.cache.BinaryEffectfulCache
import lucuma.itc.cache.NoOpBinaryCache
import lucuma.itc.cache.RedisEffectfulCache
import lucuma.itc.input.customSed.CustomSed
import lucuma.itc.input.customSed.CustomSedCachedResolver
import lucuma.itc.input.customSed.CustomSedOdbAttachmentResolver
import lucuma.itc.legacy.FLocalItc
import lucuma.itc.legacy.ItcImpl
import lucuma.itc.legacy.LocalItc
import lucuma.itc.service.config.*
import lucuma.itc.service.config.ExecutionEnvironment.*
import lucuma.itc.service.metrics.MetricsService
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.NatchezMiddleware
import natchez.http4s.implicits.*
import natchez.log.Log
import org.http4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Cache-Control`
import org.http4s.implicits.*
import org.http4s.server.Server
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.CORSPolicy
import org.http4s.server.middleware.GZip
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.File
import java.io.FileFilter
import java.net.URL
import java.net.URLClassLoader
import scala.concurrent.duration.*
import scala.util.Try

// #server
object Main extends IOApp with ItcCacheOrRemote {
  val ServiceName     = "lucuma-itc"
  val DefaultCacheTTL = 6.hours
  // Seems a bit optimistic
  val CustomSedTTL    = 1.minute

  override protected def blockedThreadDetectionEnabled = true

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](cfg: Config): F[Unit] =
    val redis = cfg.redisUrl.fold {
      if (cfg.inHeroku) "required on Heroku but missing!" else "disabled (no-op cache)"
    }("redis server at " + _.toString)

    val runtime     = Runtime.getRuntime
    val localStatus = if (BuildInfo.ocsLocal) " (local)" else ""
    val banner      =
      s"""|
            |   / /_  _________  ______ ___  ____ _      (_) /______
            |  / / / / / ___/ / / / __ `__ \\/ __ `/_____/ / __/ ___/
            | / / /_/ / /__/ /_/ / / / / / / /_/ /_____/ / /_/ /__
            |/_/\\__,_/\\___/\\__,_/_/ /_/ /_/\\__,_/     /_/\\__/\\___/
            |
            | redis                : $redis
            | port                 : ${cfg.port}
            | data checksum        : ${BuildInfo.ocslibHash}
            | version (git commit) : ${BuildInfo.gitHeadCommit.getOrElse("----")}
            | ocs branch           : ${BuildInfo.ocsGitBranch}$localStatus
            | ocs git hash         : ${BuildInfo.ocsGitHash}
            | ocs local            : ${BuildInfo.ocsLocal}
            | odb address          : ${cfg.odbBaseUrl}
            | cores                : ${runtime.availableProcessors()}
            | total memory         : ${runtime.totalMemory() / 1024 / 1024} MB
            | max memory           : ${runtime.maxMemory() / 1024 / 1024} MB
            | java version         : ${System.getProperty("java.version")}
            |""".stripMargin

    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  /** A middleware that adds CORS headers. In production the origin must match the cookie domain. */
  def cors(env: ExecutionEnvironment, domain: Option[String]): CORSPolicy =
    env match
      case Local | Review | Staging =>
        CORS.policy
      case Production               =>
        CORS.policy
          .withAllowOriginHostCi(domain.contains)

  /**
   * A resource that yields a Natchez tracing entry point, either a Honeycomb endpoint if `config`
   * is defined, otherwise a log endpoint.
   */
  def entryPointResource[F[_]: Sync: Logger](
    config: Option[HoneycombConfig]
  ): Resource[F, EntryPoint[F]] =
    config.fold(
      Log.entryPoint(ServiceName).pure[Resource[F, *]]
    ): cfg =>
      Honeycomb.entryPoint(ServiceName): cb =>
        Sync[F].blocking:
          cb.setWriteKey(cfg.writeKey)
          cb.setDataset(cfg.dataset)
          cb.build()

  def cacheMiddleware[F[_]: Functor](service: HttpRoutes[F]): HttpRoutes[F] =
    Kleisli: (req: Request[F]) =>
      service(req).map:
        case Status.Successful(resp) =>
          resp.putHeaders:
            `Cache-Control`(CacheDirective.public, CacheDirective.`max-age`(DefaultCacheTTL))
        case resp                    =>
          resp

  def serverResource[F[_]: Async: Network](
    app: WebSocketBuilder2[F] => HttpApp[F],
    cfg: Config
  ): Resource[F, Server] =
    // Spin up the server ...
    EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(Port.fromInt(cfg.port).get)
      .withHttpWebSocketApp(app)
      .build

  private def createCache[F[_]: Async: Trace: Logger](
    redisUrl: Option[Uri]
  ): Resource[F, BinaryEffectfulCache[F]] =
    redisUrl match
      case Some(url) =>
        for
          redis <- Redis[F].simple(url.toString, RedisCodec.gzip(RedisCodec.Bytes))
          cache <- Resource.eval(RedisEffectfulCache[F](redis))
        yield cache
      case None      =>
        Resource.eval(NoOpBinaryCache[F])

  def routes[F[_]: Async: Logger: Parallel: Trace: Compression: Network](
    cfg: Config,
    itc: LocalItc
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for
      itc                        <- Resource.eval(ItcImpl.build(FLocalItc[F](itc)).pure[F])
      cache                      <- createCache[F](cfg.redisUrl)
      _                          <- Resource.eval(checkVersionToPurge[F](cache))
      customSedResolver          <- CustomSedOdbAttachmentResolver[F](cfg.odbBaseUrl, cfg.odbServiceToken)
      given CustomSed.Resolver[F] = CustomSedCachedResolver(customSedResolver, cache, CustomSedTTL)
      mapping                    <- Resource.eval(ItcMapping[F](cfg.environment, cache, itc, cfg))
    yield wsb =>
      // Routes for the ITC GraphQL service
      NatchezMiddleware.server:
        GZip:
          cors(cfg.environment, none):
            cacheMiddleware:
              Routes.forService(_ => GraphQLService[F](mapping).some.pure[F], wsb, "itc")

  // Custom class loader to give priority to the jars in the urls over the parent classloader
  class ReverseClassLoader(urls: Array[URL], parent: ClassLoader)
      extends URLClassLoader(urls, parent) {
    override def loadClass(name: String): Class[?] =
      // First check whether it's already been loaded, if so use it
      if (name.startsWith("java.lang"))
        super.loadClass(name)
      else
        Option(findLoadedClass(name)).getOrElse:
          // Not loaded, try to load it
          Try(findClass(name)).getOrElse:
            // If not found locally, use normal parent delegation in URLClassloader
            super.loadClass(name)
  }

  // Build a custom class loader to read and call the legacy ocs2 libs
  // without affecting the current classes. This is mostly because ocs2 uses scala 2.11
  // and it will conflict with the current scala 3 classes
  def legacyItcLoader[F[_]: Sync]: F[LocalItc] =
    Sync[F]
      .blocking:
        val jarFiles: Array[File] =
          new File("ocslib").listFiles(new FileFilter() {
            override def accept(file: File): Boolean =
              file.getName().endsWith(".jar");
          })
        LocalItc:
          new ReverseClassLoader(jarFiles.map(_.toURI.toURL), ClassLoader.getSystemClassLoader())

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down in
   * cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server(cfg: Config)(using Logger[IO]): Resource[IO, ExitCode] =
    for
      _  <- Resource.eval(banner[IO](cfg))
      _  <- MetricsService.resource[IO](cfg.metrics)
      cl <- Resource.eval(legacyItcLoader[IO])
      ep <- entryPointResource[IO](cfg.honeycomb)
      ap <- ep.wsLiftR(routes(cfg, cl)).map(_.map(_.orNotFound))
      _  <- serverResource(ap, cfg)
    yield ExitCode.Success

  def run(args: List[String]): IO[ExitCode] =
    for
      cfg              <- Config.config.load[IO]
      given Logger[IO] <- Slf4jLogger.create[IO]
      _                <- server(cfg).use(_ => IO.never[ExitCode])
    yield ExitCode.Success
}
