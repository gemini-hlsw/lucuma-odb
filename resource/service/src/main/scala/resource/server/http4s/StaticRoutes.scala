// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import org.http4s.*
import org.http4s.CacheDirective
import org.http4s.CacheDirective.*
import org.http4s.dsl.io.*
import org.http4s.headers.`Cache-Control`

import scala.concurrent.duration.*

class StaticRoutes[F[_]: {Sync, Files}]:

  private val AppDir: String = "app"

  private val OneYear: FiniteDuration = 365.days

  private val CacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`max-age`(OneYear)))
  )

  // Cache index pages for a short time to avoid stale content
  private val IndexCacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`max-age`(6.hours), `must-revalidate`))
  )

  // /assets/* files are fingerprinted and can be cached for a long time
  val immutable                                     = CacheDirective("immutable")
  private val AssetCacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`public`, `max-age`(OneYear), immutable))
  )

  def localFile(path: String, req: Request[F]): OptionT[F, Response[F]] =
    OptionT
      .liftF(baseDir)
      .flatMap: dir =>
        StaticFile.fromPath(
          Path.fromNioPath(dir.resolve(AppDir).resolve(path.stripPrefix("/"))),
          req.some
        )

  extension (req: Request[F])
    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.toString.endsWith)

    def serve(path: String, headers: List[Header.ToRaw]): F[Response[F]] =
      localFile(path, req)
        .map(_.putHeaders(headers*))
        .getOrElse(Response.notFound[F])

  private val supportedExtension = List(
    ".html",
    ".js",
    ".map",
    ".css",
    ".png",
    ".eot",
    ".svg",
    ".woff",
    ".woff2",
    ".ttf",
    ".mp3",
    ".ico",
    ".webm",
    ".json"
  )

  def service: HttpRoutes[F] =
    HttpRoutes.of[F]:
      case req @ GET -> Root                                                  =>
        req.serve("/index.html", IndexCacheHeaders)
      case req @ GET -> "assets" /: rest if req.endsWith(supportedExtension*) =>
        req.serve(req.pathInfo.toString, AssetCacheHeaders)
      case req if req.endsWith(supportedExtension*)                           =>
        req.serve(req.pathInfo.toString, CacheHeaders)
      // This maybe not desired in all cases but it helps to keep client side routing cleaner
      case req if !req.pathInfo.toString.contains(".")                        =>
        req.serve("/index.html", IndexCacheHeaders)
