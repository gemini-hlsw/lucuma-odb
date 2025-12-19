// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.sso.client.SsoClient
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.typelevel.ci.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*

object ProxyRoutes:

  def apply[F[_]: {Concurrent, LoggerFactory as LF}](
    httpClient:  Client[F],
    ssoClient:   SsoClient[F, User],
    proxyConfig: Config.CORSProxy
  ): HttpRoutes[F] =
    given Logger[F] = LF.getLoggerFromName("cors-proxy")

    val dsl = Http4sDsl[F]
    import dsl.*

    object TargetUrlMatcher extends QueryParamDecoderMatcher[String]("url")

    def allowedDomain(uri: Uri): Boolean =
      uri.host.exists: host =>
        val hostStr = host.value
        proxyConfig.allowedDomains.exists: allowed =>
          hostStr === allowed || hostStr.endsWith(s".$allowed")

    HttpRoutes.of[F]:
      case req @ GET -> Root / "proxy" :? TargetUrlMatcher(targetUrl) =>
        ssoClient.require(req): _ =>
          Uri.fromString(targetUrl) match
            case Left(parseError) =>
              BadRequest(s"Invalid URL: ${parseError.message}")

            case Right(targetUri) =>
              if !allowedDomain(targetUri) then
                val host = targetUri.host.foldMap(_.value)
                warn"Blocked proxy request to domain: $host" *>
                  Forbidden(s"Domain not allowed: $host")
              else
                // Don't propagate auth
                val proxyRequest = Request[F](
                  method = Method.GET,
                  uri = targetUri,
                  headers = Headers(
                    req.headers.headers.filterNot(h =>
                      h.name == CIString("Authorization") ||
                      h.name == CIString("Cookie") ||
                      h.name == CIString("Host")
                    )
                  )
                )

                httpClient.run(proxyRequest).use: response =>
                  Response[F](
                    status = response.status,
                    headers = response.headers,
                    body = response.body
                  ).pure[F]
                .handleErrorWith: error =>
                  Logger[F].error(error)(s"Error proxying request to $targetUri") *>
                    InternalServerError("Proxy request failed")
