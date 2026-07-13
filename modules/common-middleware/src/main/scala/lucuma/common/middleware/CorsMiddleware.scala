// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.*
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.Uri
import org.http4s.Uri.Scheme
import org.http4s.server.middleware.CORS

import scala.concurrent.duration.*

object CorsMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * Checks if a host matches any of the allowed domains, allowing for exact match
   * or subdomain matching (e.g., "sub.domain.com" matches "domain.com").
   *
   * @param host The host to check (e.g., "sub.example.com")
   * @param allowedDomains A list of trusted domains (e.g., List("example.com"))
   * @return true if the host is allowed, false otherwise
   */
  def isAllowed(host: Uri.Host, allowedDomains: List[String]): Boolean =
    val h = host.value.toLowerCase
    allowedDomains.exists: d =>
      val dd = d.toLowerCase
      h === dd || h.endsWith("." + dd)

  /**
   * A middleware that adds CORS headers. Credentials are allowed, so the origin must match one
   * of the configured domains.
   * When `corsOverHttps` is true, only HTTPS origins are accepted. (plain http is for dev)
   *
   * @param corsOverHttps whether to additionally require the origin to use HTTPS
   * @param domain the trusted domains allowed to make credentialed cross-origin requests
   */
  def cors[F[_]: Monad](corsOverHttps: Boolean = false, domain: List[String]): Middleware[F] =
    CORS.policy
      .withAllowCredentials(true)
      .withAllowOriginHost(u => (!corsOverHttps || (u.scheme === Scheme.https)) && isAllowed(u.host, domain))
      .withMaxAge(1.day)
      .apply
