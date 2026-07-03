// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import org.http4s.Uri
import cats.syntax.eq.*

object CorsMiddleware:
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
