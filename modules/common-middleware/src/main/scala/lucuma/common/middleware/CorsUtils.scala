// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

object CorsUtils {
  /**
   * Checks if a host matches any of the allowed domains, allowing for exact match
   * or subdomain matching (e.g., "sub.domain.com" matches "domain.com").
   *
   * @param host The host to check (e.g., "sub.example.com")
   * @param allowedDomains A list of trusted domains (e.g., List("example.com"))
   * @return true if the host is allowed, false otherwise
   */
  def isAllowed(host: String, allowedDomains: List[String]): Boolean =
    allowedDomains.exists(d => host == d || host.endsWith("." + d))
}
