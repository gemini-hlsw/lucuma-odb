// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.config

import cats.Eq
import cats.derived.*
import skunk.SSL

/**
 * How a service connects to Postgres, for both Skunk and Flyway. No mode checks the server
 * certificate. Only `Prefer` falls back to plaintext when the server refuses TLS.
 */
enum DatabaseSsl(val jdbcSslMode: String, val skunkSsl: SSL) derives Eq:

  /**
   * TLS if the server accepts it, otherwise plaintext. Only for local and test databases, because
   * an attacker on the network can force the plaintext fallback.
   */
  case Prefer extends DatabaseSsl("prefer", SSL.Trusted.withFallback(true))

  /** TLS is required, but the server certificate is not checked. */
  case Require extends DatabaseSsl("require", SSL.Trusted)

object DatabaseSsl:

  /** Only a `Local` environment can connect without TLS. */
  def forEnvironment(environment: Environment): DatabaseSsl =
    if environment.isLocal then Prefer else Require
