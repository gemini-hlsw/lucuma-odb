// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.config

import cats.Eq
import cats.derived.*
import cats.effect.Temporal
import cats.effect.std.Console
import ciris.*
import ciris.http4s.*
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import fs2.io.net.Network
import org.http4s.Uri
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import skunk.Session
import skunk.TypingStrategy

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** Where and how a service connects to its Postgres database. */
final case class DatabaseConnection(
  host:     Host,
  port:     Port,
  database: String,
  user:     String,
  password: Secret[String],
  ssl:      DatabaseSsl
) derives Eq:

  // We use Flyway (which uses JDBC) to perform schema migrations. Savor the irony.
  def jdbcUrl: String =
    // pgjdbc decodes the database name with URLDecoder, so encode it with the matching URLEncoder.
    val jdbcDatabase = URLEncoder.encode(database, StandardCharsets.UTF_8)
    s"jdbc:postgresql://$host:$port/$jdbcDatabase?sslmode=${ssl.jdbcSslMode}"

  /**
   * A Skunk session builder for this database. Build every session from it, because the default of
   * Skunk is plaintext. `applicationName` tags the connections in `pg_stat_activity`.
   */
  def sessionBuilder[F[_]: Temporal: Network: Console: TracerProvider: MeterProvider](
    applicationName: String
  ): Session.Builder[F] =
    Session.Builder[F]
      .withHost(host)
      .withPort(port)
      .withUserAndPassword(user, password.value)
      .withDatabase(database)
      .withSSL(ssl.skunkSsl)
      .withTypingStrategy(TypingStrategy.SearchPath)
      // .withDebug(true)
      .withConnectionParameters(
        Session.DefaultConnectionParameters + ("application_name" -> applicationName)
      )

object DatabaseConnection:

  /** Parses a URL of the form `postgres://user:password@host:port/database`. */
  def fromUri(uri: Uri, ssl: DatabaseSsl): Option[DatabaseConnection] =
    for
      userInfo <- uri.userInfo
      user     =  userInfo.username
      password <- userInfo.password
      host     <- uri.host.flatMap(h => Host.fromString(h.value))
      port     <- uri.port.flatMap(Port.fromInt)
      database =  uri.path.segments.map(_.decoded()).mkString("/")
    yield DatabaseConnection(
      host     = host,
      port     = port,
      database = database,
      user     = user,
      password = Secret(password),
      ssl      = ssl
    )

  /**
   * Loads `DATABASE_URL`, which Heroku sets. The TLS mode comes from `environment`. The value stays
   * redacted, so a decoding error does not show the password.
   */
  def fromCiris(environment: Environment): ConfigValue[Effect, DatabaseConnection] =
    val ssl = DatabaseSsl.forEnvironment(environment)
    given ConfigDecoder[Uri, DatabaseConnection] =
      ConfigDecoder[Uri].mapOption("DatabaseConnection")(fromUri(_, ssl))
    envOrProp("DATABASE_URL").redacted
      .as[Uri]
      .as[DatabaseConnection]
