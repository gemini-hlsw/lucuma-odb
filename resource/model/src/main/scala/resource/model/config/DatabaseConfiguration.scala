// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import ciris.*
import ciris.http4s.*
import com.comcast.ip4s.Port
import org.http4s.*
import org.http4s.Uri.Host

case class DatabaseConfiguration(
  maxConnections: Int,
  host:           Host,
  port:           Port,
  database:       String,
  user:           String,
  password:       String,
  resetDatabase:  Boolean,
  skipMigration:  Boolean
) derives Eq:
  // We use Flyway (which uses JDBC) to perform schema migrations. Savor the irony.
  def jdbcUrl: String = s"jdbc:postgresql://${host}:${port}/${database}?sslmode=require"

object DatabaseConfiguration:
  object Default:
    val MaxConnections = Runtime.getRuntime.availableProcessors * 2 + 1

  def fromDatabaseUrl(
    maxConnections: Int,
    uri:            Uri,
    resetDatabase:  Boolean,
    skipMigration:  Boolean
  ): Option[DatabaseConfiguration] =
    for
      userInfo <- uri.userInfo
      user      = userInfo.username
      password <- userInfo.password
      host     <- uri.host
      port     <- uri.port.flatMap(Port.fromInt)
      database  = uri.path.renderString.stripPrefix("/")
    yield DatabaseConfiguration(
      maxConnections = maxConnections,
      host = host,
      port = port,
      database = database,
      user = user,
      password = password,
      resetDatabase = resetDatabase,
      skipMigration = skipMigration
    )

  lazy val fromCiris: ConfigValue[Effect, DatabaseConfiguration] = (
    envOrProp("MAX_CONNECTIONS").as[Int].default(Default.MaxConnections),
    envOrProp("DATABASE_URL").as[Uri].redacted, // passed by Heroku
    envOrProp("RESET_DATABASE").as[Boolean].default(false),
    envOrProp("SKIP_MIGRATION").as[Boolean].default(false)
  ).parTupled.as[DatabaseConfiguration]

  private given ConfigDecoder[(Int, Uri, Boolean, Boolean), DatabaseConfiguration] =
    ConfigDecoder[(Int, Uri, Boolean, Boolean)].mapOption("DatabaseConfiguration")(fromDatabaseUrl)
