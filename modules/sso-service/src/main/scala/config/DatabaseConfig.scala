// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.config

import cats.Show
import ciris.*

import java.net.URI

case class DatabaseConfig(
  host: String,
  port: Int,
  database: String,
  user: String,
  password: Option[String]
) {

  // We use Flyway (which uses JDBC) to perform schema migrations. Savor the irony.
  def jdbcUrl: String =
    s"jdbc:postgresql://${host}:${port}/${database}?sslmode=require"

}

object DatabaseConfig {

  val Local  = DatabaseConfig(
    host     = "localhost",
    port     = 5432,
    database = "lucuma-sso",
    user     = "jimmy",
    password = Some("banana"),
  )

  // postgres://username:password@host:port/database name
  def fromHerokuUri(uri: URI): Option[DatabaseConfig] =
    uri.getUserInfo.split(":") match {
      case Array(user, password) =>
        Some(DatabaseConfig(
          host     = uri.getHost,
          port     = uri.getPort,
          database = uri.getPath.drop(1),
          user     = user,
          password = Some(password),
        ))
      case _ => None
    }

  private implicit val ShowURI: Show[URI] =
    Show.fromToString

  private implicit val ConfigDecoderDatabaseConfig: ConfigDecoder[URI, DatabaseConfig] =
    ConfigDecoder[URI].mapOption("DatabaseConfig")(fromHerokuUri)

  val config: ConfigValue[Effect, DatabaseConfig] =
    envOrProp("DATABASE_URL").as[URI].as[DatabaseConfig]


}