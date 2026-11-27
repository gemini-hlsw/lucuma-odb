// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import ciris.*
import lucuma.common.config.DatabaseConnection
import lucuma.common.config.Environment
import lucuma.common.config.envOrProp

case class DatabaseConfiguration(
  connection:     DatabaseConnection,
  maxConnections: Int,
  resetDatabase:  Boolean,
  skipMigration:  Boolean
) derives Eq

object DatabaseConfiguration:
  object Default:
    val MaxConnections = Runtime.getRuntime.availableProcessors * 2 + 1

  def fromCiris(
    environment: ConfigValue[Effect, Environment]
  ): ConfigValue[Effect, DatabaseConfiguration] = (
    environment.flatMap(DatabaseConnection.fromCiris),
    envOrProp("MAX_CONNECTIONS").as[Int].default(Default.MaxConnections),
    envOrProp("RESET_DATABASE").as[Boolean].default(false),
    envOrProp("SKIP_MIGRATION").as[Boolean].default(false)
  ).parMapN(DatabaseConfiguration.apply)
