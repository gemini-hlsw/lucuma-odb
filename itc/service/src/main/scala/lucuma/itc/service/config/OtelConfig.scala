// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.syntax.all.*
import ciris.*
import lucuma.otel.OtelConfig as SharedOtelConfig

object OtelConfig:

  private val environment: ConfigValue[Effect, String] =
    envOrProp("ODB_ENVIRONMENT").default("local")

  private val inHeroku: ConfigValue[Effect, Boolean] =
    envOrProp("DYNO").option.map(_.isDefined)

  val fromCiris: ConfigValue[Effect, Option[SharedOtelConfig]] =
    inHeroku.flatMap: inHeroku =>
      if inHeroku then
        (
          envOrProp("ODB_OTEL_ENDPOINT"),
          envOrProp("ODB_OTEL_KEY"),
          environment
        ).parMapN: (endpoint, key, env) =>
          SharedOtelConfig(endpoint, key, env).some
      else
        (
          envOrProp("ODB_OTEL_ENDPOINT").option,
          envOrProp("ODB_OTEL_KEY").option,
          environment
        ).parTupled.map:
          case (Some(endpoint), Some(key), env)
              if endpoint.trim.nonEmpty && key.trim.nonEmpty =>
            SharedOtelConfig(endpoint, key, env).some
          case _ =>
            None
