// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel

import cats.syntax.all.*
import ciris.*

case class OtelConfig(
  endpoint:    String,
  key:         String,
  environment: String
)

object OtelConfig:

  private def envOrProp(name: String): ConfigValue[Effect, String] =
    env(name).or(prop(name))

  private val inHeroku: ConfigValue[Effect, Boolean] =
    envOrProp("DYNO").option.map(_.isDefined)

  /**
   * Loads `<prefix>_ENDPOINT` and `<prefix>_KEY`. Both are required on Heroku; elsewhere
   * telemetry is a silent no-op unless both are set.
   */
  def fromEnv(prefix: String, environment: ConfigValue[Effect, String]): ConfigValue[Effect, Option[OtelConfig]] =
    val endpointKey = s"${prefix}_ENDPOINT"
    val keyKey      = s"${prefix}_KEY"
    inHeroku.flatMap: inHeroku =>
      if inHeroku then
        (envOrProp(endpointKey), envOrProp(keyKey), environment).parMapN: (endpoint, key, env) =>
          OtelConfig(endpoint, key, env).some
      else
        (envOrProp(endpointKey).option, envOrProp(keyKey).option, environment).parTupled.map:
          case (Some(endpoint), Some(key), env) if endpoint.trim.nonEmpty && key.trim.nonEmpty =>
            OtelConfig(endpoint, key, env).some
          case _ =>
            None
