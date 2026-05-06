// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import ciris.*
import ciris.http4s.*
import com.comcast.ip4s.*
import lucuma.otel.OtelConfig

final case class ResourceConfiguration(
  database:    DatabaseConfiguration,
  environment: ExecutionEnvironment,
  port:        Port,
  otel:        Option[OtelConfig]
) derives Eq

object ResourceConfiguration:

  lazy val fromCiris: ConfigValue[Effect, ResourceConfiguration] = (
    DatabaseConfiguration.fromCiris,
    environment,
    port,
    otelConfig
  ).parMapN(ResourceConfiguration.apply)

  private val environment = envOrProp[ExecutionEnvironment]("RESOURCE_ENVIRONMENT")
    .default(ExecutionEnvironment.Local)

  private val port = envOrProp[Port]("PORT").default(port"8484")

  private val inHeroku: ConfigValue[Effect, Boolean] =
    envOrProp[String]("DYNO").option.map(_.isDefined)

  private val otelConfig: ConfigValue[Effect, Option[OtelConfig]] =
    // In Heroku, otel configuration is required
    inHeroku.ifM(
      (envOrProp[String]("RESOURCE_OTEL_ENDPOINT"),
       envOrProp[String]("RESOURCE_OTEL_KEY"),
       environment
      ).parMapN((endpoint, key, env) => OtelConfig(endpoint, key, env.stringValue).some),
      (
        envOrProp[String]("RESOURCE_OTEL_ENDPOINT").option,
        envOrProp[String]("RESOURCE_OTEL_KEY").option,
        environment
      ).parTupled.map:
        // Otherwise otel is optional
        case (Some(endpoint), Some(key), env) if endpoint.trim.nonEmpty && key.trim.nonEmpty =>
          OtelConfig(endpoint, key, env.stringValue).some
        case _                                                                               =>
          None
    )
