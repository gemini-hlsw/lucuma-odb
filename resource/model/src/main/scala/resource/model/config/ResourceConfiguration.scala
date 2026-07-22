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
  database:      DatabaseConfiguration,
  environment:   ExecutionEnvironment,
  port:          Port,
  corsOverHttps: Boolean,     // Whether to require CORS over HTTPS
  domain:        Seq[String], // Domains, for CORS headers
  otel:          Option[OtelConfig],
  sso:           SsoConfiguration
) derives Eq

object ResourceConfiguration:

  lazy val fromCiris: ConfigValue[Effect, ResourceConfiguration] = (
    DatabaseConfiguration.fromCiris,
    environment,
    port,
    corsOverHttps,
    domain,
    otelConfig,
    SsoConfiguration.fromCiris
  ).parMapN(ResourceConfiguration.apply)

  private lazy val environment =
    val ee = envOrProp("RESOURCE_ENVIRONMENT").as[ExecutionEnvironment]
    inHeroku.ifM(
      ee,
      ee.default(ExecutionEnvironment.Local)
    )

  private lazy val port = envOrProp("PORT").as[Port].default(port"8484")

  private lazy val inHeroku: ConfigValue[Effect, Boolean] =
    envOrProp("DYNO").option.map(_.isDefined)

  private lazy val otelConfig: ConfigValue[Effect, Option[OtelConfig]] =
    val otelEndpoint = envOrProp("RESOURCE_OTEL_ENDPOINT")
    val otelKey      = envOrProp("RESOURCE_OTEL_KEY").redacted
    // In Heroku, otel configuration is required
    inHeroku.ifM(
      (otelEndpoint, otelKey, environment).parMapN((endpoint, key, env) =>
        OtelConfig(endpoint, key, env.stringValue).some
      ),
      (otelEndpoint.option, otelKey.option, environment).parTupled.map:
        // Otherwise otel is optional
        case (Some(endpoint), Some(key), env) if endpoint.trim.nonEmpty && key.trim.nonEmpty =>
          OtelConfig(endpoint, key, env.stringValue).some
        case _                                                                               =>
          None
    )

  private lazy val corsOverHttps = envOrProp("CORS_OVER_HTTPS").as[Boolean].default(true)

  private lazy val domain = envOrProp("RESOURCE_DOMAIN").map(_.split(",").map(_.trim).toSeq)

def envOrProp(name: String): ConfigValue[Effect, String] =
  env(name).or(prop(name))
