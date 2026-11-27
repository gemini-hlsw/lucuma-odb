// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import ciris.*
import ciris.http4s.*
import com.comcast.ip4s.*
import lucuma.common.config.Environment
import lucuma.common.config.envOrProp
import lucuma.otel.OtelConfig

final case class ResourceConfiguration(
  database:    DatabaseConfiguration,
  environment: Environment,
  port:        Port,
  domain:      Seq[String], // Domains, for CORS headers
  otel:        Option[OtelConfig],
  sso:         SsoConfiguration
) derives Eq

object ResourceConfiguration:

  lazy val fromCiris: ConfigValue[Effect, ResourceConfiguration] = (
    DatabaseConfiguration.fromCiris(environment),
    environment,
    port,
    domain,
    OtelConfig.fromEnv("RESOURCE_OTEL", environment.map(_.name)),
    SsoConfiguration.fromCiris
  ).parMapN(ResourceConfiguration.apply)

  private lazy val environment = Environment.fromCiris("RESOURCE_ENVIRONMENT")

  private lazy val port = envOrProp("PORT").as[Port].default(port"8484")

  private lazy val domain = envOrProp("RESOURCE_DOMAIN").map(_.split(",").map(_.trim).toSeq)
