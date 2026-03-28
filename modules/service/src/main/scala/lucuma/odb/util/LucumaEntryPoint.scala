// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import lucuma.odb.Config
import lucuma.odb.otel.OtelConfig
import lucuma.odb.otel.OtelServices
import lucuma.odb.otel.OtelSetup
import org.typelevel.log4cats.Logger

object LucumaEntryPoint:
  def tracingBackend(config: Config) = config.otel match
    case Some(_) => "OpenTelemetry (OTLP)"
    case None    => "No-op (silent)"

  def otelServicesResource(
    serviceName: String,
    config: Config
  )(using Logger[IO]): Resource[IO, OtelServices[IO]] =
    OtelSetup.resource(
      serviceName,
      config.commitHash.format,
      config.otel.map(c => OtelConfig(c.endpoint, c.instanceId, c.apiKey, c.environment))
    )
