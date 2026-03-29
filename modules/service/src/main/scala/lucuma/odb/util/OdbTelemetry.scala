// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import lucuma.odb.Config
import lucuma.odb.otel.OtelConfig
import lucuma.odb.otel.OtelServices
import lucuma.odb.otel.OtelSetup
import org.typelevel.log4cats.Logger

object OdbTelemetry:
  def tracingBackend(config: Config) = config.otel match
    case Some(_) => "OpenTelemetry (OTLP)"
    case None    => "No-op (silent)"

  def otel[F[_]: Async: Logger: LiftIO](
    serviceName: String,
    config: Config
  ): Resource[F, OtelServices[F]] =
    OtelSetup.resource(
      serviceName,
      config.commitHash.format,
      config.otel.map(c => OtelConfig(c.endpoint, c.instanceId, c.apiKey, c.environment))
    )
