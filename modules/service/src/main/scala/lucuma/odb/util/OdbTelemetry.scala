// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import lucuma.odb.Config
import lucuma.otel.OtelServices
import lucuma.otel.OtelSetup

object OdbTelemetry:
  def tracingBackend(config: Config) = config.otel match
    case Some(cfg) => s"OpenTelemetry (OTLP) endpoint=${cfg.endpoint} environment=${cfg.environment}"
    case None      => "No-op (silent)"

  def otel[F[_]: Async: LiftIO](
    serviceName: String,
    config: Config
  ): Resource[F, OtelServices[F]] =
    OtelSetup.resource(serviceName, config.commitHash.format, config.otel)
