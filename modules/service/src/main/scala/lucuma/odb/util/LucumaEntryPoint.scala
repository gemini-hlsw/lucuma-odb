// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import cats.syntax.all.*
import io.opentelemetry.exporter.otlp.http.trace.OtlpHttpSpanExporter
import io.opentelemetry.sdk.resources.Resource as OTelResource
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.BatchSpanProcessor
import io.opentelemetry.api.common.AttributeKey
import lucuma.odb.Config
import natchez.EntryPoint
import natchez.noop.NoopEntrypoint
import natchez.opentelemetry.OpenTelemetry
import org.typelevel.log4cats.Logger

import java.util.Base64

object LucumaEntryPoint:
  def tracingBackend(config: Config) = config.otel match
    case Some(_) => "OpenTelemetry (OTLP)"
    case None    => "No-op (silent)"

  def entryPointResource[F[_]: Sync: Logger](
    serviceName: String,
    config: Config
  ): Resource[F, EntryPoint[F]] =
    config.otel match
      case Some(otelConfig) =>
        Resource.eval(Logger[F].info("Initializing OpenTelemetry tracing backend")) *>
          OpenTelemetry.entryPoint[F]() { sdkBuilder =>
            Resource.make(
              Sync[F].delay {
                val credentials =
                  Base64.getEncoder.encodeToString(
                    s"${otelConfig.instanceId}:${otelConfig.apiKey}".getBytes
                  )
                val endpoint =
                  val base = otelConfig.endpoint.stripSuffix("/")
                  if base.endsWith("/v1/traces") then base
                  else s"$base/v1/traces"
                val exporter = OtlpHttpSpanExporter.builder()
                  .setEndpoint(endpoint)
                  .addHeader("Authorization", s"Basic $credentials")
                  .build()
                val processor = BatchSpanProcessor.builder(exporter).build()
                val resource = OTelResource.builder()
                  .put(AttributeKey.stringKey("service.name"), serviceName)
                  .build()
                val tracerProvider = SdkTracerProvider.builder()
                  .setResource(OTelResource.getDefault.merge(resource))
                  .addSpanProcessor(processor)
                  .build()
                sdkBuilder.setTracerProvider(tracerProvider)
              }
            )(_ => Sync[F].unit)
          }
      case None =>
        Resource.eval(Logger[F].info("No OpenTelemetry configuration")) *>
          Resource.pure(NoopEntrypoint())
