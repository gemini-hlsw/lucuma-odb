// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import cats.syntax.all.*
import lucuma.odb.Config
import natchez.Trace
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.trace.TracerProvider
import io.opentelemetry.instrumentation.runtimetelemetry.RuntimeTelemetry
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

import scala.jdk.CollectionConverters.*
import java.util.Base64

case class OtelServices[F[_]](
  trace:          Trace[F],
  meterProvider:  MeterProvider[F],
  tracerProvider: TracerProvider[F]
)

object LucumaEntryPoint:
  def tracingBackend(config: Config) = config.otel match
    case Some(_) => "OpenTelemetry (OTLP)"
    case None    => "No-op (silent)"

  def otelServicesResource(
    serviceName: String,
    config: Config
  )(using Logger[IO]): Resource[IO, OtelServices[IO]] =
    config.otel match
      case Some(otelConfig) =>
        Resource.eval(info"Initializing OpenTelemetry tracing backend") *>
          IOLocal(Context.root).toResource.flatMap: ioLocal =>
            given LocalProvider[IO, Context] = LocalProvider.fromIOLocal[IO, Context](ioLocal)

            OtelJava.autoConfigured[IO]: builder =>
              val credentials =
                Base64.getEncoder.encodeToString(s"${otelConfig.instanceId}:${otelConfig.apiKey}".getBytes)

              builder.addPropertiesSupplier { () =>
                Map(
                  "otel.service.name"           -> serviceName,
                  "otel.service.version"        -> config.commitHash.format,
                  "otel.resource.attributes"    -> s"deployment.environment.name=${otelConfig.environment}",
                  "otel.exporter.otlp.protocol" -> "http/protobuf",
                  "otel.exporter.otlp.endpoint" -> otelConfig.endpoint,
                  "otel.exporter.otlp.headers"  -> s"Authorization=Basic $credentials"
                ).asJava
              }
            .flatTap: otel =>
              Resource.fromAutoCloseable(
                Sync[IO].delay(RuntimeTelemetry.create(otel.underlying))
              )
            .evalMap: otel =>
              otel.tracerProvider.get(serviceName).map: tracer =>
                OtelServices(
                  trace = Otel4sTrace.fromTracer(tracer),
                  meterProvider = otel.meterProvider,
                  tracerProvider = otel.tracerProvider
                )
      case None =>
        Resource.eval(info"No OpenTelemetry configuration") *>
          Resource.pure(OtelServices(
            trace = Trace.Implicits.noop,
            meterProvider = MeterProvider.noop,
            tracerProvider = TracerProvider.noop
          ))
