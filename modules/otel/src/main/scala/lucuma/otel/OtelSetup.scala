// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel

import cats.effect.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import io.opentelemetry.instrumentation.runtimetelemetry.RuntimeTelemetry
import natchez.Trace
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.instrumentation.ce.IORuntimeMetrics
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.jdk.CollectionConverters.*

case class OtelServices[F[_]](
  trace:          Trace[F],
  tracer:         Tracer[F],
  meterProvider:  MeterProvider[F],
  tracerProvider: TracerProvider[F]
)

object OtelSetup:

  def resource[F[_]: Async: LiftIO](
    serviceName:    String,
    serviceVersion: String,
    otelConfig:     Option[OtelConfig]
  ): Resource[F, OtelServices[F]] =
    otelConfig match
      case Some(cfg) =>
        given LocalProvider[F, Context] = LocalProvider.fromLiftIO[F, Context]

        OtelJava.autoConfigured[F]: builder =>
          val dynoAttr = sys.env.get("DYNO").fold("")(d => s",dyno.id=$d")

          builder.addPropertiesSupplier: () =>
            Map(
              "otel.service.name"           -> serviceName,
              "otel.resource.attributes"    -> s"service.version=$serviceVersion,deployment.environment.name=${cfg.environment}$dynoAttr",
              "otel.exporter.otlp.protocol" -> "http/protobuf",
              "otel.exporter.otlp.endpoint" -> cfg.endpoint,
              "otel.exporter.otlp.headers"  -> s"Authorization=Basic ${cfg.key}",
              "otel.exporter.otlp.timeout"  -> "30000"
            ).asJava
        .flatTap: otel =>
          Resource.fromAutoCloseable(
            Sync[F].delay(RuntimeTelemetry.create(otel.underlying))
          )
        .flatTap: otel =>
          given MeterProvider[F] = otel.meterProvider
          IORuntimeMetrics.register[F](IORuntime.global.metrics, IORuntimeMetrics.Config.default)
        .evalMap: otel =>
          otel.tracerProvider.get(serviceName).map: tracer =>
            OtelServices(
              trace = Otel4sTrace.fromTracer(tracer),
              tracer = tracer,
              meterProvider = otel.meterProvider,
              tracerProvider = otel.tracerProvider
            )
      case None =>
        Resource.pure(OtelServices(
          trace = Trace.Implicits.noop,
          tracer = Tracer.noop,
          meterProvider = MeterProvider.noop,
          tracerProvider = TracerProvider.noop
        ))
