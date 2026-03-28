// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.otel

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
import org.typelevel.otel4s.trace.TracerProvider
import org.typelevel.log4cats.Logger

import scala.jdk.CollectionConverters.*
import java.util.Base64

case class OtelServices[F[_]](
  trace:          Trace[F],
  meterProvider:  MeterProvider[F],
  tracerProvider: TracerProvider[F]
)

case class OtelConfig(
  endpoint:    String,
  instanceId:  String,
  apiKey:      String,
  environment: String
)

object OtelSetup:

  def resource(
    serviceName:    String,
    serviceVersion: String,
    otelConfig:     Option[OtelConfig]
  )(using Logger[IO]): Resource[IO, OtelServices[IO]] =
    otelConfig match
      case Some(cfg) =>
        Resource.eval(Logger[IO].info("Initializing OpenTelemetry tracing backend")) *>
          IOLocal(Context.root).toResource.flatMap: ioLocal =>
            given LocalProvider[IO, Context] = LocalProvider.fromIOLocal[IO, Context](ioLocal)

            val credentials =
              Base64.getEncoder.encodeToString(s"${cfg.instanceId}:${cfg.apiKey}".getBytes)
            val dynoAttr = sys.env.get("DYNO").fold("")(d => s",dyno.id=$d")

            OtelJava.autoConfigured[IO]: builder =>
              builder.addPropertiesSupplier { () =>
                Map(
                  "otel.service.name"           -> serviceName,
                  "otel.service.version"        -> serviceVersion,
                  "otel.resource.attributes"    -> s"deployment.environment.name=${cfg.environment}$dynoAttr",
                  "otel.exporter.otlp.protocol" -> "http/protobuf",
                  "otel.exporter.otlp.endpoint" -> cfg.endpoint,
                  "otel.exporter.otlp.headers"  -> s"Authorization=Basic $credentials"
                ).asJava
              }
            .flatTap: otel =>
              Resource.fromAutoCloseable(
                Sync[IO].delay(RuntimeTelemetry.create(otel.underlying))
              )
            .flatTap: otel =>
              given MeterProvider[IO] = otel.meterProvider
              IORuntimeMetrics.register[IO](IORuntime.global.metrics, IORuntimeMetrics.Config.default)
            .evalMap: otel =>
              otel.tracerProvider.get(serviceName).map: tracer =>
                OtelServices(
                  trace = Otel4sTrace.fromTracer(tracer),
                  meterProvider = otel.meterProvider,
                  tracerProvider = otel.tracerProvider
                )
      case None =>
        Resource.eval(Logger[IO].info("No OpenTelemetry configuration")) *>
          Resource.pure(OtelServices(
            trace = Trace.Implicits.noop,
            meterProvider = MeterProvider.noop,
            tracerProvider = TracerProvider.noop
          ))
