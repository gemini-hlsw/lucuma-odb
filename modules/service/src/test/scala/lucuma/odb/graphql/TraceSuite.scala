// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.effect.Resource
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.data.SpanData
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.trace.TracerProvider

import scala.jdk.CollectionConverters.*

class TraceSuite extends OdbSuite {

  val pi = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  // Provided by otel4 java for testing, not very functional friendly
  var spanExporter: InMemorySpanExporter = null

  override def tracerProvider: Resource[IO, TracerProvider[IO]] =
    Resource.eval:
      IO {
        spanExporter = InMemorySpanExporter.create()
        val sdkProvider = SdkTracerProvider.builder()
          .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
          .build()
        OpenTelemetrySdk.builder().setTracerProvider(sdkProvider).build()
      }.flatMap { sdk =>
        given LocalProvider[IO, Context] = LocalProvider.fromLiftIO[IO, Context]
        OtelJava.fromJOpenTelemetry[IO](sdk).map(_.tracerProvider)
      }

  def clearSpans: IO[Unit] =
    IO(spanExporter.reset())

  def finishedSpans: IO[List[SpanData]] =
    IO(spanExporter.getFinishedSpanItems.asScala.toList)

  test("test tracing (WS)") {
    clearSpans >>
    createProgramAs(pi, clientOption = ClientOption.Ws).replicateA(5) >>
    finishedSpans.map: spans =>
      assert:
        spans.exists(_.getName == "connection.init") &&
        spans.exists(_.getName == "connection.execute")
  }

  test("test tracing (HTTP)") {
    clearSpans >>
    createProgramAs(pi, clientOption = ClientOption.Http).replicateA(5) >>
    finishedSpans.map: all =>
      assert:
        all.exists(_.getName == "POST /odb")
  }

}
