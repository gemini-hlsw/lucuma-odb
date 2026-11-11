// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel

import cats.effect.IO
import cats.effect.Resource
import fs2.Stream
import io.opentelemetry.api.common.AttributeKey
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.metrics.SdkMeterProvider
import io.opentelemetry.sdk.metrics.data.MetricData
import io.opentelemetry.sdk.testing.exporter.InMemoryMetricReader
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import io.opentelemetry.sdk.trace.data.SpanData
import lucuma.otel.OtelServices.given
import munit.CatsEffectSuite
import org.http4s.Header
import org.http4s.HttpRoutes
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.typelevel.ci.*
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.trace.Tracer

import scala.jdk.CollectionConverters.*

class HttpInstrumentationSuite extends CatsEffectSuite:

  private def routes(using Tracer[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO]:
      case GET -> Root / "ok"     => Tracer[IO].span("inner").surround(Ok("hello"))
      case GET -> Root / "ws"     => Ok("upgrade")
      case GET -> Root / "health" => Ok("ok")
      case GET -> Root / "chunks" => Ok(Stream("a", "b", "c").covary[IO])

  /** What one request through the instrumented app leaves behind. */
  private final case class Recorded(status: Status, spans: List[SpanData], metrics: List[MetricData]):
    def spanNames: List[String] = spans.map(_.getName)

    private def metric(name: String): Option[MetricData] = metrics.find(_.getName == name)

    /** The value of `http.server.active_requests`, or 0 if no request touched it. */
    def activeRequests: Long =
      metric("http.server.active_requests").fold(0L)(_.getLongSumData.getPoints.asScala.map(_.getValue).sum)

    /** The number of requests that `http.server.request.duration` recorded. */
    def durations: Long =
      metric("http.server.request.duration").fold(0L)(_.getHistogramData.getPoints.asScala.map(_.getCount).sum)

  /**
   * Runs `request` through the instrumented app. `consume` plays the server: it reads the response
   * body, or part of it, or none of it.
   */
  private def run(
    request: Request[IO],
    consume: Response[IO] => IO[Unit] = _.body.compile.drain
  ): IO[Recorded] =
    val exporter = InMemorySpanExporter.create()
    val reader   = InMemoryMetricReader.create()
    val sdk      =
      OpenTelemetrySdk
        .builder()
        .setTracerProvider(
          SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(exporter)).build()
        )
        .setMeterProvider(SdkMeterProvider.builder().registerMetricReader(reader).build())
        .build()
    given LocalProvider[IO, Context] = LocalProvider.fromLiftIO[IO, Context]
    Resource
      .make(OtelJava.fromJOpenTelemetry[IO](sdk))(_ => IO(sdk.close()))
      .use: otel =>
        OtelServices.fromOtelJava("test", otel).flatMap: services =>
          given OtelServices[IO] = services
          for
            instrument <- HttpInstrumentation.server[IO]()
            response   <- instrument(routes.orNotFound).run(request)
            _          <- consume(response)
          yield Recorded(
            response.status,
            exporter.getFinishedSpanItems.asScala.toList,
            reader.collectAllMetrics().asScala.toList
          )

  test("a request ends its server span when the body finishes, with app spans as children"):
    run(Request[IO](Method.GET, uri"/ok")).map: r =>
      assertEquals(r.status, Status.Ok)
      val server = r.spans.find(_.getName == "GET")
      val inner  = r.spans.find(_.getName == "inner")
      assert(server.isDefined, r.spanNames)
      assertEquals(inner.map(_.getParentSpanId), server.map(_.getSpanId))

  test("a request records its duration and leaves no active request"):
    run(Request[IO](Method.GET, uri"/ok")).map: r =>
      assertEquals(r.durations, 1L)
      assertEquals(r.activeRequests, 0L)

  test("a body that the client abandons part way still ends the exchange"):
    run(Request[IO](Method.GET, uri"/chunks"), _.body.take(1).compile.drain).map: r =>
      assertEquals(r.spanNames, List("GET"))
      assertEquals(r.durations, 1L)
      assertEquals(r.activeRequests, 0L)

  test("a body that never runs leaves the exchange open"):
    // Servers must run the body, also for HEAD. Blaze drains it and Ember writes it.
    run(Request[IO](Method.GET, uri"/ok"), _ => IO.unit).map: r =>
      assertEquals(r.spanNames, List("inner"))
      assertEquals(r.activeRequests, 1L)

  test("a WebSocket upgrade request records no span and no metrics"):
    run(Request[IO](Method.GET, uri"/ws").putHeaders(Header.Raw(ci"Upgrade", "websocket"))).map: r =>
      assertEquals(r.status, Status.Ok)
      assertEquals(r.spanNames, Nil)
      assertEquals(r.durations, 0L)
      assertEquals(r.activeRequests, 0L)

  test("an unknown route records a span for its 404"):
    run(Request[IO](Method.GET, uri"/unknown")).map: r =>
      assertEquals(r.status, Status.NotFound)
      assertEquals(r.spanNames, List("GET"))

  test("a health check records no span and no metrics"):
    run(Request[IO](Method.GET, uri"/health")).map: r =>
      assertEquals(r.status, Status.Ok)
      assertEquals(r.spanNames, Nil)
      assertEquals(r.durations, 0L)
      assertEquals(r.activeRequests, 0L)

  test("the span masks the values of sensitive query parameters and keeps the others"):
    run(Request[IO](Method.GET, uri"/ok?key=secret&code=abc&query=q&programId=p-1")).map: r =>
      val query = r.spans.find(_.getName == "GET").map(_.getAttributes.get(AttributeKey.stringKey("url.query")))
      assertEquals(query, Some("key=REDACTED&code=REDACTED&query=REDACTED&programId=p-1"))
