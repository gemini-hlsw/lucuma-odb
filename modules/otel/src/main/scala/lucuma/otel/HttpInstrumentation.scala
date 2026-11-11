// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel

import cats.Endo
import cats.effect.Temporal
import cats.syntax.all.*
import lucuma.otel.health.HealthRoutes
import org.http4s.HttpApp
import org.http4s.client.Client
import org.http4s.headers.Upgrade
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.typelevel.ci.*
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider

/**
 * HTTP server and client metrics and tracing, shared by all services. Spans mask the values of
 * sensitive query parameters (case-sensitive match).
 */
object HttpInstrumentation:

  private val SensitiveQueryParameters: Set[String] =
    QueryRedactor.DefaultSensitiveQueryParameters ++
      Set("key", "code", "access_token", "client_secret", "state", "token", "apiKey", "api_key", "password", "jwt", "query", "variables")

  private val redactor: UriRedactor =
    new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive:
      override def sensitiveQueryParameters: Set[String] = SensitiveQueryParameters

  /**
   * Skips WebSocket upgrades and health checks. Ember and Blaze answer the WebSocket handshake
   * themselves, so the exchange never finishes: its span stays open and the active request count
   * never drops. Health checks are frequent probes with no value as telemetry.
   */
  private val serverRequestFilter: PerRequestFilter = request =>
    val webSocket = request.headers.get[Upgrade].exists(_.values.exists(_.name === ci"websocket"))
    if webSocket || HealthRoutes.Paths.contains(request.uri.path) then PerRequestConfig.Disabled
    else PerRequestConfig.Enabled

  /**
   * Records HTTP server metrics and spans, except for WebSocket upgrades and health checks. Apply
   * it once, to the final app after `orNotFound`, as spans end with the response body.
   */
  def server[F[_]: {Temporal, MeterProvider, TracerProvider, ContextManager}](
    routeClassifier: RouteClassifier = RouteClassifier.indeterminate,
    recordHeaders:   Boolean = false
  ): F[Endo[HttpApp[F]]] =
    val spans            = ServerSpanDataProvider.openTelemetry(redactor).withRouteClassifier(routeClassifier)
    val spanDataProvider =
      if recordHeaders then
        spans
          .optIntoHttpRequestHeaders(HeaderRedactor.default)
          .optIntoHttpResponseHeaders(HeaderRedactor.default)
      else spans
    ServerInstrumentation
      .middleware[F](
        ServerMetricsConfig.default
          .withRouteClassifier(routeClassifier)
          .withRequestFilter(serverRequestFilter)
          .optIntoActiveRequests
          .optIntoRequestBodySize
          .optIntoResponseBodySize,
        ServerTracingConfig(spanDataProvider)
          .withRequestFilter(serverRequestFilter)
      )

  /** Records HTTP client metrics and spans. */
  def client[F[_]: {Temporal, MeterProvider, TracerProvider, ContextManager}]: F[Endo[Client[F]]] =
    ClientInstrumentation.middleware[F](
      ClientMetricsConfig.default
        .optIntoActiveRequests
        .optIntoRequestBodySize
        .optIntoResponseBodySize,
      ClientTracingConfig(ClientSpanDataProvider.openTelemetry(redactor))
    )
