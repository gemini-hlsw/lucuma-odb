// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.syntax.all.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.Status
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl

import scala.concurrent.duration.*

/**
 * Unauthenticated health routes shared by every HTTP service.
 *
 *  - `GET /health` is liveness: 200 whenever the server can run a fiber.
 *  - `GET /health/ready` runs the dependency checks and answers 503 when a
 *    `Required` one fails or times out. Results are cached for `cacheTtl` so a
 *    burst of polls does not multiply load on the dependencies.
 *
 * Mount these outside the auth, CORS, tracing and logging middleware.
 */
object HealthRoutes:

  final case class Config(
    checkTimeout: FiniteDuration = 2.seconds,
    cacheTtl:     FiniteDuration = 10.seconds
  )

  private enum Outcome derives CanEqual:
    case Ok, Fail

    def json: Json = this match
      case Ok   => Json.fromString("ok")
      case Fail => Json.fromString("fail")

  private final case class Readiness(status: Status, body: Json)

  def apply[F[_]: Async: Parallel](
    commit: String,
    checks: List[HealthCheck[F]],
    config: Config = Config()
  ): F[HttpRoutes[F]] =
    apply(commit, checks, RuntimeStats.current[F](IORuntime.global), config)

  def apply[F[_]: Async: Parallel](
    commit: String,
    checks: List[HealthCheck[F]],
    stats:  F[RuntimeStats],
    config: Config
  ): F[HttpRoutes[F]] =
    Ref[F].of(Option.empty[(FiniteDuration, Readiness)]).map: cache =>
      val dsl = Http4sDsl[F]
      import dsl.*

      def base(status: Outcome): F[Json] =
        stats.map: s =>
          Json.obj(
            "status"  -> status.json,
            "commit"  -> Json.fromString(commit),
            "runtime" -> s.asJson
          )

      def runCheck(c: HealthCheck[F]): F[(HealthCheck[F], Outcome)] =
        c.run
          .timeout(config.checkTimeout)
          .as(Outcome.Ok)
          .handleError(_ => Outcome.Fail)
          .tupleLeft(c)

      val computeReadiness: F[Readiness] =
        checks.parTraverse(runCheck).flatMap: results =>
          val requiredFailed = results.exists: (c, o) =>
            c.importance == Importance.Required && o == Outcome.Fail
          val status         = if requiredFailed then Outcome.Fail else Outcome.Ok
          base(status).map: json =>
            val detail = Json.obj(results.map((c, o) => c.name -> o.json)*)
            Readiness(
              if requiredFailed then Status.ServiceUnavailable else Status.Ok,
              json.deepMerge(Json.obj("checks" -> detail))
            )

      val readiness: F[Readiness] =
        Async[F].monotonic.flatMap: now =>
          cache.get.flatMap:
            case Some((at, r)) if now - at < config.cacheTtl => r.pure[F]
            case _                                           =>
              computeReadiness.flatTap(r => cache.set((now, r).some))

      HttpRoutes.of[F]:
        case GET -> Root / "health"           =>
          base(Outcome.Ok).flatMap(Ok(_))
        case GET -> Root / "health" / "ready" =>
          readiness.map(r => Response[F](r.status).withEntity(r.body))
