// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.Parallel
import cats.effect.Async
import cats.effect.Deferred
import cats.effect.Ref
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.Status
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/**
 * Unauthenticated health routes shared by every HTTP service.
 *
 *  - `GET /health` is liveness: 200 whenever the server can run a fiber.
 *  - `GET /health/ready` runs the dependency checks and answers 503 when a
 *    `Required` one fails or times out.
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

  def apply[F[_]: Async: Parallel: Logger](
    commit: String,
    checks: List[HealthCheck[F]],
    config: Config = Config()
  ): F[HttpRoutes[F]] =
    Ref[F].of(Option.empty[(FiniteDuration, Deferred[F, Readiness])]).map: cache =>
      val dsl = Http4sDsl[F]
      import dsl.*

      def base(status: Outcome): Json =
        Json.obj(
          "status" -> status.json,
          "commit" -> Json.fromString(commit)
        )

      def runCheck(c: HealthCheck[F]): F[(HealthCheck[F], Outcome)] =
        c.run
          .timeout(config.checkTimeout)
          .as(Outcome.Ok)
          .handleErrorWith: e =>
            Logger[F].warn(e)(s"health check '${c.name}' failed").as(Outcome.Fail)
          .tupleLeft(c)

      val computeReadiness: F[Readiness] =
        checks.parTraverse(runCheck).flatMap: results =>
          val requiredFailed = results.exists: (c, o) =>
            c.importance == Importance.Required && o == Outcome.Fail
          val status         = if requiredFailed then Outcome.Fail else Outcome.Ok
          val detail         = Json.obj(results.map((c, o) => c.name -> o.json)*)
          Readiness(
            if requiredFailed then Status.ServiceUnavailable else Status.Ok,
            base(status).deepMerge(Json.obj("checks" -> detail))
          ).pure[F]

      // Single flight: the first miss installs a Deferred and computes, later callers wait on it.
      val readiness: F[Readiness] =
        Async[F].monotonic.flatMap: now =>
          Deferred[F, Readiness].flatMap: fresh =>
            cache.modify:
              case current @ Some((at, d)) if now - at < config.cacheTtl => (current, d.get)
              case _                                                     =>
                ((now, fresh).some, computeReadiness.flatTap(fresh.complete).onError(_ => cache.set(None)))
            .flatten

      HttpRoutes.of[F]:
        case GET -> Root / "health"           =>
          Ok(base(Outcome.Ok))
        case GET -> Root / "health" / "ready" =>
          readiness.map(r => Response[F](r.status).withEntity(r.body))
