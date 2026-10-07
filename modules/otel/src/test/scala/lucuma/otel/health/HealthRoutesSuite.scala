// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import io.circe.Json
import munit.CatsEffectSuite
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.circe.*
import org.http4s.implicits.*

import scala.concurrent.duration.*

class HealthRoutesSuite extends CatsEffectSuite:

  private val stats: IO[RuntimeStats] =
    IO.pure(RuntimeStats(workerThreads = 4, activeThreads = 1, blockedWorkerThreads = 0, suspendedFibers = 7, heapUsedPercent = 42))

  private val config: HealthRoutes.Config =
    HealthRoutes.Config(checkTimeout = 100.millis, cacheTtl = 200.millis)

  private def routes(checks: List[HealthCheck[IO]], cfg: HealthRoutes.Config = config) =
    HealthRoutes[IO]("abc123", checks, stats, cfg)

  private def get(path: String, checks: List[HealthCheck[IO]], cfg: HealthRoutes.Config = config): IO[(Status, Json)] =
    routes(checks, cfg).flatMap: r =>
      r.orNotFound.run(Request[IO](Method.GET, uri"/" / "health" / path)).flatMap: res =>
        res.as[Json].map(json => (res.status, json))

  private def getLive(checks: List[HealthCheck[IO]]): IO[(Status, Json)] =
    routes(checks).flatMap: r =>
      r.orNotFound.run(Request[IO](Method.GET, uri"/health")).flatMap: res =>
        res.as[Json].map(json => (res.status, json))

  private def status(json: Json): Option[String] =
    json.hcursor.get[String]("status").toOption

  private def check(json: Json, name: String): Option[String] =
    json.hcursor.downField("checks").get[String](name).toOption

  test("liveness is 200 with commit and runtime stats, no checks run"):
    val boom = HealthCheck.required[IO]("db", IO.raiseError(new RuntimeException("down")))
    getLive(List(boom)).map: (st, json) =>
      assertEquals(st, Status.Ok)
      assertEquals(status(json), "ok".some)
      assertEquals(json.hcursor.get[String]("commit").toOption, "abc123".some)
      assertEquals(json.hcursor.downField("runtime").get[Int]("suspendedFibers").toOption, 7.some)
      assert(json.hcursor.downField("checks").failed)

  test("readiness is 200 when all checks pass"):
    val checks = List(
      HealthCheck.required[IO]("db", IO.unit),
      HealthCheck.informational[IO]("itc", IO.unit)
    )
    get("ready", checks).map: (st, json) =>
      assertEquals(st, Status.Ok)
      assertEquals(status(json), "ok".some)
      assertEquals(check(json, "db"), "ok".some)
      assertEquals(check(json, "itc"), "ok".some)

  test("readiness is 503 when a required check fails"):
    val checks = List(
      HealthCheck.required[IO]("db", IO.raiseError(new RuntimeException("down"))),
      HealthCheck.required[IO]("sso", IO.unit)
    )
    get("ready", checks).map: (st, json) =>
      assertEquals(st, Status.ServiceUnavailable)
      assertEquals(status(json), "fail".some)
      assertEquals(check(json, "db"), "fail".some)
      assertEquals(check(json, "sso"), "ok".some)

  test("informational failure is reported but never flips the status"):
    val checks = List(
      HealthCheck.required[IO]("db", IO.unit),
      HealthCheck.informational[IO]("s3", IO.raiseError(new RuntimeException("down")))
    )
    get("ready", checks).map: (st, json) =>
      assertEquals(st, Status.Ok)
      assertEquals(status(json), "ok".some)
      assertEquals(check(json, "s3"), "fail".some)

  test("a check that exceeds the timeout counts as failed"):
    val checks = List(HealthCheck.required[IO]("db", IO.sleep(5.seconds)))
    get("ready", checks).map: (st, json) =>
      assertEquals(st, Status.ServiceUnavailable)
      assertEquals(check(json, "db"), "fail".some)

  test("readiness result is cached for the configured ttl"):
    Ref[IO].of(0).flatMap: counter =>
      val checks = List(HealthCheck.required[IO]("db", counter.update(_ + 1)))
      routes(checks).flatMap: r =>
        val hit = r.orNotFound.run(Request[IO](Method.GET, uri"/health/ready")).flatMap(_.as[Json]).void
        for
          _ <- hit
          _ <- hit
          n1 <- counter.get
          _ <- IO.sleep(300.millis)
          _ <- hit
          n2 <- counter.get
        yield
          assertEquals(n1, 1)
          assertEquals(n2, 2)

  test("unknown paths fall through"):
    routes(Nil).flatMap: r =>
      r.run(Request[IO](Method.GET, uri"/health/other")).value.map(res => assert(res.isEmpty))
