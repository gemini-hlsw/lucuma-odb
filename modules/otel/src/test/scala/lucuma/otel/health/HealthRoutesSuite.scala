// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import io.circe.Json
import munit.CatsEffectSuite
import org.http4s.HttpRoutes
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import scala.concurrent.duration.*

class HealthRoutesSuite extends CatsEffectSuite:

  private given Logger[IO] = NoOpLogger[IO]

  private val config: HealthRoutes.Config =
    HealthRoutes.Config(checkTimeout = 100.millis, cacheTtl = 200.millis)

  private def routes(checks: List[HealthCheck[IO]]): IO[HttpRoutes[IO]] =
    HealthRoutes[IO]("abc123", checks, config)

  private def get(uri: Uri, checks: List[HealthCheck[IO]]): IO[(Status, Json)] =
    routes(checks).flatMap: r =>
      r.orNotFound.run(Request[IO](Method.GET, uri)).flatMap: res =>
        res.as[Json].map(json => (res.status, json))

  private def status(json: Json): Option[String] =
    json.hcursor.get[String]("status").toOption

  private def check(json: Json, name: String): Option[String] =
    json.hcursor.downField("checks").get[String](name).toOption

  test("liveness is 200 with commit, no checks run"):
    val boom = HealthCheck.required[IO]("db", IO.raiseError(new RuntimeException("down")))
    get(uri"/health", List(boom)).map: (st, json) =>
      assertEquals(st, Status.Ok)
      assertEquals(status(json), "ok".some)
      assertEquals(json.hcursor.get[String]("commit").toOption, "abc123".some)
      assert(json.hcursor.downField("checks").failed)

  test("readiness is 503 when a required check fails"):
    val checks = List(
      HealthCheck.required[IO]("db", IO.raiseError(new RuntimeException("down"))),
      HealthCheck.required[IO]("sso", IO.unit)
    )
    get(uri"/health/ready", checks).map: (st, json) =>
      assertEquals(st, Status.ServiceUnavailable)
      assertEquals(status(json), "fail".some)
      assertEquals(check(json, "db"), "fail".some)
      assertEquals(check(json, "sso"), "ok".some)

  test("info failure is reported but never flips the status"):
    val checks = List(
      HealthCheck.required[IO]("db", IO.unit),
      HealthCheck.info[IO]("s3", IO.raiseError(new RuntimeException("down")))
    )
    get(uri"/health/ready", checks).map: (st, json) =>
      assertEquals(st, Status.Ok)
      assertEquals(status(json), "ok".some)
      assertEquals(check(json, "s3"), "fail".some)

  test("a check that exceeds the timeout counts as failed"):
    val checks = List(HealthCheck.required[IO]("db", IO.sleep(5.seconds)))
    get(uri"/health/ready", checks).map: (st, json) =>
      assertEquals(st, Status.ServiceUnavailable)
      assertEquals(check(json, "db"), "fail".some)

  test("concurrent misses share one computation"):
    Ref[IO].of(0).flatMap: counter =>
      val checks = List(HealthCheck.required[IO]("db", IO.sleep(50.millis) *> counter.update(_ + 1)))
      routes(checks).flatMap: r =>
        val hit = r.orNotFound.run(Request[IO](Method.GET, uri"/health/ready")).flatMap(_.as[Json]).void
        List.fill(5)(hit).parSequence_ *> counter.get.map(assertEquals(_, 1))

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

