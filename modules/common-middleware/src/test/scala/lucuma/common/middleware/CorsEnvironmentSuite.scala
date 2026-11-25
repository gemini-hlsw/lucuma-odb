// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.effect.IO
import lucuma.common.config.Environment
import munit.CatsEffectSuite
import org.http4s.*
import org.typelevel.ci.*

class CorsEnvironmentSuite extends CatsEffectSuite:

  private val routes: HttpRoutes[IO] =
    HttpRoutes.of[IO] { case _ => IO.pure(Response[IO](Status.Ok)) }

  /** True if the CORS middleware for `environment` allows a request from `origin`. */
  private def allowsOrigin(environment: Environment, origin: String): IO[Boolean] =
    val app = CorsMiddleware.cors[IO](environment, List("gemini.edu"))(routes).orNotFound
    val req = Request[IO](uri = Uri.unsafeFromString("/")).putHeaders(Header.Raw(ci"Origin", origin))
    app(req).map(_.headers.get(ci"Access-Control-Allow-Origin").isDefined)

  test("Local allows plain HTTP and HTTPS origins"):
    assertIO(allowsOrigin(Environment.Local, "http://www.gemini.edu"), true) *>
      assertIO(allowsOrigin(Environment.Local, "https://www.gemini.edu"), true)

  List(Environment.Dev, Environment.Staging, Environment.Production).foreach: env =>
    test(s"$env refuses plain HTTP origins"):
      assertIO(allowsOrigin(env, "http://www.gemini.edu"), false)

    test(s"$env allows HTTPS origins"):
      assertIO(allowsOrigin(env, "https://www.gemini.edu"), true)

  Environment.values.foreach: env =>
    test(s"$env refuses origins outside the domain"):
      assertIO(allowsOrigin(env, "https://evil.com"), false)
