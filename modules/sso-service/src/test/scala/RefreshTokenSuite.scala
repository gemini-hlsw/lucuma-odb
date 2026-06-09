// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import lucuma.sso.service.simulator.SsoSimulator
import org.http4s.*
import org.http4s.headers.Cookie

object RefreshTokenSuite extends SsoSuite with Fixture with FlakyTests {

  test("Cookie shouldn't expire.") {
    flaky()(
      SsoSimulator[IO].use { case (_, _, sso, _, _) =>
        for {
          c  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getCookie(_))
        } yield expect.same(Some(HttpDate.MaxValue), c.expires)
      }
    )
  }

  test("SomeSite should be Strict (simulator is pretending it's using https)") {
    flaky()(
      SsoSimulator[IO].use { case (_, _, sso, _, _) =>
        for {
          c  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getCookie(_))
        } yield expect.same(Some(SameSite.Strict), c.sameSite)
      }
    )
  }

  test("Cookie should be removed on logout.") {
    flaky()(
      SsoSimulator[IO].use { case (_, _, sso, _, _) =>
        for {
          _  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest"))
          c  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "logout")).use(CookieReader[IO].getCookie(_))
        } yield expect.same(Some(HttpDate.Epoch), c.expires)
      }
    )
  }

  test("Refresh should fail after logout.") {
    flaky()(
      SsoSimulator[IO].use { case (_, _, sso, _, _) =>
        for {
          _  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest"))
          _  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "logout"))
          s  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "refresh-token"))
        } yield expect.same(Status.Forbidden, s)
      }
    )
  }

  test("Invalid cookie should yield 403.") {
    flaky()(
      SsoSimulator[IO].use { case (_, _, sso, _, _) =>
        sso.status {
          Request[IO](
            method  = Method.POST,
            uri     = SsoRoot / "api" / "v1" / "refresh-token",
            headers = Headers(Cookie(RequestCookie("lucuma-refresh-token", "8241D73F-EE0B-44D3-A05F-A15416F039DE")))
          )
        } map { status =>
          expect.same(Status.Forbidden, status)
        }
      }
    )
  }

}




