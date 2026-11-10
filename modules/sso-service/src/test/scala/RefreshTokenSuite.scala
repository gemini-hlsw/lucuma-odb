// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import org.http4s.*
import org.http4s.headers.Cookie

class RefreshTokenSuite extends SsoSuite with Fixture {

  test("Cookie shouldn't expire.") {
    SsoSimulator[IO].use { case (_, _, sso, _, _) =>
      for {
        c  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getCookie(_))
      } yield assertEq(Some(HttpDate.MaxValue), c.expires)
    }
  }

  test("SomeSite should be Strict (simulator is pretending it's using https)") {
    SsoSimulator[IO].use { case (_, _, sso, _, _) =>
      for {
        c  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getCookie(_))
      } yield assertEquals(Some(SameSite.Strict), c.sameSite)
    }
  }

  test("Cookie should be removed on logout.") {
    SsoSimulator[IO].use { case (_, _, sso, _, _) =>
      for {
        _  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest"))
        c  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "logout")).use(CookieReader[IO].getCookie(_))
      } yield assertEq(Some(HttpDate.Epoch), c.expires)
    }
  }

  test("Refresh should fail after logout.") {
    SsoSimulator[IO].use { case (_, _, sso, _, _) =>
      for {
        _  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest"))
        _  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "logout"))
        s  <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "refresh-token"))
      } yield assertEq(Status.Forbidden, s)
    }
  }

  test("Logout should invalidate the session token on the server."):
    SsoSimulator[IO]
      .flatMap { case (pool, _, sso, _, _) => pool.map(db => (db, sso)) }
      .use { case (db, sso) =>
        for {
          tok    <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getSessionToken(_))
          before <- db.findUserFromToken(tok)
          _      <- sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "logout"))
          after  <- db.findUserFromToken(tok)
          // Replay the old cookie, bypassing the client's cookie jar.
          s      <- sso.status {
                      Request[IO](
                        method  = Method.POST,
                        uri     = SsoRoot / "api" / "v1" / "refresh-token",
                        headers = Headers(Cookie(RequestCookie("lucuma-refresh-token", tok.value.toString)))
                      )
                    }
        } yield {
          assert(before.isDefined)
          assertEq(None, after)
          assertEq(Status.Forbidden, s)
        }
      }

  test("Logout without a cookie should still succeed."):
    SsoSimulator[IO].use { case (_, _, sso, _, _) =>
      sso.status(Request[IO](Method.POST, SsoRoot / "api" / "v1" / "logout")).map(assertEq(Status.Ok, _))
    }

  test("Invalid cookie should yield 403.") {
    SsoSimulator[IO].use { case (_, _, sso, _, _) =>
      sso.status {
        Request[IO](
          method  = Method.POST,
          uri     = SsoRoot / "api" / "v1" / "refresh-token",
          headers = Headers(Cookie(RequestCookie("lucuma-refresh-token", "8241D73F-EE0B-44D3-A05F-A15416F039DE")))
        )
      } map { status =>
        assertEq(Status.Forbidden, status)
      }
    }
  }

}
