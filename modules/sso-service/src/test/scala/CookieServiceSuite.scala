// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.Response

import java.util.UUID

class CookieServiceSuite extends CatsEffectSuite {

  val token = SessionToken(UUID.randomUUID())

  List(true, false).foreach { secure =>

    val cookies = CookieService[IO]("gemini.edu", secure)

    test(s"session cookie is HttpOnly (secure = $secure)") {
      cookies.sessionCookie(token).map { c =>
        assert(c.httpOnly, "session cookie must be HttpOnly")
        assertEquals(c.secure, secure)
      }
    }

    test(s"removal cookie is HttpOnly (secure = $secure)") {
      for {
        res <- cookies.removeCookie(Response[IO]())
        c   <- cookies.getCookie(res)
      } yield {
        assert(c.httpOnly, "removal cookie must be HttpOnly")
        assertEquals(c.secure, secure)
      }
    }

  }

}
