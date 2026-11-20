// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.testing.TestingLogger
import org.typelevel.log4cats.testing.TestingLogger.LogMessage
import org.typelevel.log4cats.testing.TestingLogger.WARN

class SsoClientRequireSuite extends CatsEffectSuite with TestSsoClient:

  private val pi = TestUsers.Standard.pi(301, 302)

  def validUsers: List[User] = List(pi)

  private val Secret: String = "not-a-real-token-1234"

  private val invalid: Authorization =
    Authorization(Credentials.Token(AuthScheme.Bearer, Secret))

  private def logging: IO[(HttpApp[IO], SsoClient[IO, User], IO[Vector[LogMessage]])] =
    IO.delay:
      val log          = TestingLogger.impl[IO]()
      given Logger[IO] = log
      val client       = ssoClient
      val app          = HttpRoutes.of[IO] { case req @ GET -> Root / "ok" => client.require(req)(_ => Ok()) }.orNotFound
      (app, client, log.logged)

  private def warnings(ms: Vector[LogMessage]): Vector[String] =
    ms.collect { case WARN(m, _) => m }

  private def get(auth: Option[Authorization]): Request[IO] =
    Request[IO](Method.GET, uri"/ok", headers = Headers(auth.toList))

  test("Invalid credentials get 403, and a WARN line with the reason and the operation, but not the credentials."):
    logging.flatMap: (app, _, logged) =>
      app.run(get(invalid.some)).flatMap: resp =>
        assertEquals(resp.status, Status.Forbidden)
        logged.map: ms =>
          val ws = warnings(ms)
          assertEquals(ws.length, 1, ms)
          assert(ws.head.contains("Authentication denied"), ws.head)
          assert(ws.head.contains("Bearer"), ws.head)
          assert(ws.head.contains("GET /ok"), ws.head)
          assert(ws.head.contains("Access denied"), ws.head)
          assert(!ms.exists(_.message.contains(Secret)), ms)

  test("A malformed Authorization header gets 403 and a WARN line, but not the header."):
    val raw = Header.Raw(CIString("Authorization"), s"@@@ $Secret")
    logging.flatMap: (app, _, logged) =>
      app.run(Request[IO](Method.GET, uri"/ok", headers = Headers(raw))).flatMap: resp =>
        assertEquals(resp.status, Status.Forbidden)
        logged.map: ms =>
          val ws = warnings(ms)
          assertEquals(ws.length, 1, ms)
          assert(ws.head.contains("malformed"), ws.head)
          assert(!ms.exists(_.message.contains(Secret)), ms)

  test("A request without credentials gets 403, and no WARN line."):
    logging.flatMap: (app, _, logged) =>
      app.run(get(None)).flatMap: resp =>
        assertEquals(resp.status, Status.Forbidden)
        logged.map(ms => assertEquals(warnings(ms), Vector.empty))

  test("A request that succeeds gives no WARN line."):
    logging.flatMap: (app, _, logged) =>
      authorizationHeader(pi).flatMap: h =>
        app.run(get(h.some)).flatMap: resp =>
          assertEquals(resp.status, Status.Ok)
          logged.map(ms => assertEquals(warnings(ms), Vector.empty))

  test("find and get do not log, because the middleware calls them for each request."):
    logging.flatMap: (_, client, logged) =>
      client.find(get(invalid.some)) >> client.get(invalid) >> logged.map(ms => assertEquals(warnings(ms), Vector.empty))
