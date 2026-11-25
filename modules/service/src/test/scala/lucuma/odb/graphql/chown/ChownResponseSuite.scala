// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.chown

import cats.effect.IO
import grackle.Result
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.ChownRoutes
import lucuma.odb.graphql.TestUsers
import munit.CatsEffectSuite
import org.http4s.Status
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.testing.TestingLogger
import org.typelevel.log4cats.testing.TestingLogger.ERROR
import org.typelevel.log4cats.testing.TestingLogger.LogMessage
import org.typelevel.log4cats.testing.TestingLogger.WARN

/** Tests the HTTP response of `/chown` for each kind of result, with no database. */
class ChownResponseSuite extends CatsEffectSuite:

  private val guest    = TestUsers.guest(201)
  private val standard = TestUsers.Standard.pi(202, 203)
  private val service  = TestUsers.service(204)

  private def respond(result: Result[List[Program.Id]]): IO[(Status, String, Vector[LogMessage])] =
    val log          = TestingLogger.impl[IO]()
    given Logger[IO] = log
    for
      res  <- ChownRoutes.response[IO](service, guest, standard, result)
      body <- res.as[String]
      ms   <- log.logged
    yield (res.status, body, ms)

  test("An internal error gives status 500 without the stack trace or the message of the error."):
    val error = new RuntimeException("secret detail of the database")
    respond(Result.internalError(error)).map: (status, body, ms) =>
      assertEquals(status, Status.InternalServerError)
      assert(!body.contains("secret detail"), body)
      assert(!body.contains("ChownResponseSuite"), body)
      assert(!body.contains("\tat ") && !body.contains(".scala:"), body)
      assert(ms.exists { case ERROR(_, Some(t)) => t eq error; case _ => false }, ms)

  test("A NotAuthorized failure gives status 403, and is logged with the user and the operation."):
    respond(OdbError.NotAuthorized(service.id).asFailure).map: (status, _, ms) =>
      assertEquals(status, Status.Forbidden)
      val ws = ms.collect { case WARN(m, _) => m }
      assertEquals(ws.length, 1, ms)
      assert(ws.head.contains(service.id.toString), ws.head)
      assert(ws.head.contains("chown"), ws.head)

  test("A success gives status 200 and logs nothing."):
    respond(Result.success(Nil)).map: (status, _, ms) =>
      assertEquals(status, Status.Ok)
      assertEquals(ms, Vector.empty)
