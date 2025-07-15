// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package scheduler

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.text
import lucuma.core.model.User
import org.http4s.*
import org.http4s.implicits.*

abstract class SchedulerRoutesSuite extends OdbSuite:

  val pi          = TestUsers.Standard.pi(1, 30)
  val serviceUser = TestUsers.service(2)
  val validUsers  = List(pi, serviceUser)

  protected  def responseStream(s: String): Stream[IO, Byte] =
    Stream[IO, String](s).through(text.utf8.encode)

  protected def headers(user: User): IO[Headers] = authorizationHeader(user).map(Headers(_))

  protected def atomsRequest(user: User, oids: String*): IO[Request[IO]] =
    headers(user).map: hs =>
      Request[IO](
        method  = Method.POST,
        uri     = uri"scheduler/atoms",
        headers = hs,
        body    = Stream.emits(oids.toList).intersperse("\n").through(fs2.text.utf8.encode)
      )

  extension (resp: IO[Response[IO]])
    def assertResponse(expectedStatus: Status, expectedBody: Option[String]): IO[Unit] =
      for
        actual <- resp
        body   <- actual.as[String]
      yield
        val statusCheck = actual.status === expectedStatus
        val bodyCheck   = expectedBody.fold(body.isEmpty)(_ === body)
        assert(statusCheck && bodyCheck, s"Expected '$expectedStatus:$expectedBody' Actual: '${actual.status}:$body'")