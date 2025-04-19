// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package attachments

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.text
import lucuma.core.model.User
import lucuma.odb.service.AttachmentFileService.AttachmentException
import munit.CatsEffectSuite
import org.http4s.*

abstract class AttachmentRoutesSuite extends CatsEffectSuite with TestSsoClient {
  val pi               = TestUsers.Standard.pi(1, 30)
  val forbiddenUser    = TestUsers.guest(2)
  val fileNotFoundUser = TestUsers.Standard.pi(3, 30)
  val invalidFileUser  = TestUsers.Standard.pi(4, 30)

  val validUsers = List(pi, forbiddenUser, fileNotFoundUser, invalidFileUser)

  protected  def responseStream(s: String): Stream[IO, Byte] =
    Stream[IO, String](s).through(text.utf8.encode)

  protected  val fileContents    = "Phred"
  protected  val notFound        = "Not found".some
  protected  val invalidFileName = "File name must be right"
  protected val presignedUrl     = "http://wherever.com"

  // This isn't really what the errors depend on, but we're not testing the service.
  // Only the result matters.
  protected  def getError(user: User): Option[AttachmentException] =
    if (user === forbiddenUser) AttachmentException.Forbidden.some
    else if (user === fileNotFoundUser) AttachmentException.FileNotFound.some
    else if (user === invalidFileUser) AttachmentException.InvalidRequest(invalidFileName).some
    else none

  protected def headers(user: User): IO[Headers] = authorizationHeader(user).map(Headers(_))

  extension (resp: IO[Response[IO]])
    def assertResponse(expectedStatus: Status, expectedBody: Option[String]): IO[Unit] =
      for {
        actual <- resp
        body   <- actual.as[String]
      } yield {
        val statusCheck = actual.status === expectedStatus
        val bodyCheck   = expectedBody.fold(body.isEmpty)(_ === body)
        assert(statusCheck && bodyCheck,
               s"Expected '$expectedStatus:$expectedBody' Actual: '${actual.status}:$body'"
        )
      }

}
