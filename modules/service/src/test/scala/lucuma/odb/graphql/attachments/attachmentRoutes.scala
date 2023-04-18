// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.text
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.service.AttachmentService
import lucuma.odb.service.AttachmentService.AttachmentException
import lucuma.refined.*
import munit.CatsEffectSuite
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.implicits.*

class attachmentRoutes extends CatsEffectSuite with TestSsoClient {
  val pi               = TestUsers.Standard.pi(1, 30)
  val forbiddenUser    = TestUsers.guest(2)
  val fileNotFoundUser = TestUsers.Standard.pi(3, 30)
  val invalidFileUser  = TestUsers.Standard.pi(4, 30)

  val validUsers = List(pi, forbiddenUser, fileNotFoundUser, invalidFileUser)

  private def responseStream(s: String): Stream[IO, Byte] =
    Stream[IO, String](s).through(text.utf8.encode)

  private val fileContents    = "Phred"
  private val attachmentId    = ObsAttachment.Id(5.refined)
  private val notFound        = "Not found".some
  private val invalidFileName = "File name must be right"

  // This isn't really what the errors depend on, but we're not testing the service.
  // Only the result matters.
  private def getError(user: User): Option[AttachmentException] =
    if (user === forbiddenUser) AttachmentException.Forbidden.some
    else if (user === fileNotFoundUser) AttachmentException.FileNotFound.some
    else if (user === invalidFileUser) AttachmentException.InvalidRequest(invalidFileName).some
    else none

  private val service: AttachmentService[IO] = new AttachmentService[IO] {
    def getAttachment(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): IO[Either[AttachmentException, Stream[IO, Byte]]] = {
      val either = getError(user).fold(responseStream(fileContents).asRight)(_.asLeft)
      IO(either)
    }

    def uploadAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       String,
      description:    Option[NonEmptyString],
      data:           Stream[IO, Byte]
    ): IO[ObsAttachment.Id] =
      getError(user).fold(IO(attachmentId))(IO.raiseError)

    def deleteAttachment(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id): IO[Unit] =
      getError(user).fold(IO.unit)(IO.raiseError)
  }

  private val routes = AttachmentRoutes(service, ssoClient, 1).orNotFound

  private def headers(user: User): Headers = Headers(authHeader(user))

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

  test("GET requires authorization") {
    val request = Request[IO](method = Method.GET, uri = uri"attachment/p-1/a-1")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST requires authorization") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder"
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("DELETE requires authorization") {
    val request = Request[IO](method = Method.DELETE, uri = uri"attachment/p-1/a-1")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GET requires valid user") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/p-1/a-1",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST requires valid user") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("DELETE requires valid user") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/p-1/a-1",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("valid GET returns file contents") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/p-1/a-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.Ok, fileContents.some)
  }

  test("valid POST returns attachment") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.Ok, attachmentId.toString.some)
  }

  test("valid DELETE returns Ok") {
    val request =
      Request[IO](method = Method.DELETE, uri = uri"attachment/p-1/a-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.Ok, none)
  }

  test("GET returns NotFound for invalid program id") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/p1/a-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("GET returns NotFound for invalid attachment id") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/p-1/a-x1", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for invalid program id") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/a-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for missing fileName") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/a-1?attachmentType=finder",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for missing attachmentType") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/a-1?fileName=/file.txt/",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("DELETE returns NotFound for invalid program id") {
    val request =
      Request[IO](method = Method.DELETE, uri = uri"attachment/p/a-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("DELETE returns NotFound for invalid attachment id") {
    val request =
      Request[IO](method = Method.DELETE, uri = uri"attachment/p-1/a", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("GET returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/p-1/a-1",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GET returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/p-1/a-1",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("GET returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/p-1/a-1",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("POST returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("POST returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("DELETE returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/p-1/a-1",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("DELETE returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/p-1/a-1",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("DELETE returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/p-1/a-1",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

}
