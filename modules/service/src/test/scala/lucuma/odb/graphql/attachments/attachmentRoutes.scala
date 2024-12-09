// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.service.AttachmentFileService
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.NoTransaction
import lucuma.refined.*
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.implicits.*

class attachmentRoutes extends AttachmentRoutesSuite {

  private val attachmentId    = Attachment.Id(5.refined)

  private val service: AttachmentFileService[IO] = new AttachmentFileService[IO] {
    def getAttachment(
      user:         User,
      programId:    Program.Id,
      attachmentId: Attachment.Id
    )(using NoTransaction[IO]): IO[Either[AttachmentException, Stream[IO, Byte]]] = {
      val either = getError(user).fold(responseStream(fileContents).asRight)(_.asLeft)
      IO(either)
    }

    def insertAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: AttachmentType,
      fileName:       String,
      description:    Option[NonEmptyString],
      data:           Stream[IO, Byte]
    )(using NoTransaction[IO]): IO[Either[AttachmentException, Attachment.Id]] =
      getError(user).fold(attachmentId.asRight.pure[IO])(_.asLeft.pure)

    def updateAttachment(
      user: User,
      programId: Program.Id,
      attachmentId: Attachment.Id,
      fileName: String,
      description: Option[NonEmptyString],
      data: Stream[cats.effect.IO, Byte]
    )(using NoTransaction[IO]): IO[Either[AttachmentException, Unit]] =
      getError(user).fold(().asRight.pure[IO])(_.asLeft.pure)

    def deleteAttachment(user: User, programId: Program.Id, attachmentId: Attachment.Id)(using NoTransaction[IO]): IO[Either[AttachmentException, Unit]] =
      getError(user).fold(().asRight.pure[IO])(_.asLeft.pure)

    def getPresignedUrl(user: User, programId: Program.Id, attachmentId: Attachment.Id)(using NoTransaction[IO]): IO[Either[AttachmentException, String]] =
      getError(user).fold(presignedUrl.asRight.pure[IO])(_.asLeft.pure)
  }

  private val routes: HttpApp[IO] = AttachmentRoutes(service, ssoClient, 1).orNotFound

  test("GET requires authorization") {
    val request = Request[IO](method = Method.GET, uri = uri"attachment/p-1/a-1")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GETurl requires authorization") {
    val request = Request[IO](method = Method.GET, uri = uri"attachment/url/p-1/a-1")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST requires authorization") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder"
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("PUT requires authorization") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1?fileName=/file.txt"
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

  test("GETurl requires valid user") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/url/p-1/a-1",
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

  test("PUT requires valid user") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1?fileName=/file.txt",
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

  test("valid GETurl returns url") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/url/p-1/a-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.Ok, presignedUrl.some)
  }

  test("valid POST returns attachment") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.Ok, attachmentId.toString.some)
  }

  test("valid PUT returns Ok") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.Ok, none)
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

  test("GETurl returns NotFound for invalid program id") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/url/p1/a-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("GETurl returns NotFound for invalid attachment id") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/url/p-1/a-x1", headers = headers(pi))
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
                              uri = uri"attachment/p-1?attachmentType=finder",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for missing attachmentType") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("PUT returns NotFound for invalid program id") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/z-1/a-1?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("PUT returns NotFound for invalid attachment id") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/q-1?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("PUT returns NotFound for missing fileName") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1",
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

  test("GETurl returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/url/p-1/a-1",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GETurl returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/url/p-1/a-1",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("GETurl returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/url/p-1/a-1",
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

  test("POST returns BadRequest with message if service returns FileNotFound") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/p-1?fileName=/file.txt/&attachmentType=finder",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("PUT returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1?fileName=/file.txt",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("PUT returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1?fileName=/file.txt",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("PUT returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/p-1/a-1?fileName=/file.txt",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
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
