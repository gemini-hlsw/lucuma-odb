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
import lucuma.odb.service.AttachmentFileService.AttachmentException
import lucuma.odb.service.ProposalAttachmentFileService
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.implicits.*
import skunk.Transaction

class proposalAttachmentRoutes extends AttachmentRoutesSuite {
  
  private val service: ProposalAttachmentFileService[IO] = new ProposalAttachmentFileService[IO] {
    def getAttachment(
      user: User,
      programId: Program.Id,
      attachmentType: Tag
    )(using Transaction[IO]): IO[Either[AttachmentException, Stream[cats.effect.IO, Byte]]] = {
      val either = getError(user).fold(responseStream(fileContents).asRight)(_.asLeft)
      IO(either)
    }

    def insertAttachment(
      user: User,
      programId: Program.Id,
      attachmentType: Tag,
      fileName: String,
      description: Option[NonEmptyString],
      data: Stream[cats.effect.IO, Byte]
    )(using Transaction[IO]): IO[Unit] = 
      getError(user).fold(IO.unit)(IO.raiseError)

    def updateAttachment(
      user: User,
      programId: Program.Id,
      attachmentType: Tag,
      fileName: String,
      description: Option[NonEmptyString],
      data: Stream[cats.effect.IO, Byte]
    )(using Transaction[IO]): IO[Unit] =
      getError(user).fold(IO.unit)(IO.raiseError)

    def deleteAttachment(user: User, programId: Program.Id, attachmentType: Tag)(using Transaction[IO]): IO[Unit] = 
      getError(user).fold(IO.unit)(IO.raiseError)

    def getPresignedUrl(user: User, programId: Program.Id, attachmentType: Tag)(using Transaction[IO]): IO[String] =
      getError(user).fold(IO(presignedUrl))(IO.raiseError)
  }

  private val routes: HttpApp[IO] = ??? // ProposalAttachmentRoutes(service, ssoClient, 1).orNotFound

  test("GET requires authorization") {
    val request = Request[IO](method = Method.GET, uri = uri"attachment/proposal/p-1/science")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GETurl requires authorization") {
    val request = Request[IO](method = Method.GET, uri = uri"attachment/proposal/url/p-1/science")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST requires authorization") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/team?fileName=/file.txt"
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("PUT requires authorization") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/team?fileName=/file.txt"
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("DELETE requires authorization") {
    val request = Request[IO](method = Method.DELETE, uri = uri"attachment/proposal/p-1/team")
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GET requires valid user") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/p-1/science",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GETurl requires valid user") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/url/p-1/science",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST requires valid user") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/team?fileName=/file.txt",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("PUT requires valid user") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/science?fileName=/file.txt",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("DELETE requires valid user") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/proposal/p-1/science",
                              headers = Headers(invalidAuthHeader)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("valid GET returns file contents") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/proposal/p-1/science", headers = headers(pi))
    routes.run(request).assertResponse(Status.Ok, fileContents.some)
  }

  test("valid GETurl returns url") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/proposal/url/p-1/science", headers = headers(pi))
    routes.run(request).assertResponse(Status.Ok, presignedUrl.some)
  }

  test("valid POST returns Ok") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/science?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.Ok, none)
  }

  test("valid PUT returns Ok") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/science?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.Ok, none)
  }

  test("valid DELETE returns Ok") {
    val request =
      Request[IO](method = Method.DELETE, uri = uri"attachment/proposal/p-1/science", headers = headers(pi))
    routes.run(request).assertResponse(Status.Ok, none)
  }

  test("GET returns NotFound for invalid program id") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/proposal/p1/team", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("GETurl returns NotFound for invalid program id") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/proposal/url/p1/team", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("GET returns NotFound for missing attachment type") {
    val request =
      Request[IO](method = Method.GET, uri = uri"attachment/proposal/p-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for invalid program id") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/a-1/team?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for missing fileName") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("POST returns NotFound for missing attachmentType") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1?fileName=/file.txt/",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("PUT returns NotFound for invalid program id") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/z-1/science?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("PUT returns NotFound for missing attachment type") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1?fileName=/file.txt",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("PUT returns NotFound for missing fileName") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/science",
                              headers = headers(pi)
    )
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("DELETE returns NotFound for invalid program id") {
    val request =
      Request[IO](method = Method.DELETE, uri = uri"attachment/proposal/p/team", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("DELETE returns NotFound for missing attachment type") {
    val request =
      Request[IO](method = Method.DELETE, uri = uri"attachment/proposal/p-1", headers = headers(pi))
    routes.run(request).assertResponse(Status.NotFound, notFound)
  }

  test("GET returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GET returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("GET returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("GETurl returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/url/p-1/team",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("GETurl returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/url/p-1/team",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("GETurl returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.GET,
                              uri = uri"attachment/proposal/url/p-1/team",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("POST returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/team?fileName=/file.txt",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("POST returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/team?fileName=/file.txt",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("POST returns BadRequest if service returns FileNotFound") {
    val request = Request[IO](method = Method.POST,
                              uri = uri"attachment/proposal/p-1/team?fileName=/file.txt",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("PUT returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/science?fileName=/file.txt",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }

  test("PUT returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/science?fileName=/file.txt",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("PUT returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.PUT,
                              uri = uri"attachment/proposal/p-1/science?fileName=/file.txt",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("DELETE returns Forbidden if service returns Forbidden") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(forbiddenUser)
    )
    routes.run(request).assertResponse(Status.Forbidden, none)
  }

  test("DELETE returns NotFound if service returns FileNotFound") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(fileNotFoundUser)
    )
    routes.run(request).assertResponse(Status.NotFound, none)
  }

  test("DELETE returns BadRequest with message if service returns InvalidRequest") {
    val request = Request[IO](method = Method.DELETE,
                              uri = uri"attachment/proposal/p-1/team",
                              headers = headers(invalidFileUser)
    )
    routes.run(request).assertResponse(Status.BadRequest, invalidFileName.some)
  }
}
