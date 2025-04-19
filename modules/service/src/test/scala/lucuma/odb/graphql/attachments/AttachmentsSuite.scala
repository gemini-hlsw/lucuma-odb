// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package attachments

import cats.Order.given
import cats.effect.IO
import cats.effect.Resource
import eu.timepit.refined.types.string.NonEmptyString
import fs2.text.utf8
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.core.model.User
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

abstract class AttachmentsSuite extends OdbSuiteWithS3 {
  
  // this logger is turned off to silence some errors (see logback.xml)
  override implicit val log: Logger[IO] =
    Slf4jLogger.getLoggerFromName("lucuma-odb-test-attachments")

  val client: Client[IO] = JavaNetClientBuilder[IO].create

  extension (response: Response[IO])
    def getBody: IO[String] = response.body.through(utf8.decode).compile.string

  extension (response: Resource[IO, Response[IO]])
    def withExpectation(expectedStatus: Status, expectedBody: String = ""): IO[Unit] =
      response
        .use(resp => resp.getBody.map(s => (resp.status, s)))
        .assertEquals((expectedStatus, expectedBody))

    def expectBody(body: String): IO[Unit] = withExpectation(Status.Ok, body)

    def expectOk: IO[Unit] = withExpectation(Status.Ok)

    def toAttachmentId: IO[Attachment.Id] =
      response.use { resp =>
        for {
          _   <- IO(resp.status).assertEquals(Status.Ok)
          aid <- resp.getBody.map(s => Attachment.Id.parse(s).get)
        } yield aid
      }
    
    def toNonEmptyString: IO[NonEmptyString] =
      response.use { resp =>
        for {
          _   <- IO(resp.status).assertEquals(Status.Ok)
          nes <- resp.getBody.map(s => NonEmptyString.unsafeFrom(s))
        } yield nes
      }

  val pi: User      = TestUsers.Standard.pi(1, 30)
  val pi2: User     = TestUsers.Standard.pi(2, 30)
  val service: User = TestUsers.service(3)

  val validUsers: List[User] = List(pi, pi2, service)

  case class TestAttachment(
    fileName:       String,
    attachmentType: String,
    description:    Option[String],
    content:        String,
    checked:        Boolean = false
  ) {
    val upperType: String = attachmentType.toUpperCase
  }

  def insertAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap { svr =>
        val uri =
          (svr.baseUri / "attachment")
            .withQueryParam("programId", programId.toString)
            .withQueryParam("fileName", ta.fileName)
            .withQueryParam("attachmentType", ta.attachmentType)
            .withOptionQueryParam("description", ta.description)

        val request = Request[IO](
          method = Method.POST,
          uri = uri,
          headers = Headers(auth)
        ).withEntity(ta.content)

        client.run(request)
      }
  
  def updateAttachment(
    user:         User,
    attachmentId: Attachment.Id,
    ta:           TestAttachment
  ): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap { svr =>
        val uri =
          (svr.baseUri / "attachment" / attachmentId.toString)
            .withQueryParam("fileName", ta.fileName)
            .withOptionQueryParam("description", ta.description)

        val request = Request[IO](
          method = Method.PUT,
          uri = uri,
          headers = Headers(auth)
        ).withEntity(ta.content)

        client.run(request)
      }

  def getAttachment(
    user:         User,
    attachmentId: Attachment.Id
  ): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap { svr =>
        val uri     = svr.baseUri / "attachment" / attachmentId.toString
        val request = Request[IO](
          method = Method.GET,
          uri = uri,
          headers = Headers(auth)
        )

        client.run(request)
      }

  def getViaPresignedUrl(url: NonEmptyString): Resource[IO, Response[IO]] =
    server.flatMap { _ =>
      val uri = Uri.unsafeFromString(url.value)
      val request = Request[IO](
        method = Method.GET,
        uri = uri,
      )

      client.run(request)
    }

  def getPresignedUrl(
    user:         User,
    attachmentId: Attachment.Id
  ): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap { svr =>
        val uri     = svr.baseUri / "attachment" / "url" / attachmentId.toString
        val request = Request[IO](
          method = Method.GET,
          uri = uri,
          headers = Headers(auth)
        )

        client.run(request)
      }

  def deleteAttachment(
    user:         User,
    attachmentId: Attachment.Id
  ): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap { svr =>
        val uri     = svr.baseUri / "attachment" / attachmentId.toString
        val request = Request[IO](
          method = Method.DELETE,
          uri = uri,
          headers = Headers(auth)
        )

        client.run(request)
      }

  def expectedAttachments(
    attachments: List[(Attachment.Id, TestAttachment)]
  ): Json =
    Json.obj(
      "attachments" -> Json.fromValues(
        attachments.sortBy(_._1).map((tid, ta) =>
          Json.obj(
            "id"             -> tid.asJson,
            "attachmentType" -> ta.attachmentType.toUpperCase.asJson,
            "fileName"       -> ta.fileName.asJson,
            "description"    -> ta.description.asJson,
            "checked"        -> ta.checked.asJson,
            "fileSize"       -> ta.content.length.asJson
          )
        )
      )
    )

  val AttachmentsGraph: String =
    """attachments {
       |  id
       |  attachmentType
       |  fileName
       |  description
       |  checked
       |  fileSize
       |}""".stripMargin
}
