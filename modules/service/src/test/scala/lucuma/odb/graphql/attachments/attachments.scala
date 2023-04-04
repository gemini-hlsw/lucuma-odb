// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.aws.s3.models.Models.FileKey
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.refined.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder

class attachments extends OdbSuiteWithS3 {

  case class TestAttachment(
    fileName:       String,
    attachmentType: String,
    description:    Option[String],
    content:        String
  ) {
    // The above values are allowed to be empty so that we can test the error conditions. However,
    // we can only check s3 if we have a file name.
    // So, only call this for values with NonEmpty file names
    def fileKey(programId: Program.Id): FileKey =
      awsConfig.fileKey(programId, NonEmptyString.unsafeFrom(fileName))
  }

  def assertAttachmentsOdb(
    user:        User,
    programId:   Program.Id,
    attachments: (Attachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
          query {
            program(programId: "$programId") {
              attachments {
                id
                attachmentType
                fileName
                description
                checked
                fileSize
              }
            }
          }
        """,
      expected = Right(
        Json.obj(
          "program" -> Json.obj(
            "attachments" -> Json.fromValues(
              attachments.map((tid, ta) =>
                Json.obj(
                  "id"             -> tid.asJson,
                  "attachmentType" -> ta.attachmentType.toUpperCase.asJson,
                  "fileName"       -> ta.fileName.asJson,
                  "description"    -> ta.description.asJson,
                  "checked"        -> false.asJson,
                  "fileSize"       -> ta.content.length.asJson
                )
              )
            )
          )
        )
      )
    )

  val client: Client[IO] = JavaNetClientBuilder[IO].create

  def uploadAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / programId.toString)
          .withQueryParam("fileName", ta.fileName)
          .withQueryParam("attachmentType", ta.attachmentType)
          .withOptionQueryParam("description", ta.description)

      val request = Request[IO](
        method = Method.POST,
        uri = uri,
        headers = Headers(authHeader(user))
      ).withEntity(ta.content)

      client.run(request)
    }

  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: Attachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      var uri     = svr.baseUri / "attachment" / programId.toString / attachmentId.toString
      var request = Request[IO](
        method = Method.GET,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def deleteAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: Attachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      var uri     = svr.baseUri / "attachment" / programId.toString / attachmentId.toString
      var request = Request[IO](
        method = Method.DELETE,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

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

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 30)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service)

  val file1A           = TestAttachment("file1", "finder", "A description".some, "Hopeful")
  val file1B           = TestAttachment("file1", "proposal", None, "New contents")
  val file1Empty       = TestAttachment("file1", "pre_imaging", "Thing".some, "")
  val file1MissingType = TestAttachment("file1", "", "Whatever".some, "It'll never make it")
  val file1InvalidType = TestAttachment("file1", "NotAType", none, "It'll never make it")
  val file2            = TestAttachment("file2", "mos_mask", "Masked".some, "Zorro")
  val fileWithPath     = TestAttachment("this/file.txt", "pre_imaging", none, "Doesn't matter")
  val missingFileName  = TestAttachment("", "finder", none, "Doesn't matter")

  test("successful upload, download and delete") {
    for {
      pid    <- createProgramAs(pi)
      fileKey = file1A.fileKey(pid)
      aid    <- uploadAttachment(pi, pid, file1A).toAttachmentId
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsOdb(pi, pid, (aid, file1A))
      _      <- getAttachment(pi, pid, aid).expectBody(file1A.content)
      _      <- deleteAttachment(pi, pid, aid).expectOk
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsOdb(pi, pid)
      _      <- getAttachment(pi, pid, aid).withExpectation(Status.NotFound)
    } yield ()
  }

  test("successful upload, download and delete of multiple files") {
    for {
      pid  <- createProgramAs(pi)
      fk1   = file1A.fileKey(pid)
      fk2   = file2.fileKey(pid)
      aid1 <- uploadAttachment(pi, pid, file1A).toAttachmentId
      _    <- assertS3(fk1, file1A.content)
      aid2 <- uploadAttachment(pi, pid, file2).toAttachmentId
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsOdb(pi, pid, (aid1, file1A), (aid2, file2))
      _    <- getAttachment(pi, pid, aid1).expectBody(file1A.content)
      _    <- getAttachment(pi, pid, aid2).expectBody(file2.content)
      _    <- deleteAttachment(pi, pid, aid1).expectOk
      _    <- getAttachment(pi, pid, aid1).withExpectation(Status.NotFound)
      _    <- getAttachment(pi, pid, aid2).expectBody(file2.content)
      _    <- assertS3NotThere(fk1)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsOdb(pi, pid, (aid2, file2))
    } yield ()
  }

  test("upload with same name overwrites previous upload") {
    for {
      pid    <- createProgramAs(pi)
      fileKey = file1A.fileKey(pid)
      aid    <- uploadAttachment(pi, pid, file1A).toAttachmentId
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsOdb(pi, pid, (aid, file1A))
      _      <- uploadAttachment(pi, pid, file1B).toAttachmentId.assertEquals(aid)
      _      <- assertS3(fileKey, file1B.content)
      _      <- assertAttachmentsOdb(pi, pid, (aid, file1B))
      _      <- getAttachment(pi, pid, aid).expectBody(file1B.content)
    } yield ()
  }

  test("empty file upload fails, doesn't overwrite previous") {
    for {
      pid    <- createProgramAs(pi)
      fileKey = file1A.fileKey(pid)
      aid    <- uploadAttachment(pi, pid, file1A).toAttachmentId
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsOdb(pi, pid, (aid, file1A))
      _      <- uploadAttachment(pi, pid, file1Empty).withExpectation(Status.InternalServerError)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsOdb(pi, pid, (aid, file1A))
    } yield ()
  }

  test("invalid attachment type upload fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- uploadAttachment(pi, pid, file1InvalidType).withExpectation(Status.BadRequest, "Invalid attachment type")
    } yield ()
  }

  test("empty attachment type upload fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- uploadAttachment(pi, pid, file1MissingType).withExpectation(Status.BadRequest, "Invalid attachment type")
    } yield ()
  }

  test("empty file name upload fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- uploadAttachment(pi, pid, missingFileName).withExpectation(Status.BadRequest, "File name is required")
    } yield ()
  }

  test("file name with path upload fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- uploadAttachment(pi, pid, fileWithPath).withExpectation(Status.BadRequest, "File name cannot include a path")
    } yield ()
  }

  test("pi can only upload to their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      fk1   = file1A.fileKey(pid1)
      fk2   = file1A.fileKey(pid2)
      _    <- uploadAttachment(pi, pid2, file1A).withExpectation(Status.Forbidden)
      _    <- uploadAttachment(pi2, pid1, file1A).withExpectation(Status.Forbidden)
      _    <- assertS3NotThere(fk1)
      _    <- assertS3NotThere(fk2)
      _    <- assertAttachmentsOdb(pi, pid1)
      _    <- assertAttachmentsOdb(pi2, pid2)
    } yield ()
  }

  test("pi can only download from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      fk1   = file1A.fileKey(pid1)
      fk2   = file2.fileKey(pid2)
      aid1 <- uploadAttachment(pi, pid1, file1A).toAttachmentId
      _    <- assertS3(fk1, file1A.content)
      _    <- assertAttachmentsOdb(pi, pid1, (aid1, file1A))
      aid2 <- uploadAttachment(pi2, pid2, file2).toAttachmentId
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsOdb(pi2, pid2, (aid2, file2))
      _    <- getAttachment(pi, pid2, aid2).withExpectation(Status.Forbidden)
      _    <- getAttachment(pi2, pid1, aid1).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only delete from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      fk1   = file1A.fileKey(pid1)
      fk2   = file2.fileKey(pid2)
      aid1 <- uploadAttachment(pi, pid1, file1A).toAttachmentId
      _    <- assertS3(fk1, file1A.content)
      _    <- assertAttachmentsOdb(pi, pid1, (aid1, file1A))
      aid2 <- uploadAttachment(pi2, pid2, file2).toAttachmentId
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsOdb(pi2, pid2, (aid2, file2))
      _    <- deleteAttachment(pi, pid2, aid2).withExpectation(Status.Forbidden)
      _    <- deleteAttachment(pi2, pid1, aid1).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("service user can manage any program's attachments") {
    for {
      pid    <- createProgramAs(pi)
      fileKey = file1A.fileKey(pid)
      aid    <- uploadAttachment(service, pid, file1A).toAttachmentId
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsOdb(service, pid, (aid, file1A))
      _      <- getAttachment(service, pid, aid).expectBody(file1A.content)
      _      <- deleteAttachment(service, pid, aid).expectOk
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsOdb(service, pid)
    } yield ()
  }

}
