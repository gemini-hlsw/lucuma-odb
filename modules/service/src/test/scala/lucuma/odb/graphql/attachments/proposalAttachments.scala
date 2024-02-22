// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package attachments

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.FMain
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs.*
import natchez.Trace.Implicits.noop
import org.http4s.*
import skunk.*
import skunk.syntax.all.*

class proposalAttachments extends AttachmentsSuite {

  case class TestAttachment(
    fileName:       String,
    attachmentType: String,
    content:        String
  ) {
    val upperType: String = attachmentType.toUpperCase
  }

  def assertAttachmentsGql(
    user:        User,
    programId:   Program.Id,
    expectedTas: TestAttachment*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
          query {
            program(programId: "$programId") {
              proposalAttachments {
                attachmentType
                fileName
                fileSize
              }
            }
          }
        """,
      expected = Right(
        Json.obj(
          "program" -> expected(expectedTas*)
        )
      )
    )

  def updateAttachmentsGql(
    user:        User,
    programId:   Program.Id,
    WHERE:       String,
    SET:         String,
    expectedTas: TestAttachment*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateProposalAttachments(
            input: {
              programId: "$programId"
              WHERE: """ + WHERE + """
              SET: """ + SET + """
            }
          ) {
            proposalAttachments {
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
          "updateProposalAttachments" -> expected(expectedTas*)
        )
      )
    )

  def expected(attachments: TestAttachment*): Json =
    Json.obj(
      "proposalAttachments" -> Json.fromValues(
        attachments.sortBy(_.attachmentType).map(ta =>
          Json.obj(
            "attachmentType" -> ta.attachmentType.toUpperCase.asJson,
            "fileName"       -> ta.fileName.asJson,
            "fileSize"       -> ta.content.length.asJson
          )
        )
      )
    )

  def insertAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / "proposal" / programId.toString / ta.attachmentType)
          .withQueryParam("fileName", ta.fileName)

      val request = Request[IO](
        method = Method.POST,
        uri = uri,
        headers = Headers(authHeader(user))
      ).withEntity(ta.content)

      client.run(request)
    }

  def updateAttachment(
    user:         User,
    programId:    Program.Id,
    ta:           TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / "proposal" / programId.toString / ta.attachmentType)
          .withQueryParam("fileName", ta.fileName)

      val request = Request[IO](
        method = Method.PUT,
        uri = uri,
        headers = Headers(authHeader(user))
      ).withEntity(ta.content)

      client.run(request)
    }

  def getAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri     = svr.baseUri / "attachment" / "proposal" / programId.toString / ta.attachmentType
      val request = Request[IO](
        method = Method.GET,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def getPresignedUrl(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri     = svr.baseUri / "attachment" / "proposal" / "url" / programId.toString / ta.attachmentType
      val request = Request[IO](
        method = Method.GET,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def deleteAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri     = svr.baseUri / "attachment" / "proposal" / programId.toString / ta.attachmentType
      val request = Request[IO](
        method = Method.DELETE,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }

  def getRemotePathFromDb(pid: Program.Id, ta: TestAttachment): IO[NonEmptyString] = {
    val query =
      sql"""
        select c_remote_path from t_proposal_attachment
        where c_program_id = $program_id and c_attachment_type = $tag
      """.query(text_nonempty)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(pid, Tag(ta.attachmentType)))
    )
  }

  private val file1A           = TestAttachment("file1.pdf", "science", "Hopeful")
  private val file1B           = TestAttachment("file1.pdf", "science", "New contents")
  private val file1C           = TestAttachment("file1.pdf", "team", "Same name, different type")
  private val file1Empty       = TestAttachment("file1.pdf", "science", "")
  private val file1InvalidType = TestAttachment("file1.pdf", "NotAType", "It'll never make it")
  private val file2            = TestAttachment("file2.pdf", "team", "Zorro")
  private val fileWithPath     = TestAttachment("this/file.pdf", "science", "Doesn't matter")
  private val missingFileName  = TestAttachment("", "science", "Doesn't matter")
  // same attachment type as file1A, but different name, etc.
  private val file3            = TestAttachment("different.pdf", "science", "Something different")
  private val missingExtension = TestAttachment("file1", "science", "Doesn't matter")
  private val emptyExtension   = TestAttachment("file1.", "team", "Doesn't matter")
  private val invalidExtension = TestAttachment("file1.pif", "science", "Doesn't matter")

  val invalidExtensionMsg = "Invalid file. Must be a PDF file."

  test("successful insert, download and delete") {
    for {
      pid    <- createProgramAs(pi)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      _      <- getAttachment(pi, pid, file1A).expectBody(file1A.content)
      _      <- deleteAttachment(pi, pid, file1A).expectOk
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid)
      _      <- getAttachment(pi, pid, file1A).withExpectation(Status.NotFound)
    } yield ()
  }

  test("can download via presigned url") {
    for {
      pid    <- createProgramAs(pi)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      url    <- getPresignedUrl(pi, pid, file1A).toNonEmptyString
      _      <- getViaPresignedUrl(url).expectBody(file1A.content)
    } yield ()
  }

  test("successful insert, download and delete of multiple files") {
    for {
      pid  <- createProgramAs(pi)
      _    <- insertAttachment(pi, pid, file1A).expectOk
      pth1 <- getRemotePathFromDb(pid, file1A)
      fk1   = awsConfig.fileKey(pth1)
      _    <- assertS3(fk1, file1A.content)
      _    <- insertAttachment(pi, pid, file2).expectOk
      pth2 <- getRemotePathFromDb(pid, file2)
      fk2   = awsConfig.fileKey(pth2)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi, pid, file1A, file2)
      _    <- getAttachment(pi, pid, file1A).expectBody(file1A.content)
      _    <- getAttachment(pi, pid, file2).expectBody(file2.content)
      _    <- deleteAttachment(pi, pid, file1A).expectOk
      _    <- getAttachment(pi, pid, file1A).withExpectation(Status.NotFound)
      _    <- getAttachment(pi, pid, file2).expectBody(file2.content)
      _    <- assertS3NotThere(fk1)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi, pid, file2)
    } yield ()
  }

  test("update with different name is successful") {
    for {
      pid    <- createProgramAs(pi)
      _       = assertEquals(file1A.attachmentType, file3.attachmentType)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      _      <- getAttachment(pi, pid, file1A).expectBody(file1A.content)
      _      <- updateAttachment(pi, pid, file3).expectOk
      path2  <- getRemotePathFromDb(pid, file1A) // use the original type
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, file3.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, file3)
      _      <- getAttachment(pi, pid, file1A).expectBody(file3.content)
    } yield ()
  }

  test("update with same name is successful") {
    for {
      pid    <- createProgramAs(pi)
      _       = assertEquals(file1A.attachmentType, file1B.attachmentType)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      _      <- getAttachment(pi, pid, file1A).expectBody(file1A.content)
      _      <- updateAttachment(pi, pid, file1B).expectOk
      path2  <- getRemotePathFromDb(pid, file1A)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, file1B.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, file1B)
      _      <- getAttachment(pi, pid, file1A).expectBody(file1B.content)
    } yield ()
  }

  test("update of non-existent attachment is NotFound") {
    for {
      pid <- createProgramAs(pi)
      _   <- updateAttachment(pi, pid, file1A).withExpectation(Status.NotFound)
    } yield ()
  }

  test("insert with duplicate type is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      _      <- insertAttachment(pi, pid, file3).withExpectation(Status.BadRequest, "Duplicate attachment type")
    } yield ()
  }
  test("insert with duplicate name is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      _      <- insertAttachment(pi, pid, file1C).withExpectation(Status.BadRequest, "Duplicate file name")
    } yield ()
  }

  test("update with duplicate name is a BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- getRemotePathFromDb(pid, file1A)
      _   <- insertAttachment(pi, pid, file2).expectOk
      _   <- getRemotePathFromDb(pid, file2)
      _   <- assertAttachmentsGql(pi, pid, file2, file1A)
      _   <- updateAttachment(pi, pid, file1C).withExpectation(Status.BadRequest, "Duplicate file name")
      _   <- assertAttachmentsGql(pi, pid, file2, file1A)
      _   <- getAttachment(pi, pid, file2).expectBody(file2.content)
    } yield ()
  }

  test("empty file insert is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      _      <- insertAttachment(pi, pid, file1Empty).withExpectation(Status.BadRequest, "File cannot be empty")
    } yield ()
  }

  test("empty file update is a BadRequest, doesn't overwrite previous") {
    for {
      pid    <- createProgramAs(pi)
      _       = assertEquals(file1A.attachmentType, file1Empty.attachmentType)
      _      <- insertAttachment(pi, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
      _      <- updateAttachment(pi, pid, file1Empty).withExpectation(Status.BadRequest, "File cannot be empty")
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, file1A)
    } yield ()
  }

  test("invalid attachment type insert is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, file1InvalidType).withExpectation(Status.BadRequest, "Invalid attachment type")
    } yield ()
  }

  test("insert with empty file name is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, missingFileName).withExpectation(Status.BadRequest, "File name is required")
    } yield ()
  }

  test("update with empty file name is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _    = assertEquals(file1A.attachmentType, missingFileName.attachmentType)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- updateAttachment(pi, pid, missingFileName).withExpectation(Status.BadRequest, "File name is required")
    } yield ()
  }

  test("file name with path insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, fileWithPath).withExpectation(Status.BadRequest, "File name cannot include a path")
    } yield ()
  }

  test("file name with path update fails") {
    for {
      pid <- createProgramAs(pi)
      _    = assertEquals(file1A.attachmentType, fileWithPath.attachmentType)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- updateAttachment(pi, pid, fileWithPath).withExpectation(Status.BadRequest, "File name cannot include a path")
    } yield ()
  }

  test("file name with missing extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, missingExtension).withExpectation(Status.BadRequest, invalidExtensionMsg)
    } yield ()
  }

  test("file name with missing extension update fails") {
    for {
      pid <- createProgramAs(pi)
      _    = assertEquals(file1A.attachmentType, fileWithPath.attachmentType)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- updateAttachment(pi, pid, missingExtension).withExpectation(Status.BadRequest, invalidExtensionMsg)
    } yield ()
  }

  test("file name with empty extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, emptyExtension).withExpectation(Status.BadRequest, invalidExtensionMsg)
    } yield ()
  }

  test("file name with empty extension update fails") {
    for {
      pid <- createProgramAs(pi)
      _    = assertEquals(file1A.attachmentType, fileWithPath.attachmentType)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- updateAttachment(pi, pid, emptyExtension).withExpectation(Status.BadRequest, invalidExtensionMsg)
    } yield ()
  }

  test("file name with invalid extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, invalidExtension).withExpectation(Status.BadRequest, invalidExtensionMsg)
    } yield ()
  }

  test("file name with invalid extension update fails") {
    for {
      pid <- createProgramAs(pi)
      _    = assertEquals(file1A.attachmentType, fileWithPath.attachmentType)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- updateAttachment(pi, pid, invalidExtension).withExpectation(Status.BadRequest, invalidExtensionMsg)
    } yield ()
  }

  test("pi can only insert to their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      _    <- insertAttachment(pi, pid2, file1A).withExpectation(Status.Forbidden)
      _    <- insertAttachment(pi2, pid1, file1A).withExpectation(Status.Forbidden)
      _    <- assertAttachmentsGql(pi, pid1)
      _    <- assertAttachmentsGql(pi2, pid2)
    } yield ()
  }

  test("pi can only update their own programs") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, file1A).expectOk
      _   <- updateAttachment(pi2, pid, file3).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only see their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      _    <- insertAttachment(pi, pid1, file1A).expectOk
      _    <- insertAttachment(pi2, pid2, file1B).expectOk
      _    <- assertAttachmentsGql(pi, pid1, file1A)
      _    <- assertAttachmentsGql(pi2, pid2, file1B)
    } yield ()
  }

  test("pi can only download from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      _    <- insertAttachment(pi, pid1, file1A).expectOk
      pth1 <- getRemotePathFromDb(pid1, file1A)
      fk1   = awsConfig.fileKey(pth1)
      _    <- assertS3(fk1, file1A.content)
      _    <- assertAttachmentsGql(pi, pid1, file1A)
      _    <- insertAttachment(pi2, pid2, file2).expectOk
      pth2 <- getRemotePathFromDb(pid2, file2)
      fk2   = awsConfig.fileKey(pth2)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi2, pid2, file2)
      _    <- getAttachment(pi, pid2, file2).withExpectation(Status.Forbidden)
      _    <- getAttachment(pi2, pid1, file1A).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only delete from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      _    <- insertAttachment(pi, pid1, file1A).expectOk
      pth1 <- getRemotePathFromDb(pid1, file1A)
      fk1   = awsConfig.fileKey(pth1)
      _    <- assertS3(fk1, file1A.content)
      _    <- assertAttachmentsGql(pi, pid1, file1A)
      _    <- insertAttachment(pi2, pid2, file2).expectOk
      pth2 <- getRemotePathFromDb(pid2, file2)
      fk2   = awsConfig.fileKey(pth2)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi2, pid2, file2)
      _    <- deleteAttachment(pi, pid2, file2).withExpectation(Status.Forbidden)
      _    <- deleteAttachment(pi2, pid1, file1A).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("service user can manage any program's attachments") {
    for {
      pid    <- createProgramAs(pi)
      _       = assertEquals(file1A.attachmentType, file3.attachmentType)
      _      <- insertAttachment(service, pid, file1A).expectOk
      path   <- getRemotePathFromDb(pid, file1A)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(service, pid, file1A)
      _      <- getAttachment(service, pid, file1A).expectBody(file1A.content)
      _      <- updateAttachment(service, pid, file3).expectOk
      path2  <- getRemotePathFromDb(pid, file3)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, file3.content)
      _      <- assertS3NotThere(fileKey)
      _      <- deleteAttachment(service, pid, file3).expectOk
      _      <- assertS3NotThere(fk2)
      _      <- assertAttachmentsGql(service, pid)
    } yield ()
  }
}
