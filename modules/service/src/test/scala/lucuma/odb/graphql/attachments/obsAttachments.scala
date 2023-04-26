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
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.FMain
import lucuma.odb.util.Codecs.obs_attachment_id
import lucuma.refined.*
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import java.util.UUID

class obsAttachments extends OdbSuiteWithS3 {
  
  case class TestAttachment(
    fileName:       String,
    attachmentType: String,
    description:    Option[String],
    content:        String,
    checked:        Boolean = false
  ) 

  def assertAttachmentsGql(
    user:        User,
    programId:   Program.Id,
    expectedTas: (ObsAttachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
          query {
            program(programId: "$programId") {
              obsAttachments {
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
          "program" -> expected(expectedTas: _*)
        )
      )
    )

  def updateAttachmentsGql(
    user:        User,
    programId:   Program.Id,
    WHERE:       String,
    SET:         String,
    expectedTas: (ObsAttachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObsAttachments(
            input: {
              programId: "$programId"
              WHERE: """ + WHERE + """
              SET: """ + SET + """
            }
          ) {
            obsAttachments {
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
          "updateObsAttachments" -> expected(expectedTas: _*)
        )
      )
    )

  def expected(attachments: (ObsAttachment.Id, TestAttachment)*): Json =
    Json.obj(
      "obsAttachments" -> Json.fromValues(
        attachments.map((tid, ta) =>
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

  val client: Client[IO] = JavaNetClientBuilder[IO].create

  def insertAttachment(
    user:      User,
    programId: Program.Id,
    ta:        TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / "obs" / programId.toString)
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
  
  def updateAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id,
    ta:           TestAttachment
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      val uri =
        (svr.baseUri / "attachment" / "obs" / programId.toString / attachmentId.toString)
          .withQueryParam("fileName", ta.fileName)
          .withOptionQueryParam("description", ta.description)

      val request = Request[IO](
        method = Method.PUT,
        uri = uri,
        headers = Headers(authHeader(user))
      ).withEntity(ta.content)

      client.run(request)
    }

  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      var uri     = svr.baseUri / "attachment" / "obs" / programId.toString / attachmentId.toString
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
    attachmentId: ObsAttachment.Id
  ): Resource[IO, Response[IO]] =
    server.flatMap { svr =>
      var uri     = svr.baseUri / "attachment" / "obs" / programId.toString / attachmentId.toString
      var request = Request[IO](
        method = Method.DELETE,
        uri = uri,
        headers = Headers(authHeader(user))
      )

      client.run(request)
    }
  
  def getRemoteIdFromDb(aid: ObsAttachment.Id): IO[UUID] = {
    val query = sql"select c_remote_id from t_obs_attachment where c_obs_attachment_id = $obs_attachment_id".query(uuid)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(aid))
    )
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

    def toAttachmentId: IO[ObsAttachment.Id] =
      response.use { resp =>
        for {
          _   <- IO(resp.status).assertEquals(Status.Ok)
          aid <- resp.getBody.map(s => ObsAttachment.Id.parse(s).get)
        } yield aid
      }

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 30)
  val service = TestUsers.service(3)

  val validUsers = List(pi, pi2, service)

  val file1A           = TestAttachment("file1", "finder", "A description".some, "Hopeful")
  val file1B           = TestAttachment("file1", "mos_mask", None, "New contents")
  val file1Empty       = TestAttachment("file1", "pre_imaging", "Thing".some, "")
  val file1MissingType = TestAttachment("file1", "", "Whatever".some, "It'll never make it")
  val file1InvalidType = TestAttachment("file1", "NotAType", none, "It'll never make it")
  val file2            = TestAttachment("file2", "mos_mask", "Masked".some, "Zorro")
  val fileWithPath     = TestAttachment("this/file.txt", "pre_imaging", none, "Doesn't matter")
  val missingFileName  = TestAttachment("", "finder", none, "Doesn't matter")
  val file3            = TestAttachment("different", "mos_mask", "Unmatching file name".some, "Something different")

  test("successful insert, download and delete") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
      _      <- getAttachment(pi, pid, aid).expectBody(file1A.content)
      _      <- deleteAttachment(pi, pid, aid).expectOk
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid)
      _      <- getAttachment(pi, pid, aid).withExpectation(Status.NotFound)
    } yield ()
  }

  test("successful insert, download and delete of multiple files") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid1 <- getRemoteIdFromDb(aid1)
      fk1   = awsConfig.obsFileKey(pid, rid1)
      _    <- assertS3(fk1, file1A.content)
      aid2 <- insertAttachment(pi, pid, file2).toAttachmentId
      rid2 <- getRemoteIdFromDb(aid2)
      fk2   = awsConfig.obsFileKey(pid, rid2)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi, pid, (aid1, file1A), (aid2, file2))
      _    <- getAttachment(pi, pid, aid1).expectBody(file1A.content)
      _    <- getAttachment(pi, pid, aid2).expectBody(file2.content)
      _    <- deleteAttachment(pi, pid, aid1).expectOk
      _    <- getAttachment(pi, pid, aid1).withExpectation(Status.NotFound)
      _    <- getAttachment(pi, pid, aid2).expectBody(file2.content)
      _    <- assertS3NotThere(fk1)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi, pid, (aid2, file2))
    } yield ()
  }

  test("update with different name is successful") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
      _      <- getAttachment(pi, pid, aid).expectBody(file1A.content)
      _      <- updateAttachment(pi, pid, aid, file2).expectOk
      rid2   <- getRemoteIdFromDb(aid)
      _       = assertNotEquals(rid, rid2)
      fk2     = awsConfig.obsFileKey(pid, rid2)
      _      <- assertS3(fk2, file2.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, (aid, file2.copy(attachmentType = file1A.attachmentType)))
      _      <- getAttachment(pi, pid, aid).expectBody(file2.content)
    } yield ()
  }

  test("update with same name is successful") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
      _      <- getAttachment(pi, pid, aid).expectBody(file1A.content)
      _      <- updateAttachment(pi, pid, aid, file1B).expectOk
      rid2   <- getRemoteIdFromDb(aid)
      _       = assertNotEquals(rid, rid2)
      fk2     = awsConfig.obsFileKey(pid, rid2)
      _      <- assertS3(fk2, file1B.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1B.copy(attachmentType = file1A.attachmentType)))
      _      <- getAttachment(pi, pid, aid).expectBody(file1B.content)
    } yield ()
  }

  test("insert with duplicate name is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
      _      <- insertAttachment(pi, pid, file1B).withExpectation(Status.BadRequest, "Duplicate file name")
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
      _      <- getAttachment(pi, pid, aid).expectBody(file1A.content)
    } yield ()
  }

  // Note: The order in the GQL is not by id. If the order changes, this test could break.
  test("update with duplicate name is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      aid2   <- insertAttachment(pi, pid, file2).toAttachmentId
      rid2   <- getRemoteIdFromDb(aid2)
      fk2     = awsConfig.obsFileKey(pid, rid2)
      _      <- assertAttachmentsGql(pi, pid, (aid2, file2), (aid, file1A))
      _      <- updateAttachment(pi, pid, aid2, file1B).withExpectation(Status.BadRequest, "Duplicate file name")
      _      <- assertAttachmentsGql(pi, pid, (aid2, file2), (aid, file1A))
      _      <- getAttachment(pi, pid, aid2).expectBody(file2.content)
    } yield ()
  }

  test("empty file update fails, doesn't overwrite previous") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
      _      <- updateAttachment(pi, pid, aid, file1Empty).withExpectation(Status.InternalServerError)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, file1A))
    } yield ()
  }

  test("invalid attachment type insert is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, file1InvalidType).withExpectation(Status.BadRequest, "Invalid attachment type")
    } yield ()
  }

  test("empty attachment type insert is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, file1MissingType).withExpectation(Status.BadRequest, "Invalid attachment type")
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
      aid <- insertAttachment(pi, pid, file1A).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, missingFileName).withExpectation(Status.BadRequest, "File name is required")
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
      aid <- insertAttachment(pi, pid, file1A).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, fileWithPath).withExpectation(Status.BadRequest, "File name cannot include a path")
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
      aid <- insertAttachment(pi, pid, file1A).toAttachmentId
      _   <- updateAttachment(pi2, pid, aid, file2).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only download from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      aid1 <- insertAttachment(pi, pid1, file1A).toAttachmentId
      rid1 <- getRemoteIdFromDb(aid1)
      fk1   = awsConfig.obsFileKey(pid1, rid1)
      _    <- assertS3(fk1, file1A.content)
      _    <- assertAttachmentsGql(pi, pid1, (aid1, file1A))
      aid2 <- insertAttachment(pi2, pid2, file2).toAttachmentId
      rid2 <- getRemoteIdFromDb(aid2)
      fk2   = awsConfig.obsFileKey(pid2, rid2)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi2, pid2, (aid2, file2))
      _    <- getAttachment(pi, pid2, aid2).withExpectation(Status.Forbidden)
      _    <- getAttachment(pi2, pid1, aid1).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only delete from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      aid1 <- insertAttachment(pi, pid1, file1A).toAttachmentId
      rid1 <- getRemoteIdFromDb(aid1)
      fk1   = awsConfig.obsFileKey(pid1, rid1)
      _    <- assertS3(fk1, file1A.content)
      _    <- assertAttachmentsGql(pi, pid1, (aid1, file1A))
      aid2 <- insertAttachment(pi2, pid2, file2).toAttachmentId
      rid2 <- getRemoteIdFromDb(aid2)
      fk2   = awsConfig.obsFileKey(pid2, rid2)
      _    <- assertS3(fk2, file2.content)
      _    <- assertAttachmentsGql(pi2, pid2, (aid2, file2))
      _    <- deleteAttachment(pi, pid2, aid2).withExpectation(Status.Forbidden)
      _    <- deleteAttachment(pi2, pid1, aid1).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("service user can manage any program's attachments") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(service, pid, file1A).toAttachmentId
      rid    <- getRemoteIdFromDb(aid)
      fileKey = awsConfig.obsFileKey(pid, rid)
      _      <- assertS3(fileKey, file1A.content)
      _      <- assertAttachmentsGql(service, pid, (aid, file1A))
      _      <- getAttachment(service, pid, aid).expectBody(file1A.content)
      _      <- updateAttachment(service, pid, aid, file2).expectOk
      rid2   <- getRemoteIdFromDb(aid)
      fk2     = awsConfig.obsFileKey(pid, rid2)
      _      <- assertS3(fk2, file2.content)
      _      <- assertS3NotThere(fileKey)
      _      <- deleteAttachment(service, pid, aid).expectOk
      _      <- assertS3NotThere(fk2)
      _      <- assertAttachmentsGql(service, pid)
    } yield ()
  }

  test("update single attachment metadata: description") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(service, pid, file1A).toAttachmentId
      newDesc = "New description"
      newTa   = file1A.copy(description = newDesc.some)
      _      <- updateAttachmentsGql(pi,
                                     pid,
                                     WHERE = s"""{ id: { EQ: "$aid"}}""",
                                     SET = s"""{ description: "$newDesc" }""",
                                     (aid, newTa)
                )
    } yield ()
  }

  test("update single attachment metadata: unset description") {
    for {
      pid  <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid, file1A).toAttachmentId
      newTa = file1A.copy(description = none)
      _    <- updateAttachmentsGql(pi,
                                   pid,
                                   WHERE = s"""{ id: { EQ: "$aid"}}""",
                                   SET = """{ description: null }""",
                                   (aid, newTa)
              )
    } yield ()
  }

  test("update single attachment metadata: checked") {
    for {
      pid  <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid, file1A).toAttachmentId
      newTa = file1A.copy(checked = true)
      _    <- updateAttachmentsGql(pi,
                                   pid,
                                   WHERE = s"""{ id: { EQ: "$aid"}}""",
                                   SET = """{ checked: true }""",
                                   (aid, newTa)
              )
    } yield ()
  }

  test("bulk update attachments metadata: by name") {
    for {
      pid    <- createProgramAs(pi)
      aid1   <- insertAttachment(pi, pid, file1A).toAttachmentId
      aid2   <- insertAttachment(pi, pid, file2).toAttachmentId
      aid3   <- insertAttachment(pi, pid, file3).toAttachmentId
      newDesc = "updated"
      newTa1  = file1A.copy(description = newDesc.some, checked = true)
      newTa2  = file2.copy(description = newDesc.some, checked = true)
      _      <- updateAttachmentsGql(pi,
                                     pid,
                                     WHERE = s"""{ fileName: { LIKE: "file%"}}""",
                                     SET = s"""{ checked: true, description: "$newDesc" }""",
                                     (aid1, newTa1),
                                     (aid2, newTa2)
                )
    } yield ()
  }

  test("bulk update attachments metadata: by ids") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, file1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, file2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, file3).toAttachmentId
      newTa1 = file1A.copy(description = none, checked = true)
      newTa3 = file3.copy(description = none, checked = true)
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ id: { IN: ["$aid1", "$aid3"]}}""",
                                    SET = """{ checked: true, description: null }""",
                                    (aid1, newTa1),
                                    (aid3, newTa3)
               )
    } yield ()
  }

  test("update attachments metadata: by checked") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, file1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, file2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, file3).toAttachmentId
      ta3c   = file3.copy(checked = true)
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ id: { EQ: "$aid3"}}""",
                                    SET = s"""{ checked: true }""",
                                    (aid3, ta3c)
               )
      newTa3 = ta3c.copy(description = "Verified".some)
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ checked: true }""",
                                    SET = """{ description: "Verified" }""",
                                    (aid3, newTa3)
               )
    } yield ()
  }

  test("update attachments metadata: by decription") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, file1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, file2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, file3).toAttachmentId
      newTa2 = file2.copy(description = none)
      newTa3 = file3.copy(description = none)
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ description: { NLIKE: "%script%" }}""",
                                    SET = """{ description: null }""",
                                    (aid2, newTa2),
                                    (aid3, newTa3)
               )
    } yield ()
  }

  test("update attachments metadata: by null decription") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, file1B).toAttachmentId
      aid2  <- insertAttachment(pi, pid, file2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, file3).toAttachmentId
      newTa1 = file1B.copy(description = "No longer null!".some)
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ description: { IS_NULL: true }}""",
                                    SET = """{ description: "No longer null!" }""",
                                    (aid1, newTa1)
               )
    } yield ()
  }

  test("update attachments metadata: by attachment type") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, file1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, file2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, file3).toAttachmentId
      newTa2 = file2.copy(description = "Found".some)
      newTa3 = file3.copy(description = "Found".some)
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ attachmentType: { EQ: MOS_MASK }}""",
                                    SET = """{ description: "Found" }""",
                                    (aid2, newTa2),
                                    (aid3, newTa3)
               )
    } yield ()
  }

  test("update attachments metadata: no matches") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, file1A).toAttachmentId
      _     <- updateAttachmentsGql(pi,
                                    pid,
                                    WHERE = s"""{ id: { NEQ: "$aid1" }}""",
                                    SET = """{ description: "Found" }"""
               )
    } yield ()
  }
}
