// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.FMain
import lucuma.odb.util.Codecs.*
import natchez.Trace.Implicits.noop
import org.http4s.*
import skunk.*
import skunk.syntax.all.*

class obsAttachments extends ObsAttachmentsSuite {

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
              $ObsAttachmentsGraph
            }
          }
        """,
      expected = Right(
        Json.obj(
          "program" -> expectedAttachments(expectedTas.toList)
        )
      )
    )

  def updateAttachmentsGql(
    user:        User,
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
              WHERE: """ + WHERE + """
              SET: """ + SET + s"""
            }
          ) {
            $ObsAttachmentsGraph
          }
        }
      """,
      expected = Right(
        Json.obj(
          "updateObsAttachments" -> expectedAttachments(expectedTas.toList)
        )
      )
    )

  def getRemotePathFromDb(aid: ObsAttachment.Id): IO[NonEmptyString] = {
    val query = sql"select c_remote_path from t_obs_attachment where c_obs_attachment_id = $obs_attachment_id".query(text_nonempty)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(aid))
    )
  }

  val mosMask1A         = TestAttachment("file1.fits", "mos_mask", "A description".some, "Hopeful")
  val mosMask1B         = TestAttachment("file1.fits", "mos_mask", None, "New contents")
  val mosMask2          = TestAttachment("file2.fits", "mos_mask", "Masked".some, "Zorro")
  val finderPNG         = TestAttachment("different.png", "finder", "Unmatching file name".some, "Something different")
  val finderJPG         = TestAttachment("finder.jpg", "finder", "jpg file".some, "A finder JPG file")
  val preImaging        = TestAttachment("pi.fits", "pre_imaging", none, "A pre imaging file")
  val emptyFile         = TestAttachment("file1.fits", "mos_mask", "Thing".some, "")
  val missingType       = TestAttachment("file1.fits", "", "Whatever".some, "It'll never make it")
  val invalidType       = TestAttachment("file1.fits", "NotAType", none, "It'll never make it")
  val missingFileName   = TestAttachment("", "finder", none, "Doesn't matter")
  val fileWithPath      = TestAttachment("this/file.jpg", "finder", none, "Doesn't matter")
  val missingFinderExt  = TestAttachment("file", "finder", none, "Doesn't matter")
  val emptyMosMaskExt   = TestAttachment("file.", "mos_mask", none, "Doesn't matter")
  val invalidFinderExt  = TestAttachment("file.fits", "finder", none, "Doesn't matter")
  val invalidMosMaskExt = TestAttachment("file.png", "mos_mask", none, "Doesn't matter")
  val invalidPreImgExt  = TestAttachment("file.jpg", "pre_imaging", none, "Doesn't matter")

  val invalidFitsMsg = "Invalid file. Must be a FITS file."
  val invalidFinderMsg = "Invalid file. Must be one of: JPEG, JPG, PNG"

  test("successful insert, download and delete") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- getAttachment(pi, pid, aid).expectBody(mosMask1A.content)
      _      <- deleteAttachment(pi, pid, aid).expectOk
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid)
      _      <- getAttachment(pi, pid, aid).withExpectation(Status.NotFound)
    } yield ()
  }

  test("can download via presigned url") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      url    <- getPresignedUrl(pi, pid, aid).toNonEmptyString
      _      <- getViaPresignedUrl(url).expectBody(mosMask1A.content)
    } yield ()
  }

  test("successful insert, download and delete of multiple files") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      pth1 <- getRemotePathFromDb(aid1)
      fk1   = awsConfig.fileKey(pth1)
      _    <- assertS3(fk1, mosMask1A.content)
      aid2 <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      pth2 <- getRemotePathFromDb(aid2)
      fk2   = awsConfig.fileKey(pth2)
      _    <- assertS3(fk2, mosMask2.content)
      _    <- assertAttachmentsGql(pi, pid, (aid2, mosMask2), (aid1, mosMask1A))
      _    <- getAttachment(pi, pid, aid1).expectBody(mosMask1A.content)
      _    <- getAttachment(pi, pid, aid2).expectBody(mosMask2.content)
      _    <- deleteAttachment(pi, pid, aid1).expectOk
      _    <- getAttachment(pi, pid, aid1).withExpectation(Status.NotFound)
      _    <- getAttachment(pi, pid, aid2).expectBody(mosMask2.content)
      _    <- assertS3NotThere(fk1)
      _    <- assertS3(fk2, mosMask2.content)
      _    <- assertAttachmentsGql(pi, pid, (aid2, mosMask2))
    } yield ()
  }

  test("successful insert of other file types/extensions") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, finderJPG).toAttachmentId
      aid2 <- insertAttachment(pi, pid, preImaging).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, finderJPG), (aid2, preImaging))
    } yield ()
  }

  test("update with different name is successful") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- getAttachment(pi, pid, aid).expectBody(mosMask1A.content)
      _      <- updateAttachment(pi, pid, aid, mosMask2).expectOk
      path2  <- getRemotePathFromDb(aid)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, mosMask2.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask2.copy(attachmentType = mosMask1A.attachmentType)))
      _      <- getAttachment(pi, pid, aid).expectBody(mosMask2.content)
    } yield ()
  }

  test("update with same name is successful") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- getAttachment(pi, pid, aid).expectBody(mosMask1A.content)
      _      <- updateAttachment(pi, pid, aid, mosMask1B).expectOk
      path2  <- getRemotePathFromDb(aid)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, mosMask1B.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1B.copy(attachmentType = mosMask1A.attachmentType)))
      _      <- getAttachment(pi, pid, aid).expectBody(mosMask1B.content)
    } yield ()
  }

  test("update of non-existent attachment is NotFound") {
    for {
      pid <- createProgramAs(pi)
      aid  = ObsAttachment.Id.fromLong(100L).get
      _   <- updateAttachment(pi, pid, aid, mosMask1A).withExpectation(Status.NotFound)
    } yield ()
  }

  test("insert with duplicate name is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- insertAttachment(pi, pid, mosMask1B).withExpectation(Status.BadRequest, "Duplicate file name")
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- getAttachment(pi, pid, aid).expectBody(mosMask1A.content)
    } yield ()
  }

  test("update with duplicate name is a BadRequest") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      aid2   <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      path2  <- getRemotePathFromDb(aid2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A), (aid2, mosMask2))
      _      <- updateAttachment(pi, pid, aid2, mosMask1B).withExpectation(Status.BadRequest, "Duplicate file name")
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A), (aid2, mosMask2))
      _      <- getAttachment(pi, pid, aid2).expectBody(mosMask2.content)
    } yield ()
  }

  test("empty file insert is a BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, emptyFile).withExpectation(Status.BadRequest, "File cannot be empty")
    } yield ()
  }

  test("empty file update is a BadRequest, doesn't overwrite previous") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- updateAttachment(pi, pid, aid, emptyFile).withExpectation(Status.BadRequest, "File cannot be empty")
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
    } yield ()
  }

  test("invalid attachment type insert is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, invalidType).withExpectation(Status.BadRequest, "Invalid attachment type")
    } yield ()
  }

  test("empty attachment type insert is BadRequest") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, missingType).withExpectation(Status.BadRequest, "Invalid attachment type")
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
      aid <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
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
      aid <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, fileWithPath).withExpectation(Status.BadRequest, "File name cannot include a path")
    } yield ()
  }

  test("file name with missing extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, missingFinderExt).withExpectation(Status.BadRequest, invalidFinderMsg)
    } yield ()
  }

  test("file name with missing extension update fails") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, finderJPG).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, missingFinderExt).withExpectation(Status.BadRequest, invalidFinderMsg)
    } yield ()
  }

  test("file name with empty extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, emptyMosMaskExt).withExpectation(Status.BadRequest, invalidFitsMsg)
    } yield ()
  }

  test("file name with empty extension update fails") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, emptyMosMaskExt).withExpectation(Status.BadRequest, invalidFitsMsg)
    } yield ()
  }

  test("finder file with invalid extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, invalidFinderExt).withExpectation(Status.BadRequest, invalidFinderMsg)
    } yield ()
  }

  test("finder file with invalid extension update fails") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, finderJPG).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, missingFinderExt).withExpectation(Status.BadRequest, invalidFinderMsg)
    } yield ()
  }

  test("mos_mask file with invalid extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, invalidMosMaskExt).withExpectation(Status.BadRequest, invalidFitsMsg)
    } yield ()
  }

  test("mos_mask file with invalid extension update fails") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, invalidMosMaskExt).withExpectation(Status.BadRequest, invalidFitsMsg)
    } yield ()
  }

  test("pre_imaging file with invalid extension insert fails") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, invalidPreImgExt).withExpectation(Status.BadRequest, invalidFitsMsg)
    } yield ()
  }

  test("pre_imaging file with invalid extension update fails") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, preImaging).toAttachmentId
      _   <- updateAttachment(pi, pid, aid, invalidPreImgExt).withExpectation(Status.BadRequest, invalidFitsMsg)
    } yield ()
  }

  test("pi can update multiple programs") {
    for {
      pid1 <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid1, preImaging).toAttachmentId
      pid2 <- createProgramAs(pi)
      aid2 <- insertAttachment(pi, pid2, preImaging).toAttachmentId
      ta3c  = preImaging.copy(checked = true)
      _    <- updateAttachmentsGql(pi,
                                   WHERE = s"""{ id: { IN: [ "$aid1", "$aid2" ] } }""",
                                   SET = s"""{ checked: true }""",
                                   (aid1, ta3c), (aid2, ta3c)
              )
    } yield ()
  }

  test("pi can only insert to their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      _    <- insertAttachment(pi, pid2, mosMask1A).withExpectation(Status.Forbidden)
      _    <- insertAttachment(pi2, pid1, mosMask1A).withExpectation(Status.Forbidden)
      _    <- assertAttachmentsGql(pi, pid1)
      _    <- assertAttachmentsGql(pi2, pid2)
    } yield ()
  }

  test("pi can only update their own programs") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      _   <- updateAttachment(pi2, pid, aid, mosMask2).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only see their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      aid1 <- insertAttachment(pi, pid1, mosMask1A).toAttachmentId
      aid2 <- insertAttachment(pi2, pid2, mosMask1B).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid1, (aid1, mosMask1A))
      _    <- assertAttachmentsGql(pi2, pid2, (aid2, mosMask1B))
    } yield ()
  }

  test("pi can only download from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      aid1 <- insertAttachment(pi, pid1, mosMask1A).toAttachmentId
      pth1 <- getRemotePathFromDb(aid1)
      fk1   = awsConfig.fileKey(pth1)
      _    <- assertS3(fk1, mosMask1A.content)
      _    <- assertAttachmentsGql(pi, pid1, (aid1, mosMask1A))
      aid2 <- insertAttachment(pi2, pid2, mosMask2).toAttachmentId
      pth2 <- getRemotePathFromDb(aid2)
      fk2   = awsConfig.fileKey(pth2)
      _    <- assertS3(fk2, mosMask2.content)
      _    <- assertAttachmentsGql(pi2, pid2, (aid2, mosMask2))
      _    <- getAttachment(pi, pid2, aid2).withExpectation(Status.Forbidden)
      _    <- getAttachment(pi2, pid1, aid1).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("pi can only delete from their own programs") {
    for {
      pid1 <- createProgramAs(pi)
      pid2 <- createProgramAs(pi2)
      aid1 <- insertAttachment(pi, pid1, mosMask1A).toAttachmentId
      pth1 <- getRemotePathFromDb(aid1)
      fk1   = awsConfig.fileKey(pth1)
      _    <- assertS3(fk1, mosMask1A.content)
      _    <- assertAttachmentsGql(pi, pid1, (aid1, mosMask1A))
      aid2 <- insertAttachment(pi2, pid2, mosMask2).toAttachmentId
      pth2 <- getRemotePathFromDb(aid2)
      fk2   = awsConfig.fileKey(pth2)
      _    <- assertS3(fk2, mosMask2.content)
      _    <- assertAttachmentsGql(pi2, pid2, (aid2, mosMask2))
      _    <- deleteAttachment(pi, pid2, aid2).withExpectation(Status.Forbidden)
      _    <- deleteAttachment(pi2, pid1, aid1).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("service user can manage any program's attachments") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(service, pid, mosMask1A).toAttachmentId
      path   <- getRemotePathFromDb(aid)
      fileKey = awsConfig.fileKey(path)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(service, pid, (aid, mosMask1A))
      _      <- getAttachment(service, pid, aid).expectBody(mosMask1A.content)
      _      <- updateAttachment(service, pid, aid, mosMask2).expectOk
      path2  <- getRemotePathFromDb(aid)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, mosMask2.content)
      _      <- assertS3NotThere(fileKey)
      _      <- deleteAttachment(service, pid, aid).expectOk
      _      <- assertS3NotThere(fk2)
      _      <- assertAttachmentsGql(service, pid)
    } yield ()
  }

  test("update single attachment metadata: description") {
    for {
      pid    <- createProgramAs(pi)
      aid    <- insertAttachment(service, pid, mosMask1A).toAttachmentId
      newDesc = "New description"
      newTa   = mosMask1A.copy(description = newDesc.some)
      _      <- updateAttachmentsGql(pi,
                                     WHERE = s"""{ id: { EQ: "$aid" }}""",
                                     SET = s"""{ description: "$newDesc" }""",
                                     (aid, newTa)
                )
    } yield ()
  }

  test("update single attachment metadata: unset description") {
    for {
      pid  <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      newTa = mosMask1A.copy(description = none)
      _    <- updateAttachmentsGql(pi,
                                   WHERE = s"""{ id: { EQ: "$aid"}}""",
                                   SET = """{ description: null }""",
                                   (aid, newTa)
              )
    } yield ()
  }

  test("update single attachment metadata: checked") {
    for {
      pid  <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      newTa = mosMask1A.copy(checked = true)
      _    <- updateAttachmentsGql(pi,
                                   WHERE = s"""{ id: { EQ: "$aid"}}""",
                                   SET = """{ checked: true }""",
                                   (aid, newTa)
              )
    } yield ()
  }

  test("bulk update attachments metadata: by name") {
    for {
      pid    <- createProgramAs(pi)
      aid1   <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      aid2   <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      aid3   <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      newDesc = "updated"
      newTa1  = mosMask1A.copy(description = newDesc.some, checked = true)
      newTa2  = mosMask2.copy(description = newDesc.some, checked = true)
      _      <- updateAttachmentsGql(pi,
                                     WHERE = s"""{ fileName: { LIKE: "file%"}, program: { id: { EQ: "$pid" } } }""",
                                     SET = s"""{ checked: true, description: "$newDesc" }""",
                                     (aid1, newTa1),
                                     (aid2, newTa2)
                )
    } yield ()
  }

  test("bulk update attachments metadata: by ids") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      newTa1 = mosMask1A.copy(description = none, checked = true)
      newTa3 = finderPNG.copy(description = none, checked = true)
      _     <- updateAttachmentsGql(pi,
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
      aid1  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      ta3c   = finderPNG.copy(checked = true)
      _     <- updateAttachmentsGql(pi,
                                    WHERE = s"""{ id: { EQ: "$aid3"}}""",
                                    SET = s"""{ checked: true }""",
                                    (aid3, ta3c)
               )
      newTa3 = ta3c.copy(description = "Verified".some)
      _     <- updateAttachmentsGql(pi,
                                    WHERE = s"""{ checked: true, program: { id: { EQ: "$pid" } } }""",
                                    SET = """{ description: "Verified" }""",
                                    (aid3, newTa3)
               )
    } yield ()
  }

  test("update attachments metadata: by decription") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      aid2  <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      newTa2 = mosMask2.copy(description = none)
      newTa3 = finderPNG.copy(description = none)
      _     <- updateAttachmentsGql(pi,
                                    WHERE = s"""{ description: { NLIKE: "%script%" }, program: { id: { EQ: "$pid" } } }""",
                                    SET = """{ description: null }""",
                                    (aid2, newTa2),
                                    (aid3, newTa3)
               )
    } yield ()
  }

  test("update attachments metadata: by null decription") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, mosMask1B).toAttachmentId
      aid2  <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      aid3  <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      newTa1 = mosMask1B.copy(description = "No longer null!".some)
      _     <- updateAttachmentsGql(pi,
                                    WHERE = s"""{ description: { IS_NULL: true }, program: { id: { EQ: "$pid" } } }""",
                                    SET = """{ description: "No longer null!" }""",
                                    (aid1, newTa1)
               )
    } yield ()
  }

  test("update attachments metadata: by attachment type") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, mosMask1B).toAttachmentId
      aid2  <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      _     <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      newTa1 = mosMask1B.copy(description = "Found".some)
      newTa2 = mosMask2.copy(description = "Found".some)
      _     <- updateAttachmentsGql(pi,
                                    WHERE = s"""{ attachmentType: { EQ: MOS_MASK }, program: { id: { EQ: "$pid" } } }""",
                                    SET = """{ description: "Found" }""",
                                    (aid1, newTa1),
                                    (aid2, newTa2)
               )
    } yield ()
  }

  test("update attachments metadata: no matches") {
    for {
      pid   <- createProgramAs(pi)
      aid1  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      _     <- updateAttachmentsGql(pi,
                                    WHERE = s"""{ id: { NEQ: "$aid1" }, program: { id: { EQ: "$pid" } } }""",
                                    SET = """{ description: "Found" }"""
               )
    } yield ()
  }
}
