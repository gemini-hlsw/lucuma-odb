// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.FMain
import lucuma.odb.service.AttachmentFileService
import lucuma.odb.util.Codecs.*
import natchez.Trace.Implicits.noop
import org.http4s.*
import skunk.*
import skunk.syntax.all.*

class attachments extends AttachmentsSuite {

  def assertAttachmentsGql(
    user:        User,
    programId:   Program.Id,
    expectedTas: (Attachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
          query {
            program(programId: "$programId") {
              $AttachmentsGraph
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
    expectedTas: (Attachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateAttachments(
            input: {
              WHERE: """ + WHERE + """
              SET: """ + SET + s"""
            }
          ) {
            $AttachmentsGraph
          }
        }
      """,
      expected = Right(
        Json.obj(
          "updateAttachments" -> expectedAttachments(expectedTas.toList)
        )
      )
    )

  def getRemotePathFromDb(aid: Attachment.Id): IO[NonEmptyString] = {
    val query = sql"select c_remote_path from t_attachment where c_attachment_id = $attachment_id".query(text_nonempty)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(aid))
    )
  }

  // TODO: science, team and custom_sed file tests
  val mosMask1A         = TestAttachment("file1.fits", "mos_mask", "A description".some, "Hopeful")
  val mosMask1B         = TestAttachment("file1.fits", "mos_mask", None, "New contents")
  val mosMask2          = TestAttachment("file2.fits", "mos_mask", "Masked".some, "Zorro")
  val finderPNG         = TestAttachment("different.png", "finder", "Unmatching file name".some, "Something different")
  val finderJPG         = TestAttachment("finder.jpg", "finder", "jpg file".some, "A finder JPG file")
  val preImaging        = TestAttachment("pi.fits", "pre_imaging", none, "A pre imaging file")
  val preImaging2       = TestAttachment("pi2.fits", "pre_imaging", none, "Another pre imaging file")
  val science1          = TestAttachment("science1.pdf", "science", none, "A science file")
  val science2          = TestAttachment("science2.pdf", "science", none, "A second science file")
  val team1             = TestAttachment("team1.pdf", "team", none, "A team file")
  val team2             = TestAttachment("team2.pdf", "team", none, "A second team file")
  val customSedSED      = TestAttachment("sed.sed", "custom_sed", "It's custom".some, "A custom SED file")
  val customSedTXT      = TestAttachment("sed.TXT", "custom_sed", "It's custom".some, "Another custom SED file")
  val customSedDAT      = TestAttachment("sed.dat", "custom_sed", "It's custom".some, "A third custom SED file")
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
      _      <- getAttachment(pi, aid).expectBody(mosMask1A.content)
      _      <- deleteAttachment(pi, aid).expectOk
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid)
      _      <- getAttachment(pi, aid).withExpectation(Status.NotFound)
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
      url    <- getPresignedUrl(pi, aid).toNonEmptyString
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
      _    <- getAttachment(pi, aid1).expectBody(mosMask1A.content)
      _    <- getAttachment(pi, aid2).expectBody(mosMask2.content)
      _    <- deleteAttachment(pi, aid1).expectOk
      _    <- getAttachment(pi, aid1).withExpectation(Status.NotFound)
      _    <- getAttachment(pi, aid2).expectBody(mosMask2.content)
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

  test("can have multiple mos masks") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask1A).toAttachmentId
      aid2 <- insertAttachment(pi, pid, mosMask2).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, mosMask1A), (aid2, mosMask2))
    } yield ()
  }

  test("can have multiple finders") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, finderJPG).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finderPNG).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, finderJPG), (aid2, finderPNG))
    } yield ()
  }

  test("can have multiple pre-imaging") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, preImaging).toAttachmentId
      aid2 <- insertAttachment(pi, pid, preImaging2).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, preImaging), (aid2, preImaging2))
    } yield ()
  }

  test("only one science allowed per program") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, science1).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, science1))
      aid2 <- insertAttachment(pi, pid, science2).withExpectation(Status.BadRequest, AttachmentFileService.duplicateTypeMsg(AttachmentType.Science))
      _    <- assertAttachmentsGql(pi, pid, (aid1, science1))
    } yield ()
  }

  test("only one team allowed per program") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, team1).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, team1))
      aid2 <- insertAttachment(pi, pid, team2).withExpectation(Status.BadRequest, AttachmentFileService.duplicateTypeMsg(AttachmentType.Team))
      _    <- assertAttachmentsGql(pi, pid, (aid1, team1))
    } yield ()
  }

  test("can have multiple custom seds") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, customSedDAT).toAttachmentId
      aid2 <- insertAttachment(pi, pid, customSedSED).toAttachmentId
      aid3 <- insertAttachment(pi, pid, customSedTXT).toAttachmentId
      _    <- assertAttachmentsGql(pi, pid, (aid1, customSedDAT), (aid2, customSedSED), (aid3, customSedTXT))
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
      _      <- getAttachment(pi, aid).expectBody(mosMask1A.content)
      _      <- updateAttachment(pi, aid, mosMask2).expectOk
      path2  <- getRemotePathFromDb(aid)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, mosMask2.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask2.copy(attachmentType = mosMask1A.attachmentType)))
      _      <- getAttachment(pi, aid).expectBody(mosMask2.content)
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
      _      <- getAttachment(pi, aid).expectBody(mosMask1A.content)
      _      <- updateAttachment(pi, aid, mosMask1B).expectOk
      path2  <- getRemotePathFromDb(aid)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, mosMask1B.content)
      _      <- assertS3NotThere(fileKey)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1B.copy(attachmentType = mosMask1A.attachmentType)))
      _      <- getAttachment(pi, aid).expectBody(mosMask1B.content)
    } yield ()
  }

  test("update of non-existent attachment is NotFound") {
    for {
      pid <- createProgramAs(pi)
      aid  = Attachment.Id.fromLong(100L).get
      _   <- updateAttachment(pi, aid, mosMask1A).withExpectation(Status.NotFound)
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
      _      <- insertAttachment(pi, pid, mosMask1B).withExpectation(Status.BadRequest, AttachmentFileService.DuplicateFileNameMsg)
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
      _      <- getAttachment(pi, aid).expectBody(mosMask1A.content)
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
      _      <- updateAttachment(pi, aid2, mosMask1B).withExpectation(Status.BadRequest, AttachmentFileService.DuplicateFileNameMsg)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A), (aid2, mosMask2))
      _      <- getAttachment(pi, aid2).expectBody(mosMask2.content)
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
      _      <- updateAttachment(pi, aid, emptyFile).withExpectation(Status.BadRequest, "File cannot be empty")
      _      <- assertS3(fileKey, mosMask1A.content)
      _      <- assertAttachmentsGql(pi, pid, (aid, mosMask1A))
    } yield ()
  }

  test("invalid attachment type insert is NotFound") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, invalidType).withExpectation(Status.NotFound, "Not found")
    } yield ()
  }

  test("empty attachment type insert is NotFound") {
    for {
      pid <- createProgramAs(pi)
      _   <- insertAttachment(pi, pid, missingType).withExpectation(Status.NotFound, "Not found")
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
      _   <- updateAttachment(pi, aid, missingFileName).withExpectation(Status.BadRequest, "File name is required")
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
      _   <- updateAttachment(pi, aid, fileWithPath).withExpectation(Status.BadRequest, "File name cannot include a path")
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
      _   <- updateAttachment(pi, aid, missingFinderExt).withExpectation(Status.BadRequest, invalidFinderMsg)
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
      _   <- updateAttachment(pi, aid, emptyMosMaskExt).withExpectation(Status.BadRequest, invalidFitsMsg)
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
      _   <- updateAttachment(pi, aid, missingFinderExt).withExpectation(Status.BadRequest, invalidFinderMsg)
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
      _   <- updateAttachment(pi, aid, invalidMosMaskExt).withExpectation(Status.BadRequest, invalidFitsMsg)
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
      _   <- updateAttachment(pi, aid, invalidPreImgExt).withExpectation(Status.BadRequest, invalidFitsMsg)
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
      _   <- updateAttachment(pi2, aid, mosMask2).withExpectation(Status.NotFound)
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
      _    <- getAttachment(pi, aid2).withExpectation(Status.NotFound)
      _    <- getAttachment(pi2, aid1).withExpectation(Status.NotFound)
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
      _    <- deleteAttachment(pi, aid2).withExpectation(Status.NotFound)
      _    <- deleteAttachment(pi2, aid1).withExpectation(Status.NotFound)
    } yield ()
  }

  test("CoI can insert attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- linkUserAs(pi, puid, pi2.id)
      _    <- insertAttachment(pi2, pid, preImaging).toAttachmentId
    } yield ()
  }

  test("CoI can update attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId // PI inserts
      _    <- updateAttachment(pi2, aid, mosMask2).expectOk       // CoI updates
    } yield ()
  }

  test("CoI can delete attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId // PI inserts
      _    <- deleteAttachment(pi2, aid).expectOk                 // CoI deletes
    } yield ()
  }

  test("CoI can download attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId   // PI inserts
      _    <- getAttachment(pi2, aid).expectBody(mosMask1A.content) // CoI downloads
    } yield ()
  }

  test("CoI can download attachments via presigned url") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.Coi)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId // PI inserts
      url  <- getPresignedUrl(pi2, aid).toNonEmptyString          // CoI downloads
      _    <- getViaPresignedUrl(url).expectBody(mosMask1A.content)
    } yield ()
  }

  test("Readonly CoI cannot insert attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.CoiRO)
      _    <- linkUserAs(pi, puid, pi2.id)
      _    <- insertAttachment(pi2, pid, preImaging).withExpectation(Status.Forbidden)
    } yield ()
  }

  test("Readonly CoI cannot update attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.CoiRO)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId                     // PI inserts
      _    <- updateAttachment(pi2, aid, mosMask2).withExpectation(Status.NotFound)  // CoiRO updates
    } yield ()
  }

  test("Readonly CoI cannot delete attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.CoiRO)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId          // PI inserts
      _    <- deleteAttachment(pi2, aid).withExpectation(Status.NotFound)  // CoiRO deletes
    } yield ()
  }

  test("Readonly CoI can download attachments") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.CoiRO)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId   // PI inserts
      _    <- getAttachment(pi2, aid).expectBody(mosMask1A.content) // CoiRO downloads
    } yield ()
  }

  test("Readonly CoI can download attachments via presigned url") {
    for {
      pid  <- createProgramAs(pi)
      puid <- addProgramUserAs(pi, pid, role = ProgramUserRole.CoiRO)
      _    <- linkUserAs(pi, puid, pi2.id)
      aid  <- insertAttachment(pi, pid, mosMask1A).toAttachmentId // PI inserts
      url  <- getPresignedUrl(pi2, aid).toNonEmptyString          // CoiRO downloads
      _    <- getViaPresignedUrl(url).expectBody(mosMask1A.content)
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
      _      <- getAttachment(service, aid).expectBody(mosMask1A.content)
      _      <- updateAttachment(service, aid, mosMask2).expectOk
      path2  <- getRemotePathFromDb(aid)
      _       = assertNotEquals(path, path2)
      fk2     = awsConfig.fileKey(path2)
      _      <- assertS3(fk2, mosMask2.content)
      _      <- assertS3NotThere(fileKey)
      _      <- deleteAttachment(service, aid).expectOk
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
