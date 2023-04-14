// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.io.file.Path
<<<<<<< main:modules/service/src/main/scala/lucuma/odb/service/AttachmentFileService.scala
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
=======
>>>>>>> Make current attachments specific to observations:modules/service/src/main/scala/lucuma/odb/service/ObsAttachmentFileService.scala
import lucuma.core.model.GuestUser
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.odb.Config
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs._
import natchez.Trace
import skunk._
import skunk.codec.all._
import skunk.syntax.all._

trait ObsAttachmentFileService[F[_]] {
  import ObsAttachmentFileService.ObsAttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
<<<<<<< main:modules/service/src/main/scala/lucuma/odb/service/AttachmentFileService.scala
  ): F[Either[AttachmentException, Stream[F, Byte]]]
=======
  ): F[Either[ObsAttachmentException, Stream[F, Byte]]]
>>>>>>> Make current attachments specific to observations:modules/service/src/main/scala/lucuma/odb/service/ObsAttachmentFileService.scala

  /** Uploads the file to S3 and addes it to the database */
  def uploadAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[ObsAttachment.Id]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id): F[Unit]
}

object ObsAttachmentFileService {
  sealed trait ObsAttachmentException extends Exception
  
  object ObsAttachmentException {
    case object Forbidden                   extends ObsAttachmentException
    case class InvalidRequest(message: String) extends ObsAttachmentException
    case object FileNotFound                extends ObsAttachmentException
  }

  import ObsAttachmentException._

  private type FileName = FileName.Type
  private object FileName extends NewType[NonEmptyString] {
    def fromString(name: String): Either[ObsAttachmentException, FileName] = {
      val path         = Path(name)
      val segmentCount = path.names.length
      val fileName     = NonEmptyString.from(path.fileName.toString).toOption

      fileName.fold(
        InvalidRequest("File name is required").asLeft
      )(fn =>
        if (path.names.length > 1) {
          InvalidRequest("File name cannot include a path").asLeft
        } else FileName(fn).asRight
      )
    }

    extension (fileName: FileName)
      def extName: Option[NonEmptyString] =
        NonEmptyString.from(Path(fileName.value.value).extName).toOption
  }

  def fromS3AndSession[F[_]: Async: Trace](
    s3FileSvc: S3FileService[F],
    session:   Session[F]
  ): ObsAttachmentFileService[F] = {

    // TODO: eventually will probably want to check for write access for uploading/deleting files.
    def checkAccess[A](user: User, programId: Program.Id): F[Unit] = user match {
      // guest users not allowed to upload files - at least for now.
      case GuestUser(_) => Async[F].raiseError(ObsAttachmentException.Forbidden)
      case _            =>
        ProgramService
          .fromSessionAndUser(session, user)
          .userHasAccess(programId)
          .flatMap(hasAccess =>
            if (hasAccess) Async[F].unit
            else Async[F].raiseError(ObsAttachmentException.Forbidden)
          )
    }

    def checkAttachmentType(attachmentType: Tag): F[Unit] = {
      val af   = Statements.existsAttachmentType(attachmentType)
      val stmt = sql"Select ${af.fragment}".query(bool)
      session
        .prepareR(stmt)
        .use { pg =>
          pg.unique(af.argument)
        }
        .flatMap(isValid =>
          if (isValid) Async[F].unit
          else Async[F].raiseError(ObsAttachmentException.InvalidRequest("Invalid attachment type"))
        )
    }

    def insertOrUpdateAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       FileName,
      description:    Option[NonEmptyString],
      fileSize:       Long
    ): F[ObsAttachment.Id] =
      Trace[F].span("insertOrUpdateAttachment") {
        val af   = 
          Statements.insertOrUpdateAttachment(user, programId, attachmentType, fileName.value, description, fileSize)
        val stmt = af.fragment.query(obs_attachment_id)
        session.prepareR(stmt).use(pg => pg.unique(af.argument))
      }

    def getAttachmentFileNameFromDB(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): F[NonEmptyString] =
      Trace[F].span("getAttachmentFileNameFromDB") {
        val af   = Statements.getAttachmentFileName(user, programId, attachmentId)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => Async[F].raiseError(ObsAttachmentException.FileNotFound)
                case Some(s) => Async[F].pure(s)
              }
          )
      }

    def deleteAttachmentFromDB(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): F[NonEmptyString] =
      Trace[F].span("deleteAttachmentFromDB") {
        val af   = Statements.deleteAttachment(user, programId, attachmentId)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => Async[F].raiseError(ObsAttachmentException.FileNotFound)
                case Some(s) => Async[F].pure(s)
              }
          )
      }

    new ObsAttachmentFileService[F] {

      def getAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
<<<<<<< main:modules/service/src/main/scala/lucuma/odb/service/AttachmentFileService.scala
      ): F[Either[AttachmentException, Stream[F, Byte]]] =
=======
      ): F[Either[ObsAttachmentException, Stream[F, Byte]]] =
>>>>>>> Make current attachments specific to observations:modules/service/src/main/scala/lucuma/odb/service/ObsAttachmentFileService.scala
        session.transaction
          .use(_ =>
            for {
              _        <- checkAccess(user, programId)
              fileName <- getAttachmentFileNameFromDB(user, programId, attachmentId)
            } yield fileName
          )
          .flatMap { fn =>
            s3FileSvc.verifyAndGet(programId, fn).map(_.asRight)
          }
          .recover {
            case e: ObsAttachmentException => e.asLeft
          }

      def uploadAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag,
        fileName:       String,
        description:    Option[NonEmptyString],
        data:           Stream[F, Byte]
      ): F[ObsAttachment.Id] =
        session.transaction
          .use(_ =>
            for {
              _ <- checkAccess(user, programId)
              _ <- checkAttachmentType(attachmentType)
            } yield ()
          )
          .flatMap { _ =>
            FileName
              .fromString(fileName)
              .fold(
                e => Async[F].raiseError(e),
                fn =>
                  // TODO: Validate the file extension based on attachment type
                  for {
                    size   <- s3FileSvc.upload(programId, fn.value, data)
                    result <- insertOrUpdateAttachmentInDB(user, programId, attachmentType, fn, description, size)
                  } yield result
              )
          }

      def deleteAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
      ): F[Unit] =
        session.transaction
          .use(_ =>
            for {
              _        <- checkAccess(user, programId)
              fileName <- deleteAttachmentFromDB(user, programId, attachmentId)
            } yield fileName
          )
          .flatMap(fn =>
            // We'll trap errors from the remote delete because, although not ideal, we don't 
            // care so much if an orphan file is left on S3. The error will have been put in the trace.
            s3FileSvc.delete(programId, fn).handleError{ case _ => () }
          )
    }
  }

  object Statements {

    def insertOrUpdateAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       NonEmptyString,
      description:    Option[NonEmptyString],
      fileSize:       Long
    ): AppliedFragment =
      sql"""
        INSERT INTO t_obs_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_description,
          c_file_size
        ) 
        SELECT 
          $program_id,
          $tag,
          $text_nonempty,
          ${text_nonempty.opt},
          $int8
      """.apply(programId ~ attachmentType ~ fileName ~ description ~ fileSize) |+|
        ProgramService.Statements.whereUserAccess(user, programId) |+|
        void"""
        ON CONFLICT (c_program_id, c_file_name) DO UPDATE SET
          c_attachment_type = EXCLUDED.c_attachment_type,
          c_description     = EXCLUDED.c_description,
          c_checked         = false,
          c_file_size       = EXCLUDED.c_file_size
        RETURNING c_obs_attachment_id
      """

    def getAttachmentFileName(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): AppliedFragment =
      sql"""
        SELECT c_file_name
        FROM t_obs_attachment
        WHERE c_program_id = $program_id AND c_obs_attachment_id = $obs_attachment_id
      """.apply(programId ~ attachmentId) |+|
        accessFrag(user, programId)

    // returns the file name
    def deleteAttachment(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): AppliedFragment =
      sql"""
        DELETE FROM t_obs_attachment
        WHERE c_program_id = $program_id AND c_obs_attachment_id = $obs_attachment_id
      """.apply(programId, attachmentId) |+|
        accessFrag(user, programId) |+|
        void"RETURNING c_file_name"

    def existsAttachmentType(attachmentType: Tag): AppliedFragment =
      sql"""
        EXISTS (select c_tag from t_obs_attachment_type where c_tag = $tag)
      """.apply(attachmentType)

    private def accessFrag(user: User, programId: Program.Id): AppliedFragment =
      ProgramService.Statements.existsUserAccess(user, programId).fold(AppliedFragment.empty) {
        af => void"AND " |+| af
      }
  }
}
