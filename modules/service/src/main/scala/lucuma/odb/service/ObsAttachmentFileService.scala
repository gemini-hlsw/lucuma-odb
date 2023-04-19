// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.io.file.Path
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

import java.util.UUID

trait ObsAttachmentFileService[F[_]] {
  import ObsAttachmentFileService.ObsAttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  ): F[Either[ObsAttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def insertAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[ObsAttachment.Id]

  def updateAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentId:   ObsAttachment.Id,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[Unit]

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
      case GuestUser(_) => Async[F].raiseError(Forbidden)
      case _            =>
        ProgramService
          .fromSessionAndUser(session, user)
          .userHasAccess(programId)
          .flatMap(hasAccess =>
            if (hasAccess) Async[F].unit
            else Async[F].raiseError(Forbidden)
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
          else Async[F].raiseError(InvalidRequest("Invalid attachment type"))
        )
    }

    def insertAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       FileName,
      description:    Option[NonEmptyString],
      fileSize:       Long, 
      remoteId:       UUID
    ): F[ObsAttachment.Id] =
      Trace[F].span("insertAttachment") {
        val af   = 
          Statements.insertAttachment(user, programId, attachmentType, fileName.value, description, fileSize, remoteId)
        val stmt = af.fragment.query(obs_attachment_id)
        session.prepareR(stmt)
          .use(pg =>
            pg.unique(af.argument)
              .recoverWith {
                case SqlState.UniqueViolation(_) => 
                  Async[F].raiseError(InvalidRequest("Duplicate file name"))
              }
           )
      }

    def updateAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentId:   ObsAttachment.Id,
      fileName:       FileName,
      description:    Option[NonEmptyString],
      fileSize:       Long, 
      remoteId:       UUID
    ): F[Unit] =
      Trace[F].span("updateAttachment") {
        val af   = 
          Statements.updateAttachment(user, programId, attachmentId, fileName.value, description, fileSize, remoteId)
        val stmt = af.fragment.command
        session.prepareR(stmt)
          .use(pg =>
            pg.execute(af.argument)
              .void
              .recoverWith {
                case SqlState.UniqueViolation(_) => 
                  Async[F].raiseError(InvalidRequest("Duplicate file name"))
              }
           )
      }

    def getAttachmentRemoteIdFromDB(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): F[UUID] =
      Trace[F].span("getAttachmentRemoteIdFromDB") {
        val af   = Statements.getAttachmentRemoteId(user, programId, attachmentId)
        val stmt = af.fragment.query(uuid)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => Async[F].raiseError(FileNotFound)
                case Some(s) => Async[F].pure(s)
              }
          )
      }

    def deleteAttachmentFromDB(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): F[UUID] =
      Trace[F].span("deleteAttachmentFromDB") {
        val af   = Statements.deleteAttachment(user, programId, attachmentId)
        val stmt = af.fragment.query(uuid)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => Async[F].raiseError(FileNotFound)
                case Some(s) => Async[F].pure(s)
              }
          )
      }

    def checkForDuplicateName(programId: Program.Id, fileName: FileName, oaid: Option[ObsAttachment.Id]): F[Unit] = {
      val af   = Statements.checkForDuplicateName(programId, fileName.value, oaid)
      val stmt = af.fragment.query(bool)

      session
        .prepareR(stmt)
        .use(pg =>
          pg.option(af.argument)
            .flatMap {
              case None    => Async[F].unit
              case Some(_) => Async[F].raiseError(InvalidRequest("Duplicate file name"))
            }
        )
    }

    new ObsAttachmentFileService[F] {

      def getAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
      ): F[Either[ObsAttachmentException, Stream[F, Byte]]] =
        session.transaction
          .use(_ =>
            for {
              _        <- checkAccess(user, programId)
              remoteId <- getAttachmentRemoteIdFromDB(user, programId, attachmentId)
            } yield remoteId
          )
          .flatMap { rid =>
            s3FileSvc.verifyAndGet(programId, rid).map(_.asRight)
          }
          .recover {
            case e: ObsAttachmentException => e.asLeft
          }

      def insertAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag,
        fileName:       String,
        description:    Option[NonEmptyString],
        data:           Stream[F, Byte]
      ): F[ObsAttachment.Id] =
        FileName
          .fromString(fileName)
          .fold(
            e  => Async[F].raiseError(e),
            fn =>
              session.transaction
                .use(_ =>
                  for {
                    _ <- checkAccess(user, programId)
                    _ <- checkAttachmentType(attachmentType)
                    _ <- checkForDuplicateName(programId, fn, none)
                  } yield ()
                )
                .flatMap( _ =>
                  for {
                    rid    <- Async[F].delay(UUID.randomUUID())
                    size   <- s3FileSvc.upload(programId, rid, data)
                    result <- insertAttachmentInDB(user, programId, attachmentType, fn, description, size, rid)
                  } yield result
                )
          )

      def updateAttachment(
        user: User,
        programId: Program.Id,
        attachmentId: ObsAttachment.Id,
        fileName: String,
        description: Option[NonEmptyString],
        data: Stream[F, Byte]
      ): F[Unit] = 
        FileName
          .fromString(fileName)
          .fold(
            e  => Async[F].raiseError(e),
            fn =>
              session.transaction
                .use(_ =>
                  for {
                    _   <- checkAccess(user, programId)
                    _   <- checkForDuplicateName(programId, fn, attachmentId.some)
                    rid <- getAttachmentRemoteIdFromDB(user, programId, attachmentId)
                  } yield rid
                )
                .flatMap(oldRid =>
                  for {
                    newRid <- Async[F].delay(UUID.randomUUID())
                    size   <- s3FileSvc.upload(programId, newRid, data)
                    _      <- updateAttachmentInDB(user, programId, attachmentId, fn, description, size, newRid)
                    _      <- s3FileSvc.delete(programId, oldRid)
                  } yield ()
                )
          )

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

    def insertAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       NonEmptyString,
      description:    Option[NonEmptyString],
      fileSize:       Long,
      remoteId:       UUID
    ): AppliedFragment =
      sql"""
        INSERT INTO t_obs_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_description,
          c_file_size,
          c_remote_id
        ) 
        SELECT 
          $program_id,
          $tag,
          $text_nonempty,
          ${text_nonempty.opt},
          $int8,
          $uuid
      """.apply(programId ~ attachmentType ~ fileName ~ description ~ fileSize ~ remoteId) |+|
        ProgramService.Statements.whereUserAccess(user, programId) |+|
        void"""
          RETURNING c_obs_attachment_id
        """

    def updateAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentId:   ObsAttachment.Id,
      fileName:       NonEmptyString,
      description:    Option[NonEmptyString],
      fileSize:       Long,
      remoteId:       UUID
    ): AppliedFragment =
      sql"""
        UPDATE t_obs_attachment
        SET c_file_name   = $text_nonempty,
            c_description = ${text_nonempty.opt},
            c_checked     = false,
            c_file_size   = $int8,
            c_remote_id   = $uuid
        WHERE c_program_id = $program_id AND c_obs_attachment_id = $obs_attachment_id
      """.apply(fileName ~ description ~ fileSize ~ remoteId ~ programId ~ attachmentId) |+|
        accessFrag(user, programId)

    def getAttachmentRemoteId(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): AppliedFragment =
      sql"""
        SELECT c_remote_id
        FROM t_obs_attachment
        WHERE c_program_id = $program_id AND c_obs_attachment_id = $obs_attachment_id
      """.apply(programId ~ attachmentId) |+|
        accessFrag(user, programId)

    def checkForDuplicateName(
      programId:     Program.Id,
      fileName:      NonEmptyString,
      oAttachmentId: Option[ObsAttachment.Id]
    ): AppliedFragment =
      sql"""
        SELECT true
        FROM t_obs_attachment
        WHERE c_program_id = $program_id AND c_file_name = $text_nonempty
      """.apply(programId ~ fileName) |+|
        oAttachmentId.foldMap(aid =>
          sql"""
            AND c_obs_attachment_id != $obs_attachment_id
          """.apply(aid)
        )

    // returns the UUID for the remote file id
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
        void"RETURNING c_remote_id"

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
