// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*
import Services.Syntax.*
import cats.effect.std.UUIDGen
import java.util.UUID

trait ObsAttachmentFileService[F[_]] {
  import AttachmentFileService.AttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  )(using Transaction[F]): F[Either[AttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def insertAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  )(using Transaction[F]): F[ObsAttachment.Id]

  def updateAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentId:   ObsAttachment.Id,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  )(using Transaction[F]): F[Unit]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id)(using Transaction[F]): F[Unit]
  
  def getPresignedUrl(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id)(using Transaction[F]): F[String]
}

object ObsAttachmentFileService extends AttachmentFileService {
  import AttachmentFileService.AttachmentException
  import AttachmentException.*

  def instantiate[F[_]: Concurrent: Trace: UUIDGen](
    s3FileSvc: S3FileService[F],
  )(using Services[F]): ObsAttachmentFileService[F] = {

    def checkAttachmentType(attachmentType: Tag): F[Unit] = {
      val af   = Statements.attachmentTypeExists(attachmentType)
      val stmt = sql"Select ${af.fragment}".query(bool)
      session
        .prepareR(stmt)
        .use { pg =>
          pg.unique(af.argument)
        }
        .flatMap(isValid =>
          if (isValid) Concurrent[F].unit
          else Concurrent[F].raiseError(InvalidRequest("Invalid attachment type"))
        )
    }

    def insertAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       FileName,
      description:    Option[NonEmptyString],
      fileSize:       Long, 
      remotePath:     NonEmptyString
    ): F[ObsAttachment.Id] =
      Trace[F].span("insertObsAttachment") {
        val af   = 
          Statements.insertAttachment(user, programId, attachmentType, fileName.value, description, fileSize, remotePath)
        val stmt = af.fragment.query(obs_attachment_id)
        session.prepareR(stmt)
          .use(pg =>
            pg.unique(af.argument)
              .recoverWith {
                case SqlState.UniqueViolation(_) => 
                  Concurrent[F].raiseError(InvalidRequest("Duplicate file name"))
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
      remotePath:     NonEmptyString
    ): F[Unit] =
      Trace[F].span("updateObsAttachment") {
        val af   = 
          Statements.updateAttachment(user, programId, attachmentId, fileName.value, description, fileSize, remotePath)
        val stmt = af.fragment.query(bool)
        session.prepareR(stmt)
          .use(pg =>
            pg.unique(af.argument)
              .flatMap(b =>
                if (b) Concurrent[F].unit 
                else Concurrent[F].raiseError(FileNotFound)
              )
              .recoverWith {
                case SqlState.UniqueViolation(_) => 
                  Concurrent[F].raiseError(InvalidRequest("Duplicate file name"))
              }
           )
      }

    def getAttachmentRemotePathFromDB(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): F[NonEmptyString] =
      Trace[F].span("getObsAttachmentRemoteIdFromDB") {
        val af   = Statements.getAttachmentRemotePath(user, programId, attachmentId)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => Concurrent[F].raiseError(FileNotFound)
                case Some(s) => Concurrent[F].pure(s)
              }
          )
      }

    def deleteAttachmentFromDB(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): F[NonEmptyString] =
      Trace[F].span("deleteObsAttachmentFromDB") {
        val af   = Statements.deleteAttachment(user, programId, attachmentId)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => Concurrent[F].raiseError(FileNotFound)
                case Some(s) => Concurrent[F].pure(s)
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
              case None    => Concurrent[F].unit
              case Some(_) => Concurrent[F].raiseError(InvalidRequest("Duplicate file name"))
            }
        )
    }

    def filePath(programId: Program.Id, remoteId: UUID, fileName: NonEmptyString) =
      s3FileSvc.filePath(programId, remoteId, fileName)

    new ObsAttachmentFileService[F] {

      def getAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
      )(using Transaction[F]): F[Either[AttachmentException, Stream[F, Byte]]] =
        session.transaction
          .use(_ =>
            for {
              _    <- checkAccess(session, user, programId)
              path <- getAttachmentRemotePathFromDB(user, programId, attachmentId)
            } yield path
          )
          .flatMap { rpath =>
            s3FileSvc.verifyAndGet(rpath).map(_.asRight)
          }
          .recover {
            case e: AttachmentException => e.asLeft
          }

      def insertAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag,
        fileName:       String,
        description:    Option[NonEmptyString],
        data:           Stream[F, Byte]
      )(using Transaction[F]): F[ObsAttachment.Id] =
        FileName
          .fromString(fileName)
          .fold(
            e  => Concurrent[F].raiseError(e),
            fn =>
              session.transaction
                .use(_ =>
                  for {
                    _ <- checkAccess(session, user, programId)
                    _ <- checkAttachmentType(attachmentType)
                    _ <- checkForDuplicateName(programId, fn, none)
                  } yield ()
                )
                .flatMap( _ =>
                  for {
                    uuid   <- UUIDGen[F].randomUUID
                    path    = filePath(programId, uuid, fn.value)
                    size   <- s3FileSvc.upload(path, data)
                    result <- insertAttachmentInDB(user, programId, attachmentType, fn, description, size, path)
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
      )(using Transaction[F]): F[Unit] = 
        FileName
          .fromString(fileName)
          .fold(
            e  => Concurrent[F].raiseError(e),
            fn =>
              session.transaction
                .use(_ =>
                  for {
                    _    <- checkAccess(session, user, programId)
                    _    <- checkForDuplicateName(programId, fn, attachmentId.some)
                    path <- getAttachmentRemotePathFromDB(user, programId, attachmentId)
                  } yield path
                )
                .flatMap(oldPath =>
                  for {
                    uuid   <- UUIDGen[F].randomUUID
                    newPath = filePath(programId, uuid, fn.value)
                    size   <- s3FileSvc.upload(newPath, data)
                    _      <- updateAttachmentInDB(user, programId, attachmentId, fn, description, size, newPath)
                    _      <- s3FileSvc.delete(oldPath)
                  } yield ()
                )
          )

      def deleteAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
      )(using Transaction[F]): F[Unit] =
        session.transaction
          .use(_ =>
            for {
              _    <- checkAccess(session, user, programId)
              path <- deleteAttachmentFromDB(user, programId, attachmentId)
            } yield path
          )
          .flatMap(remotePath =>
            // We'll trap errors from the remote delete because, although not ideal, we don't 
            // care so much if an orphan file is left on S3. The error will have been put in the trace.
            s3FileSvc.delete(remotePath).handleError{ case _ => () }
          )

      def getPresignedUrl(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id)(using Transaction[F]): F[String] = 
        session.transaction
          .use(_ =>
            for {
              _    <- checkAccess(session, user, programId)
              path <- getAttachmentRemotePathFromDB(user, programId, attachmentId)
            } yield path
          )
          .flatMap { remotePath =>
            s3FileSvc.presignedUrl(remotePath)
          }
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
      remotePath:     NonEmptyString
    ): AppliedFragment =
      sql"""
        INSERT INTO t_obs_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_description,
          c_file_size,
          c_remote_path
        ) 
        SELECT 
          $program_id,
          $tag,
          $text_nonempty,
          ${text_nonempty.opt},
          $int8,
          $text_nonempty
      """.apply(programId, attachmentType, fileName, description, fileSize, remotePath) |+|
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
      remotePath:     NonEmptyString
    ): AppliedFragment =
      sql"""
        UPDATE t_obs_attachment
        SET c_file_name   = $text_nonempty,
            c_description = ${text_nonempty.opt},
            c_checked     = false,
            c_file_size   = $int8,
            c_remote_path = $text_nonempty
        WHERE c_program_id = $program_id AND c_obs_attachment_id = $obs_attachment_id
      """.apply(fileName, description, fileSize, remotePath, programId, attachmentId) |+|
        ProgramService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING true"

    def getAttachmentRemotePath(
      user:         User,
      programId:    Program.Id,
      attachmentId: ObsAttachment.Id
    ): AppliedFragment =
      sql"""
        SELECT c_remote_path
        FROM t_obs_attachment
        WHERE c_program_id = $program_id AND c_obs_attachment_id = $obs_attachment_id
      """.apply(programId, attachmentId) |+|
        ProgramService.Statements.andWhereUserAccess(user, programId)

    def checkForDuplicateName(
      programId:     Program.Id,
      fileName:      NonEmptyString,
      oAttachmentId: Option[ObsAttachment.Id]
    ): AppliedFragment =
      sql"""
        SELECT true
        FROM t_obs_attachment
        WHERE c_program_id = $program_id AND c_file_name = $text_nonempty
      """.apply(programId, fileName) |+|
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
        ProgramService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING c_remote_path"

    def attachmentTypeExists(attachmentType: Tag): AppliedFragment =
      sql"""
        EXISTS (select c_tag from t_obs_attachment_type where c_tag = $tag)
      """.apply(attachmentType)
  }
}
