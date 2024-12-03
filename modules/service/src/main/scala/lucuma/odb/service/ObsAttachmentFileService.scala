// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import java.util.UUID

import Services.Syntax.*

trait ObsAttachmentFileService[F[_]] {
  import AttachmentFileService.AttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: ObsAttachment.Id
  )(using NoTransaction[F]): F[Either[AttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def insertAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  )(using NoTransaction[F]): F[ObsAttachment.Id]

  def updateAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentId:   ObsAttachment.Id,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  )(using NoTransaction[F]): F[Unit]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id)(using NoTransaction[F]): F[Unit]

  def getPresignedUrl(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id)(using NoTransaction[F]): F[String]
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

    def validateFileExtensionByType(attachmentType: Tag, fileName: FileName): F[Unit] = {
      val af = Statements.getFileExtensions(attachmentType)
      val stmt = af.fragment.query(text_nonempty)

      session
        .prepareR(stmt)
        .use(pq =>
          pq.stream(af.argument, chunkSize = 1024).compile.toList
        )
        .flatMap(allowedExtensions => checkExtension(fileName, allowedExtensions))
    }

    def validateFileExtensionById(attachmentId: ObsAttachment.Id, fileName: FileName): F[Unit] = {
      val af = Statements.getFileExtensionsById(attachmentId)
      val stmt = af.fragment.query(text_nonempty)

      session
        .prepareR(stmt)
        .use(_.stream(af.argument, chunkSize = 1024).compile.toList)
        .flatMap(allowedExtensions =>
          if (allowedExtensions.isEmpty) Concurrent[F].raiseError(FileNotFound)
          else checkExtension(fileName, allowedExtensions)
        )
    }

    def filePath(programId: Program.Id, remoteId: UUID, fileName: NonEmptyString) =
      s3FileSvc.filePath(programId, remoteId, fileName)

    new ObsAttachmentFileService[F] {

      def getAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
      )(using NoTransaction[F]): F[Either[AttachmentException, Stream[F, Byte]]] = {
        for {
          path <- services.transactionally {
            checkAccess(session, user, programId) >>
            getAttachmentRemotePathFromDB(user, programId, attachmentId)
          }
          res  <- s3FileSvc.verifyAndGet(path).map(_.asRight)
        } yield res
      } recover {
        case e: AttachmentException => e.asLeft
      }

      def insertAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag,
        fileName:       String,
        description:    Option[NonEmptyString],
        data:           Stream[F, Byte]
      )(using NoTransaction[F]): F[ObsAttachment.Id] =
        FileName
          .fromString(fileName)
          .fold(
            e  => Concurrent[F].raiseError(e),
            fn =>
              for {
                _      <- services.transactionally {
                  checkAccess(session, user, programId) >>
                  checkAttachmentType(attachmentType) >>
                  validateFileExtensionByType(attachmentType, fn) >>
                  checkForDuplicateName(programId, fn, none)
                }
                uuid   <- UUIDGen[F].randomUUID
                path    = filePath(programId, uuid, fn.value)
                size   <- s3FileSvc.upload(path, data)
                _      <- checkForEmptyFile(size)
                result <- insertAttachmentInDB(user, programId, attachmentType, fn, description, size, path)
              } yield result
          )

      def updateAttachment(
        user: User,
        programId: Program.Id,
        attachmentId: ObsAttachment.Id,
        fileName: String,
        description: Option[NonEmptyString],
        data: Stream[F, Byte]
      )(using NoTransaction[F]): F[Unit] =
        FileName
          .fromString(fileName)
          .fold(
            e  => Concurrent[F].raiseError(e),
            fn =>
              for {
                oldPath <- services.transactionally {
                  checkAccess(session, user, programId) >>
                  validateFileExtensionById(attachmentId, fn) >>
                  checkForDuplicateName(programId, fn, attachmentId.some) >>
                  getAttachmentRemotePathFromDB(user, programId, attachmentId)
                }
                uuid    <- UUIDGen[F].randomUUID
                newPath  = filePath(programId, uuid, fn.value)
                size    <- s3FileSvc.upload(newPath, data)
                _       <- checkForEmptyFile(size)
                _       <- updateAttachmentInDB(user, programId, attachmentId, fn, description, size, newPath)
                _       <- s3FileSvc.delete(oldPath)
              } yield ()
          )

      def deleteAttachment(
        user:         User,
        programId:    Program.Id,
        attachmentId: ObsAttachment.Id
      )(using NoTransaction[F]): F[Unit] =
        for {
          path <- services.transactionally {
            checkAccess(session, user, programId) >>
            deleteAttachmentFromDB(user, programId, attachmentId)
          }
          res  <-
            // We'll trap errors from the remote delete because, although not ideal, we don't
            // care so much if an orphan file is left on S3. The error will have been put in the trace.
            s3FileSvc.delete(path).handleError{ case _ => () }
        } yield res

      def getPresignedUrl(user: User, programId: Program.Id, attachmentId: ObsAttachment.Id)(using NoTransaction[F]): F[String] =
        for {
          path <- services.transactionally {
            checkAccess(session, user, programId) >>
            getAttachmentRemotePathFromDB(user, programId, attachmentId)
          }
          res  <- s3FileSvc.presignedUrl(path)
        } yield res

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
        ProgramUserService.Statements.whereUserAccess(user, programId) |+|
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
        ProgramUserService.Statements.andWhereUserAccess(user, programId) |+|
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
        ProgramUserService.Statements.andWhereUserAccess(user, programId)

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
        ProgramUserService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING c_remote_path"

    def attachmentTypeExists(attachmentType: Tag): AppliedFragment =
      sql"""
        EXISTS (select c_tag from t_obs_attachment_type where c_tag = $tag)
      """.apply(attachmentType)

    def getFileExtensions(attachmentType: Tag): AppliedFragment =
      sql"""
        SELECT c_file_extension
        FROM t_obs_attachment_file_ext
        WHERE c_attachment_type = $tag
      """.apply(attachmentType)

    def getFileExtensionsById(attachmentId: ObsAttachment.Id): AppliedFragment =
      sql"""
        SELECT e.c_file_extension
        FROM t_obs_attachment_file_ext e
        JOIN t_obs_attachment a ON e.c_attachment_type = a.c_attachment_type
        WHERE a.c_obs_attachment_id = $obs_attachment_id
      """.apply(attachmentId)
  }
}
