// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import java.util.UUID

import Services.Syntax.*

trait ProposalAttachmentFileService[F[_]] {
  import AttachmentFileService.AttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag
  )(using NoTransaction[F]): F[Either[AttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def insertAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    data:           Stream[F, Byte]
  )(using NoTransaction[F]): F[Unit]

  def updateAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    data:           Stream[F, Byte]
  )(using NoTransaction[F]): F[Unit]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, programId: Program.Id, attachmentType: Tag)(using NoTransaction[F]): F[Unit]

  def getPresignedUrl(user: User, programId: Program.Id, attachmentType: Tag)(using NoTransaction[F]): F[String]
}

object ProposalAttachmentFileService extends AttachmentFileService {
  import AttachmentFileService.AttachmentException
  import AttachmentException.*

  private val allowedExtensions: List[NonEmptyString] = List("pdf".refined)

  def instantiate[F[_]: MonadCancelThrow: Trace: UUIDGen](
    s3FileSvc: S3FileService[F],
  )(using Services[F]): ProposalAttachmentFileService[F] = {

    def checkAttachmentType(attachmentType: Tag): F[Unit] = {
      val af   = Statements.existsAttachmentType(attachmentType)
      val stmt = sql"Select ${af.fragment}".query(bool)
      session
        .prepareR(stmt)
        .use { pg =>
          pg.unique(af.argument)
        }
        .flatMap(isValid =>
          if (isValid) MonadCancelThrow[F].unit
          else MonadCancelThrow[F].raiseError(InvalidRequest("Invalid attachment type"))
        )
    }

    def insertAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       FileName,
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): F[Unit] =
      Trace[F].span("insertProposalAttachment") {
        val af   =
          Statements.insertAttachment(user, programId, attachmentType, fileName.value, fileSize, remotePath)
        val stmt = af.fragment.command
        session
          .prepareR(stmt)
          .use(pg =>
            pg.execute(af.argument)
              .void
              // TODO: Handle 2 constraint violations
              .recoverWith {
                // This seems a bit brittle. But this can only happen if something changes between the
                // initial checks for duplication and this function is called after the upload.
                case SqlState.UniqueViolation(e) =>
                  if (e.constraintName.contains("t_proposal_attachment_pkey"))
                    MonadCancelThrow[F].raiseError(InvalidRequest("Duplicate attachment type"))
                  else MonadCancelThrow[F].raiseError(InvalidRequest("Duplicate file name"))
              }
          )
      }

    def updateAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       FileName,
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): F[Unit] =
      Trace[F].span("updateProposalAttachment") {
        val af   =
          Statements.updateAttachment(user, programId, attachmentType, fileName.value, fileSize, remotePath)
        val stmt = af.fragment.query(bool)
        session
          .prepareR(stmt)
          .use(pg =>
            pg.unique(af.argument)
              .flatMap(b =>
                if (b) MonadCancelThrow[F].unit
                else MonadCancelThrow[F].raiseError(FileNotFound)
              )
              .recoverWith {
                case SqlState.UniqueViolation(_) =>
                  MonadCancelThrow[F].raiseError(InvalidRequest("Duplicate file name"))
              }
          )
      }

    def getAttachmentRemotePathFromDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[NonEmptyString] =
      Trace[F].span("getProposalAttachmentRemoteIdFromDB") {
        val af   = Statements.getAttachmentRemotePath(user, programId, attachmentType)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => MonadCancelThrow[F].raiseError(FileNotFound)
                case Some(s) => MonadCancelThrow[F].pure(s)
              }
          )
      }

    def deleteAttachmentFromDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[NonEmptyString] =
      Trace[F].span("deleteProposalAttachmentFromDB") {
        val af   = Statements.deleteAttachment(user, programId, attachmentType)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .flatMap {
                case None    => MonadCancelThrow[F].raiseError(FileNotFound)
                case Some(s) => MonadCancelThrow[F].pure(s)
                }
          )
      }

    def checkForDuplicateAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[Unit] =
      getOptionalRemotePath(user, programId, attachmentType)
        .flatMap {
          case Some(_) => MonadCancelThrow[F].raiseError(InvalidRequest("Duplicate attachment type"))
          case None    => MonadCancelThrow[F].unit
          }

    def getOptionalRemotePath(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[Option[NonEmptyString]] =
      Trace[F].span("getProposalAttachmentRemoteIdFromDB") {
        val af   = Statements.getAttachmentRemotePath(user, programId, attachmentType)
        val stmt = af.fragment.query(text_nonempty)

        session
          .prepareR(stmt)
          .use(_.option(af.argument))
      }

    def checkForDuplicateName(
      programId: Program.Id,
      fileName:  FileName,
      oType:     Option[Tag]
    ): F[Unit] = {
      val af   = Statements.checkForDuplicateName(programId, fileName.value, oType)
      val stmt = af.fragment.query(bool)

      session
        .prepareR(stmt)
        .use(pg =>
          pg.option(af.argument)
            .flatMap {
              case None    => MonadCancelThrow[F].unit
              case Some(_) => MonadCancelThrow[F].raiseError(InvalidRequest("Duplicate file name"))
            }
        )
    }

    def filePath(programId: Program.Id, remoteId: UUID, fileName: NonEmptyString) =
      s3FileSvc.filePath(programId, remoteId, fileName)

    new ProposalAttachmentFileService[F] {
      def getAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag
      )(using NoTransaction[F]): F[Either[AttachmentException, Stream[F, Byte]]] = {
          for {
            path <- services.transactionally {
              checkAccess(session, user, programId) >>
              checkAttachmentType(attachmentType) >>
              getAttachmentRemotePathFromDB(user, programId, attachmentType)
            }
            res <- s3FileSvc.verifyAndGet(path).map(_.asRight)
        } yield res
      } recover {
        case e: AttachmentException => e.asLeft
      }

      def insertAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag,
        fileName: String,
        data: Stream[F, Byte])(using NoTransaction[F]): F[Unit] =
        FileName
          .fromString(fileName)
          .fold(
            e  => MonadCancelThrow[F].raiseError(e),
            fn =>
              for {
                _      <- services.transactionally {
                  checkAccess(session, user, programId) >>
                  checkAttachmentType(attachmentType) >>
                  checkExtension(fn, allowedExtensions) >>
                  checkForDuplicateAttachment(user, programId, attachmentType) >>
                  checkForDuplicateName(programId, fn, none)
                }
                uuid   <- UUIDGen[F].randomUUID
                path    = filePath(programId, uuid, fn.value)
                size   <- s3FileSvc.upload(path, data)
                _      <- checkForEmptyFile(size)
                _      <- insertAttachmentInDB(user, programId, attachmentType, fn, size, path)
              } yield ()
          )

      def updateAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: Tag,
        fileName: String,
        data: Stream[F, Byte]
      )(using NoTransaction[F]): F[Unit] =
        FileName
          .fromString(fileName)
          .fold(
            e  => MonadCancelThrow[F].raiseError(e),
            fn =>
              for {
                oldPath <- services.transactionally {
                  checkAccess(session, user, programId) >>
                  checkAttachmentType(attachmentType) >>
                  checkExtension(fn, allowedExtensions) >>
                  checkForDuplicateName(programId, fn, attachmentType.some) >>
                  getAttachmentRemotePathFromDB(user, programId, attachmentType)
                }
                uuid    <- UUIDGen[F].randomUUID
                newPath = filePath(programId, uuid, fn.value)
                size    <- s3FileSvc.upload(newPath, data)
                _       <- checkForEmptyFile(size)
                _       <- updateAttachmentInDB(user, programId, attachmentType, fn, size, newPath)
                _       <- s3FileSvc.delete(oldPath)
              } yield ()
          )

      def deleteAttachment(user: User, programId: Program.Id, attachmentType: Tag)(using NoTransaction[F]): F[Unit] =
        for {
          path <- services.transactionally {
            checkAccess(session, user, programId) >>
            checkAttachmentType(attachmentType) >>
            deleteAttachmentFromDB(user, programId, attachmentType)
          }
          _    <-
            // We'll trap errors from the remote delete because, although not ideal, we don't
            // care so much if an orphan file is left on S3. The error will have been put in the trace.
            s3FileSvc.delete(path).handleError{ case _ => () }
        } yield ()

      def getPresignedUrl(user: User, programId: Program.Id, attachmentType: Tag)(using NoTransaction[F]): F[String] =
        for {
          path <- services.transactionally {
            checkAccess(session, user, programId) >>
            checkAttachmentType(attachmentType) >>
            getAttachmentRemotePathFromDB(user, programId, attachmentType)
          }
          res <- s3FileSvc.presignedUrl(path)
        } yield res

    }
  }

  object Statements {

    def insertAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       NonEmptyString,
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): AppliedFragment =
      sql"""
        INSERT INTO t_proposal_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_file_size,
          c_remote_path
        )
        SELECT
          $program_id,
          $tag,
          $text_nonempty,
          $int8,
          $text_nonempty
      """.apply(programId, attachmentType, fileName, fileSize, remotePath) |+|
        ProgramUserService.Statements.whereUserAccess(user, programId)

    def updateAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       NonEmptyString,
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): AppliedFragment =
      sql"""
        UPDATE t_proposal_attachment
        SET c_file_name   = $text_nonempty,
            c_file_size   = $int8,
            c_remote_path = $text_nonempty
        WHERE c_program_id = $program_id AND c_attachment_type = $tag
      """.apply(fileName, fileSize, remotePath, programId, attachmentType) |+|
        ProgramUserService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING true"

    def getAttachmentRemotePath(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): AppliedFragment =
      sql"""
        SELECT c_remote_path
        FROM t_proposal_attachment
        WHERE c_program_id = $program_id AND c_attachment_type = $tag
      """.apply(programId, attachmentType) |+|
        ProgramUserService.Statements.andWhereUserAccess(user, programId)

    // returns the UUID for the remote file id
    def deleteAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): AppliedFragment =
      sql"""
        DELETE FROM t_proposal_attachment
        WHERE c_program_id = $program_id AND c_attachment_type = $tag
      """.apply(programId, attachmentType) |+|
        ProgramUserService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING c_remote_path"

    def checkForDuplicateName(
      programId: Program.Id,
      fileName:  NonEmptyString,
      oType:     Option[Tag]
    ): AppliedFragment =
      sql"""
        SELECT true
        FROM t_proposal_attachment
        WHERE c_program_id = $program_id AND c_file_name = $text_nonempty
      """.apply(programId, fileName) |+|
        oType.foldMap(attachmentType =>
          sql"""
            AND c_attachment_type != $tag
          """.apply(attachmentType)
        )

    def existsAttachmentType(attachmentType: Tag): AppliedFragment =
      sql"""
        EXISTS (select from t_proposal_attachment_type where c_tag = $tag)
      """.apply(attachmentType)
  }
}
