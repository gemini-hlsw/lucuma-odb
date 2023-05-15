// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.Config
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import java.util.UUID

trait ProposalAttachmentFileService[F[_]] {
  import AttachmentFileService.AttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentType: Tag
  ): F[Either[AttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def insertAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[Unit]

  def updateAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: Tag,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[Unit]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, programId: Program.Id, attachmentType: Tag): F[Unit]
}

object ProposalAttachmentFileService extends AttachmentFileService {
  import AttachmentFileService.AttachmentException
  import AttachmentException.*

  import org.typelevel.log4cats.Logger
  def fromS3AndSession[F[_]: Async: Trace: Logger](
    s3FileSvc: S3FileService[F],
    session:   Session[F]
  ): ProposalAttachmentFileService[F] = {

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
    ): F[Unit] =
      Trace[F].span("insertProposalAttachment") {
        val af   = 
          Statements.insertAttachment(user, programId, attachmentType, fileName.value, description, fileSize, remoteId)
        val stmt = af.fragment.command
        session.prepareR(stmt)
          .use(pg =>
            pg.execute(af.argument)
              .void
              // TODO: Handle 2 constraint violations
              .recoverWith {
                // This seems a bit brittle. But this can only happen if something changes between the 
                // initial checks for duplication and this function is called after the upload.
                case SqlState.UniqueViolation(e) => 
                  if (e.constraintName.contains("t_proposal_attachment_pkey"))
                    Async[F].raiseError(InvalidRequest("Duplicate attachment type"))
                  else Async[F].raiseError(InvalidRequest("Duplicate file name"))
              }
           )
      }

    def updateAttachmentInDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       FileName,
      description:    Option[NonEmptyString],
      fileSize:       Long, 
      remoteId:       UUID
    ): F[Unit] =
      Trace[F].span("updateProposalAttachment") {
        val af   = 
          Statements.updateAttachment(user, programId, attachmentType, fileName.value, description, fileSize, remoteId)
        val stmt = af.fragment.query(bool)
        session.prepareR(stmt)
          .use(pg =>
            pg.unique(af.argument)
              .flatMap(b =>
                if (b) Async[F].unit
                else Async[F].raiseError(FileNotFound)
              )
              .recoverWith {
                case SqlState.UniqueViolation(_) => 
                  Async[F].raiseError(InvalidRequest("Duplicate file name"))
              }
           )
      }

    def getAttachmentRemoteIdFromDB(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[UUID] =
      Trace[F].span("getProposalAttachmentRemoteIdFromDB") {
        val af   = Statements.getAttachmentRemoteId(user, programId, attachmentType)
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
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[UUID] =
      Trace[F].span("deleteProposalAttachmentFromDB") {
        val af   = Statements.deleteAttachment(user, programId, attachmentType)
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

    def checkForDuplicateAttachment(user: User, programId: Program.Id, attachmentType: Tag): F[Unit] = 
      getOptionalRemoteId(user, programId, attachmentType)
        .flatMap { 
          case Some(_) => Async[F].raiseError(InvalidRequest("Duplicate attachment type"))
          case None    => Async[F].unit
          }

    def getOptionalRemoteId(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): F[Option[UUID]] =
      Trace[F].span("getProposalAttachmentRemoteIdFromDB") {
        val af   = Statements.getAttachmentRemoteId(user, programId, attachmentType)
        val stmt = af.fragment.query(uuid)

        session
          .prepareR(stmt)
          .use(_.option(af.argument))
      }

    def checkForDuplicateName(programId: Program.Id, fileName: FileName, oType: Option[Tag]): F[Unit] = {
      val af   = Statements.checkForDuplicateName(programId, fileName.value, oType)
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

    def key(programId: Program.Id, remoteId: UUID) =
      s3FileSvc.proposalFileKey(programId, remoteId)

    new ProposalAttachmentFileService[F] {
      def getAttachment(
        user: User,
        programId: Program.Id,
        attachmentType: Tag
      ): F[Either[AttachmentException, Stream[F, Byte]]] = 
        session.transaction
          .use(_ =>
            for {
              _        <- checkAccess(session, user, programId)
              _        <- checkAttachmentType(attachmentType)
              remoteId <- getAttachmentRemoteIdFromDB(user, programId, attachmentType)
            } yield remoteId
          )
          .flatMap { rid =>
            s3FileSvc.verifyAndGet(key(programId, rid)).map(_.asRight)
          }
          .recover {
            case e: AttachmentException => e.asLeft
          }

      def insertAttachment(
        user: User,
        programId: Program.Id,
        attachmentType: Tag,
        fileName: String,
        description: Option[NonEmptyString],
        data: Stream[F, Byte]): F[Unit] = 
        FileName
          .fromString(fileName)
          .fold(
            e  => Async[F].raiseError(e),
            fn =>
              session.transaction
                .use(_ =>
                  for {
                    _ <- checkAccess(session, user, programId)
                    _ <- checkAttachmentType(attachmentType)
                    _ <- checkForDuplicateAttachment(user, programId, attachmentType)
                    _ <- checkForDuplicateName(programId, fn, none)
                  } yield ()
                )
                .flatMap( _ =>
                  for {
                    rid    <- Async[F].delay(UUID.randomUUID())
                    size   <- s3FileSvc.upload(key(programId, rid), data)
                    result <- insertAttachmentInDB(user, programId, attachmentType, fn, description, size, rid)
                  } yield result
                )
          )

      def updateAttachment(
        user: User,
        programId: Program.Id,
        attachmentType: Tag,
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
                    _   <- checkAccess(session, user, programId)
                    _   <- checkAttachmentType(attachmentType)
                    _   <- checkForDuplicateName(programId, fn, attachmentType.some)
                    rid <- getAttachmentRemoteIdFromDB(user, programId, attachmentType)
                  } yield rid
                )
                .flatMap(oldRid =>
                  for {
                    newRid <- Async[F].delay(UUID.randomUUID())
                    size   <- s3FileSvc.upload(key(programId, newRid), data)
                    _      <- updateAttachmentInDB(user, programId, attachmentType, fn, description, size, newRid)
                    _      <- s3FileSvc.delete(key(programId, oldRid))
                  } yield ()
                )
          )
        
      def deleteAttachment(user: User, programId: Program.Id, attachmentType: Tag): F[Unit] = 
        session.transaction
          .use(_ =>
            for {
              _   <- checkAccess(session, user, programId)
              _   <- checkAttachmentType(attachmentType)
              rid <- deleteAttachmentFromDB(user, programId, attachmentType)
            } yield rid
          )
          .flatMap(rid =>
            // We'll trap errors from the remote delete because, although not ideal, we don't 
            // care so much if an orphan file is left on S3. The error will have been put in the trace.
            s3FileSvc.delete(key(programId, rid)).handleError{ case _ => () }
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
        INSERT INTO t_proposal_attachment (
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
      """.apply(programId, attachmentType, fileName, description, fileSize, remoteId) |+|
        ProgramService.Statements.whereUserAccess(user, programId)

    def updateAttachment(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag,
      fileName:       NonEmptyString,
      description:    Option[NonEmptyString],
      fileSize:       Long,
      remoteId:       UUID
    ): AppliedFragment =
      sql"""
        UPDATE t_proposal_attachment
        SET c_file_name   = $text_nonempty,
            c_description = ${text_nonempty.opt},
            c_checked     = false,
            c_file_size   = $int8,
            c_remote_id   = $uuid
        WHERE c_program_id = $program_id AND c_attachment_type = $tag
      """.apply(fileName, description, fileSize, remoteId, programId, attachmentType) |+|
        ProgramService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING true"

    def getAttachmentRemoteId(
      user:           User,
      programId:      Program.Id,
      attachmentType: Tag
    ): AppliedFragment =
      sql"""
        SELECT c_remote_id
        FROM t_proposal_attachment
        WHERE c_program_id = $program_id AND c_attachment_type = $tag
      """.apply(programId, attachmentType) |+|
        ProgramService.Statements.andWhereUserAccess(user, programId)

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
        ProgramService.Statements.andWhereUserAccess(user, programId) |+|
        void"RETURNING c_remote_id"

    def checkForDuplicateName(
      programId:  Program.Id,
      fileName:   NonEmptyString,
      oType:      Option[Tag]
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
