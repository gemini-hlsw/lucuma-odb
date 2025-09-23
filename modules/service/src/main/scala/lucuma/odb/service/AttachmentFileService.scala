// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.io.file.Path
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Attachment
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import java.util.UUID

import Services.Syntax.*

trait AttachmentFileService[F[_]] {
  import AttachmentFileService.AttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    attachmentId: Attachment.Id
  )(using NoTransaction[F]): F[Either[AttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def insertAttachment(
    user:           User,
    programId:      Program.Id,
    attachmentType: AttachmentType,
    fileName:       String,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  )(using NoTransaction[F]): F[Either[AttachmentException, Attachment.Id]]

  def updateAttachment(
    user:         User,
    attachmentId: Attachment.Id,
    fileName:     String,
    description:  Option[NonEmptyString],
    data:         Stream[F, Byte]
  )(using NoTransaction[F]): F[Either[AttachmentException, Unit]]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, attachmentId: Attachment.Id)(using
    NoTransaction[F]
  ): F[Either[AttachmentException, Unit]]

  def getPresignedUrl(user: User, attachmentId: Attachment.Id)(using
    NoTransaction[F]
  ): F[Either[AttachmentException, String]]
}

object AttachmentFileService {
  val DuplicateFileNameMsg                         = "Duplicate file name"
  def duplicateTypeMsg(at: AttachmentType): String =
    s"Duplicate attachment type: Only one ${at.shortName} is allowed per program"

  sealed trait AttachmentException extends Exception {
    def asLeftT[F[_]: Applicative, A]: EitherT[F, AttachmentException, A] =
      EitherT.leftT(this)
  }

  object AttachmentException {
    case object Forbidden                      extends AttachmentException
    case class InvalidRequest(message: String) extends AttachmentException
    case object FileNotFound                   extends AttachmentException
  }

  import AttachmentException.*

  protected type FileName = FileName.Type
  protected object FileName extends NewType[NonEmptyString] {
    def fromString(name: String): Either[AttachmentException, FileName] = {
      val path     = Path(name)
      val fileName = NonEmptyString.from(path.fileName.toString).toOption

      fileName.fold(
        InvalidRequest("File name is required").asLeft
      )(fn =>
        if (path.names.length > 1) {
          InvalidRequest("File name cannot include a path").asLeft
        } else FileName(fn).asRight
      )
    }

    extension (fileName: FileName)
      // does not contain the dot.
      def extName: Option[NonEmptyString] =
        NonEmptyString.from(Path(fileName.value.value).extName.drop(1).toLowerCase).toOption
  }

  extension [F[_], A](fe: F[Either[AttachmentException, A]])
    def asEitherT: EitherT[F, AttachmentException, A] =
      EitherT(fe)

  extension [A](e: Either[AttachmentException, A])
    def liftF[F[_]: Applicative]: EitherT[F, AttachmentException, A] =
      EitherT.fromEither(e)

  extension [F[_]: Applicative, A](fa: F[A])
    def right: EitherT[F, AttachmentException, A] =
      EitherT.right(fa)

  extension [F[_]](svcs: Services[F])
    def transactionallyEitherT[A](
      fa: (Transaction[F], Services[F]) ?=> EitherT[F, AttachmentException, A]
    )(using
      NoTransaction[F]
    ): EitherT[F, AttachmentException, A] =
      svcs.transactionally { val x = fa; x.value }.asEitherT

  def checkExtension(
    fileName:          FileName,
    allowedExtensions: Set[NonEmptyString]
  ): Either[AttachmentException, Unit] = {
    val isOK = allowedExtensions.isEmpty || fileName.extName.exists(allowedExtensions.contains)
    if (isOK) ().asRight
    else {
      val msg =
        if (allowedExtensions.size === 1)
          s"Must be a ${allowedExtensions.head.value.toUpperCase} file."
        else
          val valids = allowedExtensions.toList.map(_.value.toUpperCase).sorted.mkString(", ")
          s"Must be one of: $valids"
      InvalidRequest(s"Invalid file. $msg").asLeft
    }
  }

  def checkForEmptyFile(fileSize: Long): Either[AttachmentException, Unit] =
    if (fileSize <= 0) InvalidRequest("File cannot be empty").asLeft
    else ().asRight

  def instantiate[F[_]: Concurrent: Trace: UUIDGen](
    s3FileSvc: S3FileService[F]
  )(using Services[F]): AttachmentFileService[F] = {

    enum AccessRequired:
      case Read
      case Write

    def checkAccess(
      user:      User,
      programId: Program.Id,
      required: AccessRequired,
      onNoAccess: AttachmentException
    )(using Services[F], Transaction[F]): EitherT[F, AttachmentException, Unit] = user match {
      // guest users not allowed to upload files
      case GuestUser(_) => Forbidden.asLeftT
      case _            =>
        val check: F[Boolean] = required match
          case AccessRequired.Read => programUserService.userHasReadAccess(programId)
          case AccessRequired.Write => programUserService.userHasWriteAccess(programId)
        
        check
          .map(b => if (b) ().asRight else onNoAccess.asLeft)
          .asEitherT
    }

    def insertAttachmentInDB(
      programId:      Program.Id,
      attachmentType: AttachmentType,
      fileName:       FileName,
      description:    Option[NonEmptyString],
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): F[Either[AttachmentException, Attachment.Id]] =
      Trace[F].span("insertAttachment") {
        session
          .unique(Statements.InsertAttachment)(programId,
                                               attachmentType,
                                               fileName.value,
                                               description,
                                               fileSize,
                                               remotePath
          )
          .map(_.asRight)
          .recover {
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_file_name"))       =>
              InvalidRequest(DuplicateFileNameMsg).asLeft
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_attachment_type")) =>
              InvalidRequest(duplicateTypeMsg(attachmentType)).asLeft
          }
      }

    def updateAttachmentInDB(
      programId:    Program.Id,
      attachmentId: Attachment.Id,
      fileName:     FileName,
      description:  Option[NonEmptyString],
      fileSize:     Long,
      remotePath:   NonEmptyString
    ): F[Either[AttachmentException, Unit]] =
      Trace[F].span("updateAttachment") {
        session
          .unique(Statements.UpdateAttachment)(fileName.value,
                                               description,
                                               fileSize,
                                               remotePath,
                                               programId,
                                               attachmentId
          )
          .map(b =>
            if (b) ().asRight
            else FileNotFound.asLeft
          )
          .recover {
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_file_name")) =>
              InvalidRequest(DuplicateFileNameMsg).asLeft
          }
      }

    def getAttachmentInfoFromDB(
      attachmentId: Attachment.Id
    ): F[Either[AttachmentException, (Program.Id, NonEmptyString)]] =
        session
          .option(Statements.GetAttachmentInfo)(attachmentId)
          .map(_.toRight(FileNotFound))

    def getAttachmentInfoAndCheckAccess(
      user:         User,
      attachmentId: Attachment.Id,
      required: AccessRequired
    )(using Services[F], Transaction[F]): EitherT[F, AttachmentException, (Program.Id, NonEmptyString)] =
      for {
        (pid, path) <- getAttachmentInfoFromDB(attachmentId).asEitherT
        _           <- checkAccess(user, pid, required, FileNotFound)
      } yield (pid, path)

    def deleteAttachmentFromDB(
      attachmentId: Attachment.Id
    ): F[Either[AttachmentException, NonEmptyString]] =
      Trace[F].span("deleteAttachmentFromDB") {
        session
          .option(Statements.DeleteAttachment)(attachmentId)
          .map(_.toRight(FileNotFound))
      }

    def checkForDuplicateName(
      programId: Program.Id,
      fileName:  FileName,
      oaid:      Option[Attachment.Id]
    ): F[Either[AttachmentException, Unit]] = {
      val af   = Statements.checkForDuplicateName(programId, fileName.value, oaid)
      val stmt = af.fragment.query(bool)

      session
        .prepareR(stmt)
        .use(pg =>
          pg.option(af.argument)
            // if there is a value, it's a duplicate
            .map(_.fold(().asRight)(_ => InvalidRequest(DuplicateFileNameMsg).asLeft))
        )
    }

    def getAttachmentTypeById(
      attachmentId: Attachment.Id
    ): F[Either[AttachmentException, AttachmentType]] =
      session
        .option(Statements.GetAttachmentTypeById)(attachmentId)
        .map(_.toRight(FileNotFound))

    def validateFileExtensionByType(
      attachmentType: AttachmentType,
      fileName:       FileName
    ): Either[AttachmentException, Unit] =
      checkExtension(fileName, attachmentType.fileExtensions)

    def validateFileExtensionById(
      attachmentId: Attachment.Id,
      fileName:     FileName
    ): F[Either[AttachmentException, Unit]] =
      getAttachmentTypeById(attachmentId)
        .map(_.flatMap(at => checkExtension(fileName, at.fileExtensions)))

    // This can only be an issue on insert
    def checkForDuplicateType(
      programId:      Program.Id,
      attachmentType: AttachmentType
    ): F[Either[AttachmentException, Unit]] =
      if (attachmentType.uniqueInProgram)
        session
          .option(Statements.CheckForDuplicateType)(programId, attachmentType)
          .map(_.fold(().asRight)(_ => InvalidRequest(duplicateTypeMsg(attachmentType)).asLeft))
      else ().asRight.pure

    def filePath(programId: Program.Id, remoteId: UUID, fileName: NonEmptyString)(using SuperUserAccess): NonEmptyString =
      s3FileSvc.filePath(programId, remoteId, fileName)

    new AttachmentFileService[F] {

      def getAttachment(
        user:         User,
        attachmentId: Attachment.Id
      )(using NoTransaction[F]): F[Either[AttachmentException, Stream[F, Byte]]] =
        (for {
          path <- services.transactionallyEitherT {
                      getAttachmentInfoAndCheckAccess(user, attachmentId, AccessRequired.Read).map(_._2)
                  }
          res  <- Services.asSuperUser(s3FileSvc.verifyAndGet(path)).right
        } yield res).value
          .recoverWith { case e: AttachmentException =>
            e.asLeft.pure
          }

      // TODO: Need to check for uniqueness violation for proposal attachments
      def insertAttachment(
        user:           User,
        programId:      Program.Id,
        attachmentType: AttachmentType,
        fileName:       String,
        description:    Option[NonEmptyString],
        data:           Stream[F, Byte]
      )(using NoTransaction[F]): F[Either[AttachmentException, Attachment.Id]] =
          (
            for {
              fn     <- FileName.fromString(fileName).liftF
              _      <- services.transactionallyEitherT {
                          checkAccess(user, programId, AccessRequired.Write, Forbidden) >>
                            validateFileExtensionByType(attachmentType, fn).liftF >>
                            checkForDuplicateType(programId, attachmentType).asEitherT >>
                            checkForDuplicateName(programId, fn, none).asEitherT
                        }
              uuid   <- UUIDGen[F].randomUUID.right
              path    = Services.asSuperUser(filePath(programId, uuid, fn.value))
            } yield (fn, path)
          ).value
          .flatTap {
            // Up to this point, we haven't read the data yet. 
            // If we don't drain the request body before returning,
            // the client that Heroku uses as a proxy will simply 
            // return a network error. See this for more info:
            // https://github.com/http4s/http4s/pull/7602
            case Left(_)  => data.compile.drain
            case _ => ().pure
          }.asEitherT
          .flatMap((fn, path) =>
            for {
              size   <- Services.asSuperUser(s3FileSvc.upload(path, data)).right
              _      <- checkForEmptyFile(size).liftF
              result <- insertAttachmentInDB(programId,
                                             attachmentType,
                                             fn,
                                             description,
                                             size,
                                             path
                        ).asEitherT
            } yield result
          )
          .value

      def updateAttachment(
        user:         User,
        attachmentId: Attachment.Id,
        fileName:     String,
        description:  Option[NonEmptyString],
        data:         Stream[F, Byte]
      )(using NoTransaction[F]): F[Either[AttachmentException, Unit]] =
        (
          for {
            fn      <- FileName.fromString(fileName).liftF
            (pid, oldPath) <- services.transactionallyEitherT {
                for {
                  (pid, oldPath) <- getAttachmentInfoAndCheckAccess(user, attachmentId, AccessRequired.Write)
                  _              <- validateFileExtensionById(attachmentId, fn).asEitherT
                  _              <- checkForDuplicateName(pid, fn, attachmentId.some).asEitherT 
                } yield (pid, oldPath)
              }
            uuid           <- UUIDGen[F].randomUUID.right
            newPath         = Services.asSuperUser(filePath(pid, uuid, fn.value))
          } yield (fn, pid, oldPath, newPath)
        ).value
        .flatTap {
          // See comment in similar location in insertAttachment.
          case Left(_)  => data.compile.drain
          case _ => ().pure
        }.asEitherT
        .flatMap((fn, pid, oldPath, newPath) =>
          for {
            size    <- Services.asSuperUser(s3FileSvc.upload(newPath, data)).right
            _       <- checkForEmptyFile(size).liftF
            _       <- updateAttachmentInDB(pid,
                                            attachmentId,
                                            fn,
                                            description,
                                            size,
                                            newPath
                        ).asEitherT
            _       <- Services.asSuperUser(s3FileSvc.delete(oldPath)).right
          } yield ()
        )
        .value

      def deleteAttachment(
        user:         User,
        attachmentId: Attachment.Id
      )(using NoTransaction[F]): F[Either[AttachmentException, Unit]] =
        (for {
          path <- services.transactionallyEitherT {
              for {
                (_, path) <- getAttachmentInfoAndCheckAccess(user, attachmentId, AccessRequired.Write)
                _         <- deleteAttachmentFromDB(attachmentId).asEitherT
              } yield path
            }
          res  <-
            // We'll trap errors from the remote delete because, although not ideal, we don't
            // care so much if an orphan file is left on S3. The error will have been put in the trace.
            Services.asSuperUser(s3FileSvc.delete(path)).handleError { case _ => () }.right
        } yield res).value

      def getPresignedUrl(user: User, attachmentId: Attachment.Id)(using
        NoTransaction[F]
      ): F[Either[AttachmentException, String]] =
        (for {
          path <- services.transactionallyEitherT {
                      getAttachmentInfoAndCheckAccess(user, attachmentId, AccessRequired.Read).map(_._2)
                  }
          res  <- Services.asSuperUser(s3FileSvc.presignedUrl(path)).right
        } yield res).value

    }
  }

  object Statements {

    val InsertAttachment: Query[
      (Program.Id, AttachmentType, NonEmptyString, Option[NonEmptyString], Long, NonEmptyString),
      Attachment.Id
    ] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_description,
          c_file_size,
          c_remote_path
        )
        SELECT
          $program_id,
          $attachment_type,
          $text_nonempty,
          ${text_nonempty.opt},
          $int8,
          $text_nonempty
        RETURNING c_attachment_id
      """.query(attachment_id)

    val UpdateAttachment: Query[
      (NonEmptyString, Option[NonEmptyString], Long, NonEmptyString, Program.Id, Attachment.Id),
      Boolean
    ] =
      sql"""
        UPDATE t_attachment
        SET c_file_name   = $text_nonempty,
            c_description = ${text_nonempty.opt},
            c_checked     = false,
            c_file_size   = $int8,
            c_remote_path = $text_nonempty
        WHERE c_program_id = $program_id AND c_attachment_id = $attachment_id
        RETURNING true
      """.query(bool)

    val GetAttachmentInfo: Query[Attachment.Id, (Program.Id, NonEmptyString)] =
      sql"""
        SELECT c_program_id, c_remote_path
        FROM t_attachment
        WHERE c_attachment_id = $attachment_id
      """.query(program_id *: text_nonempty)

    def checkForDuplicateName(
      programId:    Program.Id,
      fileName:     NonEmptyString,
      attachmentId: Option[Attachment.Id]
    ): AppliedFragment =
      sql"""
        SELECT true
        FROM t_attachment
        WHERE c_program_id = $program_id AND c_file_name = $text_nonempty
      """.apply(programId, fileName) |+|
        attachmentId.foldMap(aid => sql"""
            AND c_attachment_id != $attachment_id
          """.apply(aid))

    val CheckForDuplicateType: Query[(Program.Id, AttachmentType), Boolean] =
      sql"""
        SELECT true
        FROM t_attachment
        WHERE c_program_id = $program_id AND c_attachment_type = $attachment_type
      """.query(bool)

    // returns the UUID for the remote file id
    val DeleteAttachment: Query[Attachment.Id, NonEmptyString] =
      sql"""
        DELETE FROM t_attachment
        WHERE c_attachment_id = $attachment_id
        RETURNING c_remote_path
      """.query(text_nonempty)

    val GetAttachmentTypeById: Query[Attachment.Id, AttachmentType] =
      sql"""
        SELECT c_attachment_type
        FROM t_attachment
        WHERE c_attachment_id = $attachment_id
      """.query(attachment_type)
  }
}
