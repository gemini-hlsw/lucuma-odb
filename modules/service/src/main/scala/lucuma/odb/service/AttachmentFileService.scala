// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Chunk
import fs2.Stream
import fs2.io.file.Path
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.mos.MosMaskProblem
import lucuma.catalog.mos.MosMaskReader
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Attachment
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.odb.data.MaskDefinition
import lucuma.odb.json.maskDefinition.given
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.circe.codec.all.*
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
  val DuplicateMaskNameMsg                         = "Duplicate mask name"
  def duplicateMaskNameMsg(maskName: NonEmptyString): String =
    s"$DuplicateMaskNameMsg: ${maskName.value}"
  def odbGeneratedMsg(at: AttachmentType): String =
    s"${at.shortName} attachments cannot be uploaded, replaced or deleted."

  val AttachmentInUseMsg = "The attachment is in use and cannot be deleted."

  val MaskInstrumentInUseMsg =
    "The attachment is in use by an observation and the replacement file is for a different instrument."

  sealed trait AttachmentException extends Exception {
    def asLeftT[F[_]: Applicative, A]: EitherT[F, AttachmentException, A] =
      EitherT.leftT(this)
  }

  object AttachmentException {
    case object Forbidden                      extends AttachmentException
    case class InvalidRequest(message: String) extends AttachmentException
    case object FileNotFound                   extends AttachmentException
    case class AttachmentInUse(message: String) extends AttachmentException
  }

  import AttachmentException.*

  /**
   * Attachments the ODB generates for itself rather than accepting from a user.
   * These have no write path through the attachment routes: they can be read and
   * downloaded, but not created, replaced or deleted.
   *
   * This is an explicit match rather than a test on `purpose` so that adding a
   * future proposal attachment type does not silently make it unwritable.
   */
  extension (at: AttachmentType)
    def isOdbGenerated: Boolean = at match
      case AttachmentType.Summary => true
      case _                      => false

  def checkNotOdbGenerated(at: AttachmentType): Either[AttachmentException, Unit] =
    if at.isOdbGenerated then InvalidRequest(odbGeneratedMsg(at)).asLeft
    else ().asRight

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

  /**
   * MOS mask files follow the standard ODF naming convention in either the OCS
   * form, `G(N|S)YYYY(A|B)<type>PPP-XX_ODF.fits`, or the GPP form built from
   * the program reference with its dashes removed,
   * `GYYYY(A|B)PPPP<type>-XX_ODF.fits`.
   *
   * The mask name is the file name with the `_ODF.fits` suffix removed.
   * Names are matched case-insensitively but stored upper case, so the identifier
   * handed to observe is canonical however the file was named.
   */
  private val OcsMaskFileName =
    raw"(?i)(G[NS]\d{4}[AB](?:ENG|CAL|COM|DD|DS|SV|LP|FT|Q|C)\d{3}-\d{2})_ODF\.fits".r
  private val GppMaskFileName = raw"(?i)(G\d{4}[AB]\d{4}[CDFLPQSV]-\d{2})_ODF\.fits".r

  val InvalidMaskFileNameMsg =
    "Invalid MOS mask file name. Must follow the ODF naming convention, e.g. 'GS2015AQ023-01_ODF.fits' or 'G2027A1234Q-42_ODF.fits'."

  def deriveMaskName(
    attachmentType: AttachmentType,
    fileName:       FileName
  ): Either[AttachmentException, Option[NonEmptyString]] =
    def maskName(root: String): Either[AttachmentException, Option[NonEmptyString]] =
      NonEmptyString.from(root.toUpperCase).bimap(_ => InvalidRequest(InvalidMaskFileNameMsg), _.some)

    if (attachmentType =!= AttachmentType.MosMask) none.asRight
    else
      fileName.value.value match
        case OcsMaskFileName(root) => maskName(root)
        case GppMaskFileName(root) => maskName(root)
        case _                     => InvalidRequest(InvalidMaskFileNameMsg).asLeft

  val EmptyFileMsg = "File cannot be empty"

  def checkForEmptyFile(fileSize: Long): Either[AttachmentException, Unit] =
    if (fileSize <= 0) InvalidRequest(EmptyFileMsg).asLeft
    else ().asRight

  def invalidMaskFileMsg(problem: MosMaskProblem): String =
    s"Invalid MOS mask file. ${problem.displayValue}"

  val MissingPositionAngleMsg =
    "Invalid MOS mask file. The design records no position angle (MASK_PA), so it cannot be observed."

  def instantiate[F[_]: {Concurrent, Tracer as T, UUIDGen}](
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
      maskName:       Option[NonEmptyString],
      maskDefinition: Option[Json],
      description:    Option[NonEmptyString],
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): F[Either[AttachmentException, Attachment.Id]] =
      T.span("insertAttachment").surround {
        session
          .unique(Statements.InsertAttachment)(programId,
                                               attachmentType,
                                               fileName.value,
                                               maskName,
                                               maskDefinition,
                                               description,
                                               fileSize,
                                               remotePath
          )
          .map(_.asRight)
          .recover {
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_mask_name"))       =>
              InvalidRequest(DuplicateMaskNameMsg).asLeft
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_file_name"))       =>
              InvalidRequest(DuplicateFileNameMsg).asLeft
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_attachment_type")) =>
              InvalidRequest(duplicateTypeMsg(attachmentType)).asLeft
          }
      }

    def updateAttachmentInDB(
      programId:      Program.Id,
      attachmentId:   Attachment.Id,
      fileName:       FileName,
      maskName:       Option[NonEmptyString],
      maskDefinition: Option[Json],
      description:    Option[NonEmptyString],
      fileSize:       Long,
      remotePath:     NonEmptyString
    ): F[Either[AttachmentException, Unit]] =
      T.span("updateAttachment").surround {
        session
          .unique(Statements.UpdateAttachment)(fileName.value,
                                               maskName,
                                               maskDefinition,
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
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_mask_name")) =>
              InvalidRequest(DuplicateMaskNameMsg).asLeft
            case SqlState.UniqueViolation(e) if e.detail.exists(_.contains("c_file_name")) =>
              InvalidRequest(DuplicateFileNameMsg).asLeft
            // Triggered in case the mask instrument changes and it is in use with a different one.
            case SqlState.ForeignKeyViolation(e) if e.constraintName.exists(_.contains("mask_attachment_fkey")) =>
              AttachmentInUse(MaskInstrumentInUseMsg).asLeft
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
      T.span("deleteAttachmentFromDB").surround {
        session
          .option(Statements.DeleteAttachment)(attachmentId)
          .map(_.toRight(FileNotFound))
          .recover:
            case SqlState.ForeignKeyViolation(_) => AttachmentInUse(AttachmentInUseMsg).asLeft
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

    def checkForDuplicateMaskName(
      programId: Program.Id,
      maskName:  Option[NonEmptyString],
      oaid:      Option[Attachment.Id]
    ): F[Either[AttachmentException, Unit]] =
      maskName.fold(().asRight.pure) { mn =>
        val af   = Statements.checkForDuplicateMaskName(programId, mn, oaid)
        val stmt = af.fragment.query(bool)

        session
          .prepareR(stmt)
          .use(pg =>
            pg.option(af.argument)
              .map(_.fold(().asRight)(_ => InvalidRequest(duplicateMaskNameMsg(mn)).asLeft))
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

    // A MOS mask file is parsed at upload so its design can be recorded on
    // the attachment.
    // Mask files are small, so the body is buffered in memory to parse and
    // upload from the same bytes.
    def parseMaskDefinition(
      maskName: NonEmptyString,
      data:     Stream[F, Byte]
    ): F[Either[AttachmentException, (Stream[F, Byte], Option[Json])]] =
      T.span("parseMaskDefinition").surround:
        data.compile.to(Chunk).flatMap: bytes =>
          val buffered = Stream.chunk(bytes).covary[F]
          if bytes.isEmpty then
            InvalidRequest(EmptyFileMsg).asLeft.pure
          else
            (for {
              header <- buffered.through(MosMaskReader.header[F]).compile.lastOrError
              slits  <- buffered.through(MosMaskReader.slits[F]).compile.toList
              result  = MaskDefinition
                          .fromMosMask(maskName, header, slits)
                          .toRight(InvalidRequest(MissingPositionAngleMsg))
                          .map(d => (buffered, d.asJson.some))
            } yield result)
              .recover { case p: MosMaskProblem => InvalidRequest(invalidMaskFileMsg(p)).asLeft }

    def maybeParseMaskDefinition(
      attachmentType: AttachmentType,
      maskName:       Option[NonEmptyString],
      data:           Stream[F, Byte]
    ): F[Either[AttachmentException, (Stream[F, Byte], Option[Json])]] =
      maskName.filter(_ => attachmentType === AttachmentType.MosMask) match
        case Some(mn) => parseMaskDefinition(mn, data)
        case None     => (data, none[Json]).asRight.pure

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
              mn     <- services.transactionallyEitherT:
                          for {
                            _  <- checkNotOdbGenerated(attachmentType).liftF
                            _  <- checkAccess(user, programId, AccessRequired.Write, Forbidden)
                            _  <- validateFileExtensionByType(attachmentType, fn).liftF
                            mn <- deriveMaskName(attachmentType, fn).liftF
                            _  <- checkForDuplicateType(programId, attachmentType).asEitherT
                            _  <- checkForDuplicateName(programId, fn, none).asEitherT
                            _  <- checkForDuplicateMaskName(programId, mn, none).asEitherT
                          } yield mn
              uuid   <- UUIDGen[F].randomUUID.right
              path    = Services.asSuperUser(filePath(programId, uuid, fn.value))
            } yield (fn, mn, path)
          ).value
          .flatTap {
            // Up to this point, we haven't read the data yet.
            // If we don't drain the request body before returning,
            // the client that Heroku uses as a proxy will simply
            // return a network error. See this for more info:
            // https://github.com/http4s/http4s/pull/7602
            case Left(_)  => data.compile.drain
            case _ => ().pure
          }
          .asEitherT
          .flatMap((fn, mn, path) =>
            for {
              (upload, md) <- maybeParseMaskDefinition(attachmentType, mn, data).asEitherT
              size         <- Services.asSuperUser(s3FileSvc.upload(path, upload)).right
              _            <- checkForEmptyFile(size).liftF
              result       <- insertAttachmentInDB(programId,
                                                   attachmentType,
                                                   fn,
                                                   mn,
                                                   md,
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
            fn                 <- FileName.fromString(fileName).liftF
            (pid, at, mn, oldPath) <- services.transactionallyEitherT {
                for {
                  (pid, oldPath) <- getAttachmentInfoAndCheckAccess(user, attachmentId, AccessRequired.Write)
                  at             <- getAttachmentTypeById(attachmentId).asEitherT
                  _              <- checkNotOdbGenerated(at).liftF
                  _              <- checkExtension(fn, at.fileExtensions).liftF
                  mn             <- deriveMaskName(at, fn).liftF
                  _              <- checkForDuplicateName(pid, fn, attachmentId.some).asEitherT
                  _              <- checkForDuplicateMaskName(pid, mn, attachmentId.some).asEitherT
                } yield (pid, at, mn, oldPath)
              }
            uuid               <- UUIDGen[F].randomUUID.right
            newPath            = Services.asSuperUser(filePath(pid, uuid, fn.value))
          } yield (fn, at, mn, pid, oldPath, newPath)
        ).value
        .flatTap {
          // See comment in similar location in insertAttachment.
          case Left(_)  => data.compile.drain
          case _ => ().pure
        }
        .asEitherT
        .flatMap((fn, at, mn, pid, oldPath, newPath) =>
          for {
            (upload, md) <- maybeParseMaskDefinition(at, mn, data).asEitherT
            size         <- Services.asSuperUser(s3FileSvc.upload(newPath, upload)).right
            _            <- checkForEmptyFile(size).liftF
            _            <- updateAttachmentInDB(pid,
                                                 attachmentId,
                                                 fn,
                                                 mn,
                                                 md,
                                                 description,
                                                 size,
                                                 newPath
                            ).asEitherT
            _            <- Services.asSuperUser(s3FileSvc.delete(oldPath)).right
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
                at        <- getAttachmentTypeById(attachmentId).asEitherT
                _         <- checkNotOdbGenerated(at).liftF
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
      (Program.Id, AttachmentType, NonEmptyString, Option[NonEmptyString], Option[Json], Option[NonEmptyString], Long, NonEmptyString),
      Attachment.Id
    ] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_mask_name,
          c_mask_definition,
          c_description,
          c_file_size,
          c_remote_path
        )
        SELECT
          $program_id,
          $attachment_type,
          $text_nonempty,
          ${text_nonempty.opt},
          ${jsonb.opt},
          ${text_nonempty.opt},
          $int8,
          $text_nonempty
        RETURNING c_attachment_id
      """.query(attachment_id)

    val UpdateAttachment: Query[
      (NonEmptyString, Option[NonEmptyString], Option[Json], Option[NonEmptyString], Long, NonEmptyString, Program.Id, Attachment.Id),
      Boolean
    ] =
      sql"""
        UPDATE t_attachment
        SET c_file_name       = $text_nonempty,
            c_mask_name       = ${text_nonempty.opt},
            c_mask_definition = ${jsonb.opt},
            c_description     = ${text_nonempty.opt},
            c_checked         = false,
            c_file_size       = $int8,
            c_remote_path     = $text_nonempty
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

    def checkForDuplicateMaskName(
      programId:    Program.Id,
      maskName:     NonEmptyString,
      attachmentId: Option[Attachment.Id]
    ): AppliedFragment =
      sql"""
        SELECT true
        FROM t_attachment
        WHERE c_program_id      = $program_id
          AND c_mask_name       = $text_nonempty
          AND c_attachment_type = 'mos_mask'
      """.apply(programId, maskName) |+|
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
