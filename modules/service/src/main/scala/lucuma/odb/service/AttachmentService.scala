// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.aws.s3.S3
import fs2.aws.s3.models.Models.FileKey
import fs2.aws.s3.models.Models.PartSizeMB
import fs2.io.file.Path
import io.laserdisc.pure.s3.tagless.Interpreter
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.model.Attachment
import lucuma.core.model.GuestUser
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
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.S3Configuration
import software.amazon.awssdk.services.s3.model.HeadObjectRequest
import software.amazon.awssdk.services.s3.model.NoSuchKeyException

sealed trait AttachmentService[F[_]] {
  import AttachmentService.AttachmentException

  /** Retrieves the given file from S3 as a stream. */
  def getAttachment(
    user:         User,
    programId:    Program.Id,
    attachmentId: Attachment.Id
  ): F[Either[AttachmentException, Stream[F, Byte]]]

  /** Uploads the file to S3 and addes it to the database */
  def uploadAttachment(
    user:           User, 
    programId:      Program.Id, 
    attachmentType: Tag,
    fileName:       String, 
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[Attachment.Id]

  /** Deletes the file from the database and then removes it from S3. */
  def deleteAttachment(user: User, programId: Program.Id, attachmentId: Attachment.Id): F[Unit]

  /** Deletes the file from S3 - assumes all validation has already occurred. */
  def remoteDeleteFile(programId: Program.Id, fileName: NonEmptyString): F[Unit]
}

object AttachmentService {
  sealed trait AttachmentException extends Exception
  // maybe we can improve on the AWS error handling...
  object AttachmentException {
    case object Forbidden                    extends AttachmentException
    case class  InvalidName(message: String) extends AttachmentException
    case class  InvalidType(message: String) extends AttachmentException
    case object FileNotFound                 extends AttachmentException
  }

  import AttachmentException._

  private type FileName = FileName.Type
  private object FileName extends NewType[NonEmptyString] {
    def fromString(name: String): Either[AttachmentException, FileName] = {
      val path         = Path(name)
      val segmentCount = path.names.length
      val fileName     = NonEmptyString.from(path.fileName.toString).toOption

      fileName.fold(
        InvalidName("File name is required").asLeft
      )(fn =>
        if (path.names.length > 1) {
          InvalidName("File name cannot include a path").asLeft
        }
        else FileName(fn).asRight
      )
    }

    extension (fileName: FileName) 
      def extName: Option[NonEmptyString] = 
        NonEmptyString.from(Path(fileName.value.value).extName).toOption
  }

  def fromConfigAndSession[F[_]: Async: Trace](
    awsConfig: Config.Aws,
    session:   Session[F]
  ): Resource[F, AttachmentService[F]] = {
    // We can switch back to unsafeFrom when a new version of refined is out that fixes 
    // https://github.com/fthomas/refined/issues/1161 or a version of fs2-aws > 6.0.0
    // is availabile with my PR that changes PartSizeMB to "GreaterEqual[5]"
    // val partSize = PartSizeMB.unsafeFrom(5)
    val partSize = refineV[Greater[5] Or Equal[5]](5).toOption.get
    
    val credentials = AwsBasicCredentials.create(awsConfig.accessKey.value, awsConfig.secretKey.value)

    val s3R: Resource[F, S3AsyncClientOp[F]] = Interpreter[F].S3AsyncClientOpResource(
      S3AsyncClient
        .builder()
        .credentialsProvider(StaticCredentialsProvider.create(credentials))
        .serviceConfiguration(
          S3Configuration
            .builder()
            .pathStyleAccessEnabled(true)
            .build()
        )
        .region(Region.US_EAST_1)
    )

    s3R.map(s => (s, S3.create[F](s))).map{ (s3Ops, s3) =>

      // We also use this to make sure the file exists and we have read permissions.
      def getFileSizeFromAws(fileKey: FileKey): F[Long] =
        s3Ops
        .headObject(
          HeadObjectRequest
            .builder()
            .bucket(awsConfig.bucketName.value.value)
            .key(fileKey.value.value)
            .build
        )
        .map(_.contentLength())

      // TODO: eventually will probably want to check for write access for uploading/deleting files.
      def withAccess[A](user: User, programId: Program.Id)(f: F[A]): F[A] = user match {
        // guest users not allowed to upload files - at least for now.
        case GuestUser(_) => Async[F].raiseError(AttachmentException.Forbidden)
        case _            =>
          ProgramService.fromSessionAndUser(session, user)
            .userHasAccess(programId)
            .flatMap(hasAccess => 
              if (hasAccess) f
              else Async[F].raiseError(AttachmentException.Forbidden)
            )
      }
      

      def isTypeValid(attachmentType: Tag): F[Boolean] = {
        val af = Statements.existsAttachmentType(attachmentType)
        val stmt = sql"Select ${af.fragment}".query(bool)
        session.prepareR(stmt).use { pg =>
          pg.unique(af.argument)
        }
      }

      def withValidType[A](attachmentType: Tag)(f: F[A]): F[A] = {
        isTypeValid(attachmentType).flatMap(isValid =>
          if (isValid) f
          else Async[F].raiseError(
            AttachmentException.InvalidType(s"Invalid attachment type"))
        )
      }

      def insertOrUpdateAttachment(
        programId:      Program.Id,
        attachmentType: Tag,
        fileName:       FileName,
        description:    Option[NonEmptyString],
        fileSize: Long
      ): F[Attachment.Id] = 
        Trace[F].span("insertOrUpdateAttachment") {
          session.prepareR(Statements.InsertOrUpdateAttachment).use(pg =>
            pg.unique(programId ~ attachmentType ~ fileName.value ~ description ~ fileSize)
          )
        }

      def getAttachmentFileNameFromDB(
        user:         User,
        programId:    Program.Id,
        attachmentId: Attachment.Id
      ): F[NonEmptyString] = 
        Trace[F].span("getAttachmentFileNameFromDB"){
          val af   = Statements.getAttachmentFileName(user, programId, attachmentId)
          val stmt = af.fragment.query(varchar)

          session.prepareR(stmt).use( pg =>
            pg.option(af.argument)
              .flatMap{
                case None    => Async[F].raiseError(AttachmentException.FileNotFound)
                case Some(s) => 
                  // should be non empty because of a database check
                  NonEmptyString.from(s)
                    .toOption
                    .fold(Async[F].raiseError(AttachmentException.InvalidName("File name is missing")))(
                      Async[F].delay)
              }
          )
        }

      def deleteAttachmentFromDB(
        user:         User,
        programId:    Program.Id,
        attachmentId: Attachment.Id
      ): F[NonEmptyString] = 
        Trace[F].span("deleteAttachmentFromDB") {
          val af   = Statements.deleteAttachment(user, programId, attachmentId)
          val stmt = af.fragment.query(varchar)

          session.prepareR(stmt).use( pg =>
            pg.option(af.argument)
              .flatMap{
                case None    => Async[F].raiseError(AttachmentException.FileNotFound)
                case Some(s) => 
                  // should be non empty because of a database check
                  NonEmptyString.from(s)
                    .toOption
                    .fold(Async[F].raiseError(AttachmentException.InvalidName("File name is missing")))(
                      Async[F].delay)
              }
          )
        }

      new AttachmentService[F] {

        def getAttachment(
          user:         User,
          programId:    Program.Id,
          attachmentId: Attachment.Id
        ): F[Either[AttachmentException, Stream[F, Byte]]] =
          withAccess(user, programId) {
            getAttachmentFileNameFromDB(user, programId, attachmentId).flatMap(fileName =>
              val fileKey = awsConfig.fileKey(programId, fileName)
              val stream  = s3.readFileMultipart(awsConfig.bucketName, fileKey, partSize)

              getFileSizeFromAws(fileKey) // make sure the file exists and we can read it
                .map(_ => stream.asRight)
            )
          }.recover { 
            case _: NoSuchKeyException  => AttachmentException.FileNotFound.asLeft
            case e: AttachmentException => e.asLeft
          }

        def uploadAttachment(
          user:             User, 
          programId:        Program.Id, 
          attachmentType:   Tag,
          fileName:         String, 
          description:      Option[NonEmptyString],
          data: Stream[F, Byte]): F[Attachment.Id] =
          withAccess(user, programId) {
            withValidType(attachmentType) {
              FileName.fromString(fileName).fold(e => Async[F].raiseError(e), fn =>
                // TODO: Validate the file extension based on attachment type
                val fileKey = awsConfig.fileKey(programId, fn.value)

                for {
                  ref <- Ref.of(0)
                  pipe = s3.uploadFileMultipart(awsConfig.bucketName, fileKey, partSize)
                  aws <- data.evalTapChunk(c => ref.update(_ + 1)).through(pipe).compile.drain
                  size <- ref.get
                  result <- insertOrUpdateAttachment(programId, attachmentType, fn, description, size)
                } yield result
              ) 
            }
          }

        def deleteAttachment(user: User, programId: Program.Id, attachmentId: Attachment.Id): F[Unit] = 
          withAccess(user, programId){
            for {
              fileName <- deleteAttachmentFromDB(user, programId, attachmentId)
              _        <- remoteDeleteFile(programId, fileName)
            } yield ()
          }
        
        def remoteDeleteFile(programId: Program.Id, fileName: NonEmptyString): F[Unit] =
          s3.delete(awsConfig.bucketName, awsConfig.fileKey(programId, fileName))
      }
    }
  }

  object Statements {

    val InsertOrUpdateAttachment: 
      Query[Program.Id ~ Tag ~ NonEmptyString ~ Option[NonEmptyString] ~ Long, Attachment.Id] = 
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_description,
          c_file_size
        ) VALUES (
          ${program_id},
          ${tag},
          ${text_nonempty},
          ${text_nonempty.opt},
          ${int8}
        )
        ON CONFLICT (c_program_id, c_file_name) DO UPDATE SET
          c_attachment_type = EXCLUDED.c_attachment_type,
          c_description     = EXCLUDED.c_description,
          c_checked         = false,
          c_file_size       = EXCLUDED.c_file_size
        RETURNING c_attachment_id
      """.query(attachment_id)

    def getAttachmentFileName(user: User, programId: Program.Id, attachmentId: Attachment.Id): AppliedFragment = {
      sql"""
        SELECT c_file_name
        FROM t_attachment
        WHERE c_program_id = $program_id AND c_attachment_id = $attachment_id
      """.apply(programId, attachmentId) |+|
        accessFrag(user, programId)
    }

    // returns the file name
    def deleteAttachment(user: User, programId: Program.Id, attachmentId: Attachment.Id): AppliedFragment = 
      sql"""
        DELETE FROM t_attachment
        WHERE c_program_id = $program_id AND c_attachment_id = $attachment_id
      """.apply(programId, attachmentId) |+|
        accessFrag(user, programId) |+|
        void"RETURNING c_file_name"

    def existsAttachmentType(attachmentType: Tag): AppliedFragment =
      sql"""
        EXISTS (select c_tag from t_attachment_type where c_tag = $tag)
      """.apply(attachmentType)


    private def accessFrag(user: User, programId: Program.Id): AppliedFragment =
      ProgramService.Statements.existsUserAccess(user, programId).fold(AppliedFragment.empty){ af =>
        void"AND " |+| af
      }
  }
}
