// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.aws.s3.S3
import fs2.aws.s3.models.Models.FileKey
import fs2.aws.s3.models.Models.PartSizeMB
import io.laserdisc.pure.s3.tagless.Interpreter
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.model.Program
import lucuma.odb.Config
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.refined.*
import natchez.Trace
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.S3Configuration
import software.amazon.awssdk.services.s3.model.GetObjectRequest
import software.amazon.awssdk.services.s3.model.HeadObjectRequest
import software.amazon.awssdk.services.s3.model.HeadObjectResponse
import software.amazon.awssdk.services.s3.presigner.S3Presigner
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest

import java.time.Duration
import java.util.UUID

/**
  * Service for dealing with the files in S3.
  * Because we are careful to only have file metadata in the database for
    files we are certain are in S3, we don't handle any errors here. If we
    get an error from S3, it is either a configuration problem, a network issue,
     or our DB is out of sync.
  */
trait S3FileService[F[_]] {

  /** Get the file as a stream */
  def get(filePath: NonEmptyString)(using SuperUserAccess): Stream[F, Byte]

  /** Get metatada about the S3 file */
  def getMetadata(filePath: NonEmptyString)(using SuperUserAccess): F[HeadObjectResponse]

  /**
    * A convenience method for verifying eistence of and access to the file.
    * In Http4s, if we don't do this check first and there is an issue, the request
    * terminates without a response code. This allows the request to at least finish
    * with Internal Server Error most of the time.
    */
  def verifyFileAcess(filePath: NonEmptyString)(using SuperUserAccess): F[Unit]

  /** A meta-convenience method combining verify and get */
  def verifyAndGet(filePath: NonEmptyString)(using SuperUserAccess): F[Stream[F, Byte]]

  /** Uploads a stream to S3 */
  def upload(filePath: NonEmptyString, data: Stream[F, Byte])(using SuperUserAccess): F[Long]

  /** Deletes a file from S3 */
  def delete(filePath: NonEmptyString)(using SuperUserAccess): F[Unit]

  def presignedUrl(filePath: NonEmptyString)(using SuperUserAccess): F[String]

  def filePath(programId: Program.Id, uuid: UUID, fileName: NonEmptyString)(using SuperUserAccess): NonEmptyString
}

object S3FileService {
  val partSize: PartSizeMB = 5.refined

  def noop[F[_]: Applicative]: S3FileService[F] =
    new S3FileService[F] {
      def get(filePath: NonEmptyString)(using SuperUserAccess): Stream[F, Byte] =
        Stream.empty

      def getMetadata(filePath: NonEmptyString)(using SuperUserAccess): F[HeadObjectResponse] =
        HeadObjectResponse.builder().build().pure[F]

      def verifyFileAcess(filePath: NonEmptyString)(using SuperUserAccess): F[Unit] =
        Applicative[F].unit

      def verifyAndGet(filePath: NonEmptyString)(using SuperUserAccess): F[Stream[F, Byte]] =
        Stream.empty.pure[F]

      def upload(filePath: NonEmptyString, data: Stream[F, Byte])(using SuperUserAccess): F[Long] =
        0L.pure[F]

      def delete(filePath: NonEmptyString)(using SuperUserAccess): F[Unit] =
        Applicative[F].unit

      def presignedUrl(filePath: NonEmptyString)(using SuperUserAccess): F[String] =
        "".pure[F]

      def filePath(programId: Program.Id, uuid: UUID, fileName: NonEmptyString)(using SuperUserAccess): NonEmptyString =
        fileName
    }

  def fromS3ConfigAndClient[F[_]: Async: Trace](
    awsConfig: Config.Aws,
    s3Ops:     S3AsyncClientOp[F],
    presigner: S3Presigner
  ): S3FileService[F] = {

    val s3 = S3.create[F](s3Ops)

    extension (filePath: NonEmptyString)
      def toKey: FileKey = awsConfig.fileKey(filePath)
      def toKeyString: String = toKey.value.value

    new S3FileService[F] {

      def get(filePath: NonEmptyString)(using SuperUserAccess): Stream[F, Byte] =
        s3.readFileMultipart(awsConfig.bucketName, filePath.toKey, partSize)

      def getMetadata(filePath: NonEmptyString)(using SuperUserAccess): F[HeadObjectResponse] =
        Trace[F].span(s"get remote file metadata for file key: ${filePath.toKey}") {
          s3Ops
            .headObject(
              HeadObjectRequest
                .builder()
                .bucket(awsConfig.bucketName.value.value)
                .key(filePath.toKeyString)
                .build
            )
            .onError { case e => Trace[F].attachError(e, ("error", true)) }
        }

      def verifyFileAcess(filePath: NonEmptyString)(using SuperUserAccess): F[Unit] =
        getMetadata(filePath).void

      def verifyAndGet(filePath: NonEmptyString)(using SuperUserAccess): F[Stream[F, Byte]] =
        verifyFileAcess(filePath).map(_ => get(filePath))

      def upload(filePath: NonEmptyString, data: Stream[F, Byte])(using SuperUserAccess): F[Long] =
        Trace[F].span(s"uploading remote file with file key: ${filePath.toKey}") {
          val f = for {
            ref  <- Ref.of(0L)
            pipe  = s3.uploadFileMultipart(awsConfig.bucketName, filePath.toKey, partSize)
            _    <- data.chunks.evalTap(c => ref.update(_ + c.size)).unchunks.through(pipe).compile.drain
            size <- ref.get
          } yield size
          f.onError { case e => Trace[F].attachError(e, ("error", true)) }
        }

      def delete(filePath: NonEmptyString)(using SuperUserAccess): F[Unit] =
        Trace[F].span(s"deleting remote file with file key: ${filePath.toKey}") {
          s3.delete(awsConfig.bucketName, filePath.toKey)
            .onError { case e => Trace[F].attachError(e, ("error", true)) }
        }

      def presignedUrl(filePath: NonEmptyString)(using SuperUserAccess): F[String] =
        Trace[F].span(s"getting presigned URL for file key: ${filePath.toKey}") {
          val objectRequest = GetObjectRequest
            .builder()
            .bucket(awsConfig.bucketName.value.value)
            .key(filePath.toKeyString)
            .build

          val presignRequest = GetObjectPresignRequest
            .builder()
            .signatureDuration(Duration.ofMinutes(120))
            .getObjectRequest(objectRequest)
            .build

          Async[F].blocking(presigner.presignGetObject(presignRequest)).map(_.url().toString)
            .onError { case e => Trace[F].attachError(e, ("error", true)) }
        }

      def filePath(programId: Program.Id, uuid: UUID, fileName: NonEmptyString)(using SuperUserAccess): NonEmptyString =
        awsConfig.filePath(programId, uuid, fileName)
    }
  }

  def s3PresignerResource[F[_]: Async](awsConfig: Config.Aws): Resource[F, S3Presigner] = {
    val credentials =
      AwsBasicCredentials.create(awsConfig.accessKey.value, awsConfig.secretKey.value)
    val builder     =
      S3Presigner
        .builder()
        .credentialsProvider(StaticCredentialsProvider.create(credentials))
        .serviceConfiguration(
          S3Configuration
            .builder()
            .pathStyleAccessEnabled(true)
            .build()
        )
        .region(Region.US_EAST_1)

    Resource.fromAutoCloseable(Async[F].delay(builder.build()))
  }

  /** A resource that encapsulates an s3 async client */
  def s3AsyncClientOpsResource[F[_]: Async](awsConfig: Config.Aws): Resource[F, S3AsyncClientOp[F]] = {
    val credentials = AwsBasicCredentials.create(awsConfig.accessKey.value, awsConfig.secretKey.value)

    Interpreter[F].S3AsyncClientOpResource(
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
  }
}
