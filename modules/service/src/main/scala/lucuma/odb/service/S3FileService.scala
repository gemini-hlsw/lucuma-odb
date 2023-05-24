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
import lucuma.core.model.Program
import lucuma.odb.Config
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
  def get(filePath: NonEmptyString): Stream[F, Byte]

  /** Get metatada about the S3 file */
  def getMetadata(filePath: NonEmptyString): F[HeadObjectResponse]

  /**
    * A convenience method for verifying eistence of and access to the file.
    * In Http4s, if we don't do this check first and there is an issue, the request 
    * terminates without a response code. This allows the request to at least finish
    * with Internal Server Error most of the time.
    */
  def verifyFileAcess(filePath: NonEmptyString): F[Unit]

  /** A meta-convenience method combining verify and get */
  def verifyAndGet(filePath: NonEmptyString): F[Stream[F, Byte]]

  /** Uploads a stream to S3 */
  def upload(filePath: NonEmptyString, data: Stream[F, Byte]): F[Long]

  /** Deletes a file from S3 */
  def delete(filePath: NonEmptyString): F[Unit]

  def presignedUrl(filePath: NonEmptyString): F[String]

  def filePath(programId: Program.Id, uuid: UUID, fileName: NonEmptyString): NonEmptyString
}

object S3FileService {

  // We can switch back to unsafeFrom when a new version of refined is out that fixes
  // https://github.com/fthomas/refined/issues/1161 or a version of fs2-aws > 6.0.0
  // is availabile with my PR that changes PartSizeMB to "GreaterEqual[5]"
  // val partSize = PartSizeMB.unsafeFrom(5)
  val partSize = refineV[Greater[5] Or Equal[5]](5).toOption.get

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

      def get(filePath: NonEmptyString): Stream[F, Byte] =
        s3.readFileMultipart(awsConfig.bucketName, filePath.toKey, partSize)

      def getMetadata(filePath: NonEmptyString): F[HeadObjectResponse] =
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

      def verifyFileAcess(filePath: NonEmptyString): F[Unit] =
        getMetadata(filePath).void

      def verifyAndGet(filePath: NonEmptyString): F[Stream[F, Byte]] =
        verifyFileAcess(filePath).map(_ => get(filePath))

      def upload(filePath: NonEmptyString, data: Stream[F, Byte]): F[Long] =
        Trace[F].span(s"uploading remote file with file key: ${filePath.toKey}") {
          val f = for {
            ref  <- Ref.of(0L)
            pipe  = s3.uploadFileMultipart(awsConfig.bucketName, filePath.toKey, partSize)
            _    <- data.evalTapChunk(c => ref.update(_ + 1)).through(pipe).compile.drain
            size <- ref.get
          } yield size
          f.onError { case e => Trace[F].attachError(e, ("error", true)) }
        }

      def delete(filePath: NonEmptyString): F[Unit] =
        Trace[F].span(s"deleting remote file with file key: ${filePath.toKey}") {
          s3.delete(awsConfig.bucketName, filePath.toKey)
            .onError { case e => Trace[F].attachError(e, ("error", true)) }
        }

      def presignedUrl(filePath: NonEmptyString): F[String] =
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

      def filePath(programId: Program.Id, uuid: UUID, fileName: NonEmptyString): NonEmptyString =
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
