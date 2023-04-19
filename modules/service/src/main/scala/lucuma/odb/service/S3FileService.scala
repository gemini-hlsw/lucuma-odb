// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.effect.Ref
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
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.core.model.Program
import lucuma.odb.Config
import natchez.Trace
import software.amazon.awssdk.services.s3.model.HeadObjectRequest
import software.amazon.awssdk.services.s3.model.HeadObjectResponse

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
  def get(programId: Program.Id, remoteId: UUID): Stream[F, Byte]

  /** Get metatada about the S3 file */
  def getMetadata(programId: Program.Id, remoteId: UUID): F[HeadObjectResponse]

  /**
    * A convenience method for verifying eistence of and access to the file.
    * In Http4s, if we don't do this check first and there is an issue, the request 
    * terminates without a response code. This allows the request to at least finish
    * with Internal Server Error most of the time.
    */
  def verifyFileAcess(programId: Program.Id, remoteId: UUID): F[Unit]

  /** A meta-convenience method combining verify and get */
  def verifyAndGet(programId: Program.Id, remoteId: UUID): F[Stream[F, Byte]]

  /** Uploads a stream to S3 */
  def upload(programId: Program.Id, remoteId: UUID, data: Stream[F, Byte]): F[Long]

  /** Deletes a file from S3 */
  def delete(programId: Program.Id, remoteId: UUID): F[Unit]
}

object S3FileService {

  // We can switch back to unsafeFrom when a new version of refined is out that fixes
  // https://github.com/fthomas/refined/issues/1161 or a version of fs2-aws > 6.0.0
  // is availabile with my PR that changes PartSizeMB to "GreaterEqual[5]"
  // val partSize = PartSizeMB.unsafeFrom(5)
  val partSize = refineV[Greater[5] Or Equal[5]](5).toOption.get

  def fromS3ConfigAndClient[F[_]: Async: Trace](
    awsConfig: Config.Aws,
    s3Ops:     S3AsyncClientOp[F]
  ): S3FileService[F] = {

    val s3 = S3.create[F](s3Ops)

    new S3FileService[F] {

      def get(programId: Program.Id, remoteId: UUID): Stream[F, Byte] = {
        val fileKey = awsConfig.obsFileKey(programId, remoteId)
        s3.readFileMultipart(awsConfig.bucketName, fileKey, partSize)
      }

      def getMetadata(
        programId: Program.Id,
        remoteId:  UUID
      ): F[HeadObjectResponse] = {
        val fileKey = awsConfig.obsFileKey(programId, remoteId)
        Trace[F].span(s"get remote file metadata for file key: $fileKey") {
          s3Ops
            .headObject(
              HeadObjectRequest
                .builder()
                .bucket(awsConfig.bucketName.value.value)
                .key(fileKey.value.value)
                .build
            )
            .onError { case e => Trace[F].attachError(e, ("error", true)) }
        }
      }

      def verifyFileAcess(programId: Program.Id, remoteId: UUID): F[Unit] =
        getMetadata(programId, remoteId).void

      def verifyAndGet(programId: Program.Id, remoteId: UUID): F[Stream[F, Byte]] = 
        verifyFileAcess(programId, remoteId).map(_ => get(programId, remoteId))

      def upload(
        programId: Program.Id,
        remoteId:  UUID,
        data:      Stream[F, Byte]
      ): F[Long] = {
        val fileKey = awsConfig.obsFileKey(programId, remoteId)
        Trace[F].span(s"uploading remote file with file key: $fileKey") {
          val f = for {
            ref  <- Ref.of(0L)
            pipe  = s3.uploadFileMultipart(awsConfig.bucketName, fileKey, partSize)
            _    <- data.evalTapChunk(c => ref.update(_ + 1)).through(pipe).compile.drain
            size <- ref.get
          } yield size
          f.onError { case e => Trace[F].attachError(e, ("error", true)) }
        }
      }

      def delete(programId: Program.Id, remoteId: UUID): F[Unit] = {
        val fileKey = awsConfig.obsFileKey(programId, remoteId)
        Trace[F].span(s"deleting remote file with file key: $fileKey") {
          s3.delete(awsConfig.bucketName, fileKey)
            .onError { case e => Trace[F].attachError(e, ("error", true)) }
        }
      }
    }
  }
}
