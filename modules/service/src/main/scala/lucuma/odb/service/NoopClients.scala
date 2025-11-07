// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import java.util.UUID

import cats.effect.kernel.Concurrent
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model.Program
import lucuma.odb.service.Services.SuperUserAccess
import software.amazon.awssdk.services.s3.model.HeadObjectResponse

/**
 * Provides noop implementations of external service clients for contexts where
 * those services are not needed or available.
 */
object NoopClients {

  /**
   * A noop S3 file service that always fails with an error indicating the service
   * is unavailable.
   */
  def noopS3FileService[F[_]: Concurrent]: S3FileService[F] =
    new S3FileService[F] {
      private def unavailable[A]: F[A] =
        Concurrent[F].raiseError(
          new UnsupportedOperationException("S3 file service not available in this context")
        )

      override def get(filePath: NonEmptyString)(using SuperUserAccess): Stream[F, Byte] =
        Stream.raiseError(
          new UnsupportedOperationException("S3 file service not available in this context")
        )

      override def getMetadata(filePath: NonEmptyString)(using SuperUserAccess): F[HeadObjectResponse] =
        unavailable

      override def verifyFileAcess(filePath: NonEmptyString)(using SuperUserAccess): F[Unit] =
        unavailable

      override def verifyAndGet(filePath: NonEmptyString)(using SuperUserAccess): F[Stream[F, Byte]] =
        unavailable

      override def upload(
        filePath: NonEmptyString,
        data:     Stream[F, Byte]
      )(using SuperUserAccess): F[Long] =
        unavailable

      override def delete(filePath: NonEmptyString)(using SuperUserAccess): F[Unit] =
        unavailable

      override def presignedUrl(filePath: NonEmptyString)(using SuperUserAccess): F[String] =
        unavailable

      override def filePath(
        programId: Program.Id,
        uuid:      UUID,
        fileName:  NonEmptyString
      )(using SuperUserAccess): NonEmptyString =
        throw new UnsupportedOperationException("S3 file service not available in this context")
    }
}
