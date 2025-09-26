// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import cats.Applicative
import cats.effect.Async
import cats.syntax.all.*
import io.chrisdavenport.keysemaphore.KeySemaphore
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

object NoOpBinaryCache:

  def apply[F[_]: Async: Logger]: F[BinaryEffectfulCache[F]] =
    KeySemaphore
      .of[F, Array[Byte]](_ => 1)
      .map: sem =>
        new BinaryEffectfulCache[F] {

          override protected val keySemaphore: KeySemaphore[F, Array[Byte]] =
            sem

          protected def read(key: Array[Byte]): F[Option[Array[Byte]]] =
            none.pure[F]

          protected def write(
            key:   Array[Byte],
            value: Array[Byte],
            ttl:   Option[FiniteDuration]
          ): F[Unit] =
            Applicative[F].unit

          protected def delete(key: Array[Byte]): F[Unit] =
            Applicative[F].unit

          def flush: F[Unit] =
            Applicative[F].unit
        }
