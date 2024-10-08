// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.Order
import cats.syntax.either.*
import fs2.Stream
import lucuma.core.util.Timestamp

/**
 * Merges two (ordered) streams by the `Timestamp`s of their elements to produce
 * an ordered `Stream` of `Either`.
 *
 * @param as a `Timestamp`-ordered `fs2.Stream[F, A]`
 * @param bs a `Timestamp`-ordered `fs2.Stream[F, B]`
 * @param fa extracts a `Timestamp` from an `A`
 * @param fb extracts a `Timestamp` from a `B`
 *
 * @return an ordered `Stream` of `Either[A, B]` where the order is determined
 *         by each element's corresponding `Timestamp`
 */
def mergeByTimestamp[F[_], A, B](
  as: Stream[F, A],
  bs: Stream[F, B]
)(
  fa: A => Timestamp,
  fb: B => Timestamp
): Stream[F, Either[A, B]] =

  val TimestampOrder: Order[Either[A, B]] =
    Order.by {
      case Left(a)  => fa(a)
      case Right(b) => fb(b)
    }

  as.map(_.asLeft[B])
    .interleaveOrdered(bs.map(_.asRight[A]))(using TimestampOrder)