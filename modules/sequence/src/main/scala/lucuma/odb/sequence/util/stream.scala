// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.Order
import cats.syntax.either.*
import fs2.Stream
import lucuma.core.util.Timestamp

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