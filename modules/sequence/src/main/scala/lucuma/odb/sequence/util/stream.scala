// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.Order
import cats.syntax.either.*
import fs2.Stream
import lucuma.core.util.Timestamp
import lucuma.odb.data.OneOf3
import lucuma.odb.data.OneOf4

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

  given Order[Either[A, B]] =
    Order.by:
      case Left(a)  => fa(a)
      case Right(b) => fb(b)

  as.map(_.asLeft[B])
    .interleaveOrdered(bs.map(_.asRight[A]))

/**
 * Merges three (ordered) streams by the `Timestamp`s of their elements to
 * produce an ordered `Stream` of `OneOf3`.
 *
 * @param as a `Timestamp`-ordered `fs2.Stream[F, A]`
 * @param bs a `Timestamp`-ordered `fs2.Stream[F, B]`
 * @param cs a `Timestamp`-ordered `fs2.Stream[F, C]`
 * @param fa extracts a `Timestamp` from an `A`
 * @param fb extracts a `Timestamp` from a `B`
 * @param fc extracts a `Timestamp` from a `C`
 *
 * @return an ordered `Stream` of `OneOf3[A, B, C]` where the order is
 *         determined by each element's corresponding `Timestamp`
 */
def merge3ByTimestamp[F[_], A, B, C](
  as: Stream[F, A],
  bs: Stream[F, B],
  cs: Stream[F, C]
)(
  fa: A => Timestamp,
  fb: B => Timestamp,
  fc: C => Timestamp
): Stream[F, OneOf3[A, B, C]] =

  given Order[OneOf3[A, B, C]] =
    Order.by(_.fold(fa, fb, fc))

  val sa = as.map(a => OneOf3.first(a): OneOf3[A, B, C])
  val sb = bs.map(OneOf3.second)
  val sc = cs.map(OneOf3.third)

  sa.interleaveOrdered(sb).interleaveOrdered(sc)

/**
 * Merges four (ordered) streams by the `Timestamp`s of their elements to
 * produce an ordered `Stream` of `OneOf4`.
 *
 * @param as a `Timestamp`-ordered `fs2.Stream[F, A]`
 * @param bs a `Timestamp`-ordered `fs2.Stream[F, B]`
 * @param cs a `Timestamp`-ordered `fs2.Stream[F, C]`
 * @param ds a `Timestamp`-ordered `fs2.Stream[F, D]`
 * @param fa extracts a `Timestamp` from an `A`
 * @param fb extracts a `Timestamp` from a `B`
 * @param fc extracts a `Timestamp` from a `C`
 * @param fd extracts a `Timestamp` from a `D`
 *
 * @return an ordered `Stream` of `OneOf4[A, B, C, D]` where the order is
 *         determined by each element's corresponding `Timestamp`
 */
def merge4ByTimestamp[F[_], A, B, C, D](
  as: Stream[F, A],
  bs: Stream[F, B],
  cs: Stream[F, C],
  ds: Stream[F, D]
)(
  fa: A => Timestamp,
  fb: B => Timestamp,
  fc: C => Timestamp,
  fd: D => Timestamp
): Stream[F, OneOf4[A, B, C, D]] =

  given Order[OneOf4[A, B, C, D]] =
    Order.by(_.fold(fa, fb, fc, fd))

  val sa = as.map(a => OneOf4.first(a): OneOf4[A, B, C, D])
  val sb = bs.map(OneOf4.second)
  val sc = cs.map(OneOf4.third)
  val sd = ds.map(OneOf4.fourth)

  sa.interleaveOrdered(sb).interleaveOrdered(sc).interleaveOrdered(sd)