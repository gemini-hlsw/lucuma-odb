// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Traverse
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import fs2.Pure
import fs2.Stream

case class ProtoExecutionConfig[F[_], S, A](
  static:      S,
  acquisition: Stream[F, A],
  science:     Stream[F, A]
) {

  def map[B](f: A => B): ProtoExecutionConfig[F, S, B] =
    ProtoExecutionConfig[F, S, B](static, acquisition.map(f), science.map(f))

  def mapSequences[F2[x] >: F[x], B](
    fa: Stream[F2, A] => Stream[F2, B],
    fs: Stream[F2, A] => Stream[F2, B]
  ): ProtoExecutionConfig[F2, S, B] =
    ProtoExecutionConfig(
      static,
      fa(acquisition),
      fs(science)
    )

  def mapBothSequences[F2[x] >: F[x], B](
    f: Stream[F2, A] => Stream[F2, B]
  ): ProtoExecutionConfig[F2, S, B] =
    mapSequences(f, f)

}