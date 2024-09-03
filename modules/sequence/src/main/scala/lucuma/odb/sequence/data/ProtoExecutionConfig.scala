// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.apply.*
import fs2.Pipe
import fs2.Pure
import fs2.Stream

case class ProtoExecutionConfig[S, A](
  static:      S,
  acquisition: Stream[Pure, A],
  science:     Stream[Pure, A]
) {

  def map[B](f: A => B): ProtoExecutionConfig[S, B] =
    ProtoExecutionConfig[S, B](static, acquisition.map(f), science.map(f))

  def pipeSequences[B](
    fa: Pipe[Pure, A, B],
    fs: Pipe[Pure, A, B]
  ): ProtoExecutionConfig[S, B] =
    ProtoExecutionConfig(
      static,
      fa(acquisition),
      fs(science)
    )

  def pipeBothSequences[B](f: Pipe[Pure, A, B]): ProtoExecutionConfig[S, B] =
    pipeSequences(f, f)

}
