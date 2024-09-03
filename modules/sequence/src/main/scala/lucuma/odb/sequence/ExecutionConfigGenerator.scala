// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import cats.effect.Concurrent
import cats.syntax.functor.*
import fs2.Stream
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord

case class ExecutionConfigGenerator[S, D](
  static:      S,
  acquisition: SequenceGenerator[D],
  science:     SequenceGenerator[D]
):
  def executionConfig[F[_]: Concurrent](
    steps: Stream[F, StepRecord[D]],
    time:  Timestamp
  )(using Eq[D]): F[ProtoExecutionConfig[S, (ProtoAtom[(ProtoStep[D], Int)], Int)]] =
    steps.fold((acquisition, science)) { case ((a, s), step) =>
      (a.record(step), s.record(step))
    }.compile.onlyOrError.map { (a, s) =>
      ProtoExecutionConfig(static, a.generate(time), s.generate(time))
    }