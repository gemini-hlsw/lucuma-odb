// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.ExecutionState
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.StepRecord

/**
 * Combines the static configuration with generators for the acquisition and
 * science sequences.  The remaining acquisition and science sequences can
 * then be produced by `executionConfig` supplying the past visits and steps.
 */
case class ExecutionConfigGenerator[S, D](
  static:      S,
  acquisition: SequenceGenerator[D],
  science:     SequenceGenerator[D]
):

  /**
   * Given the observation's stream of past visits and steps, produces the
   * remaining execution config (sequences) for the future for both acquisition
   * and science.
   *
   * @param steps past steps
   * @param when when the sequence is requested.  This is relevant because
   *             calibration files are only considered valid for a fixed time
   */
  def executionConfig[F[_]: Concurrent](
    steps: Stream[F, StepRecord[D]],
    when:  Timestamp
  )(using Eq[D]): F[(ProtoExecutionConfig[S, Atom[D]], ExecutionState)] =
    steps
      .fold((acquisition, science, ExecutionState.NotStarted)) { case ((a, s, e), step) =>
        (a.recordStep(step), s.recordStep(step), ExecutionState.Ongoing)
      }
      .compile
      .onlyOrError
      .map: (a, s, e) =>
        (ProtoExecutionConfig(static, a.generate(when), s.generate(when)), e)