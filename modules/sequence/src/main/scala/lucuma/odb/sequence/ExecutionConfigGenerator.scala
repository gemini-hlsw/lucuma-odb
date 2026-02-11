// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import fs2.Pure
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.StreamingExecutionConfig

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

  def streamingExecutionConfig: StreamingExecutionConfig[Pure, S, D] =
    StreamingExecutionConfig(
      static,
      acquisition.generate(Timestamp.Min),
      science.generate(Timestamp.Min)
    )