// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import fs2.Stream
import lucuma.core.model.sequence.Atom

/**
 * A precursor to a lucuma.core.model.sequence.ExecutionConfig, which contains
 * a concrete list of steps.  The StreamingExecutionConfig provides a `Stream`
 * of steps of a generic type because there are multiple uses for a sequence.
 * For example to calculate the sequence digest we never need to hold the whole
 * sequence in memory but instead can fold over the stream.
 *
 * @tparam S static execution config
 * @tparam A step type
 */
case class StreamingExecutionConfig[F[_], S, D](
    static:      S,
    acquisition: Stream[F, Atom[D]],
    science:     Stream[F, Atom[D]]
)