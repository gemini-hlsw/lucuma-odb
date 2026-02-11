// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import fs2.Pure
import fs2.Stream
import lucuma.core.model.sequence.Atom

/**
 * A sequence generator.
 *
 * @tparam D dynamic instrument config type
 */
trait SequenceGenerator[D]:

  /**
   * Atoms and their steps for this sequence.
   */
  def generate: Stream[Pure, Atom[D]]

object SequenceGenerator:

  /** A degenerate implementation that produces no atoms. */
  def empty[D]: SequenceGenerator[D] =
    new SequenceGenerator[D]:
      override val generate: Stream[Pure, Atom[D]] =
        Stream.empty