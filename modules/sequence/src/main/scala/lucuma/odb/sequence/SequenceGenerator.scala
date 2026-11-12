// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import fs2.Pure
import fs2.Stream
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepViolation

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

  /**
   * Problems found while preparing the sequence that no step of it shows,
   * such as calibrations left out for want of a Smart GCAL definition.
   */
  def violations: List[StepViolation] =
    List.empty

object SequenceGenerator:

  extension [D](self: SequenceGenerator[D])
    /** The same sequence, reporting `missing` as well. */
    def reporting(missing: List[StepViolation]): SequenceGenerator[D] =
      new SequenceGenerator[D]:
        override def generate: Stream[Pure, Atom[D]] =
          self.generate
        override def violations: List[StepViolation] =
          self.violations ++ missing

  /** A degenerate implementation that produces no atoms. */
  def empty[D]: SequenceGenerator[D] =
    new SequenceGenerator[D]:
      override val generate: Stream[Pure, Atom[D]] =
        Stream.empty