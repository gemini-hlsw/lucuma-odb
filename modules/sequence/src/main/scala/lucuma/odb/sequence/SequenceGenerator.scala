// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import fs2.Pure
import fs2.Stream
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.VisitRecord

/**
 * A sequence generator.  Record past steps and vists in order until all have
 * been accounted for and then `generate` the remaining sequence.
 *
 * @tparam D dynamic instrument config type
 */
trait SequenceGenerator[D]:

  /**
   * Remaining atoms and their steps for this sequence, if it were being
   * generated at timestamp 'when'.  The sequence may change depending on when
   * it is generated because, for example, past calibrations eventually expire
   * and need to be repeated.
   *
   * @param when referential time at which the sequence will be generated; does
   *             not delay the calculation in anyway
   */
  def generate(when: Timestamp): Stream[Pure, Atom[D]]

  /**
   * Records a step and returns an updated generator.
   */
  def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D]

  /**
   * Records a visit and returns an updated generator.
   */
  def recordVisit(visit: VisitRecord): SequenceGenerator[D]