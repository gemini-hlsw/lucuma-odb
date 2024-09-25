// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import fs2.Pure
import fs2.Stream
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
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
   * Remaining atoms and their steps for this sequence.  Here the `Int` part
   * of the tuples (atom and step counts) is an unfortunate requirement to
   * maintain stable atom ids as required by clients.
   */
  def generate(t: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)]

  /**
   * Records a step and returns an updated generator.
   */
  def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D]

  /**
   * Records a visit and returns an updated generator.
   */
  def recordVisit(visit: VisitRecord): SequenceGenerator[D]