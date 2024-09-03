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

/**
 * Sequence generator.
 *
 * @tparam D dynamic instrument config type
 */
trait SequenceGenerator[D]:

  def generate(t: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)]

  def record(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D]