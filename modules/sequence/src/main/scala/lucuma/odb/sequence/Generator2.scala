// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import fs2.Stream
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord


/**
 * Sequence generator.
 *
 * @tparam F effect type
 * @tparam S static instrument config type
 * @tparam D dynamic instrument config type
 */
trait Generator2[F[_], S, D] {

  /**
   * Generates the execution configuration for acquisition and science, tupled
   * with its index in the sequence and filtering out any already executed steps.
   * Individual steps in the sequence will be `Left`` if they are Smart Gcal
   * steps for which no matching GCal configuration was found.
   */
  def generate(
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime,
    stepRecords:    Stream[F, StepRecord[D]]
  ): ProtoExecutionConfig[F, S, Either[String, (ProtoAtom[ProtoStep[D]], Long)]]

}