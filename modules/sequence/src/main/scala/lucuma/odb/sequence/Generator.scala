// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.Completion
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep

/**
 * Sequence generator.
 *
 * @tparam F effect type
 * @tparam S static instrument config type
 * @tparam D dynamic instrument config type
 */
trait Generator[F[_], S, D] {

  /**
   * Generates the execution configuration for acquisition and science, tupled
   * with its index in the sequence and filtering out any already executed steps.
   * The result will be a `Left`` if the sequence cannot be generated, and
   * individual steps in the sequence will be `Left`` if they are Smart Gcal
   * steps for which no matching GCal configuration was found.
   */
  def generate(
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime,
    completion:     Completion.Matcher[D]
  ): Either[String, ProtoExecutionConfig[F, S, Either[String, (ProtoAtom[ProtoStep[D]], Long)]]]

}