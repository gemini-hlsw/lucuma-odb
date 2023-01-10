// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import lucuma.core.model.sequence.StepConfig
import monocle.Focus
import monocle.Lens

/**
 * The complete instrument and step configuration, without a step id.
 */
final case class ProtoStep[D](
  instrumentConfig: D,
  stepConfig:       StepConfig
)

object ProtoStep {

  def instrumentConfig[D]: Lens[ProtoStep[D], D] =
    Focus[ProtoStep[D]](_.instrumentConfig)

  def stepConfig[D]: Lens[ProtoStep[D], StepConfig] =
    Focus[ProtoStep[D]](_.stepConfig)

}