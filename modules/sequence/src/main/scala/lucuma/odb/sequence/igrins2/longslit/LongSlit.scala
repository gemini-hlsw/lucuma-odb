// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package igrins2.longslit

import fs2.Pure
import fs2.Stream
import lucuma.core.model.Observation
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.data.Itc.Igrins2Spectroscopy
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object LongSlit:

  def staticFrom(config: Config): Igrins2StaticConfig =
    Igrins2StaticConfig(Igrins2SVCImages(config.saveSVCImages), config.offsetMode)

  def instantiate(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[Igrins2StaticConfig, Igrins2DynamicConfig],
    namespace:     UUID,
    config:        Config,
    itc:           Either[OdbError, Igrins2Spectroscopy]
  ): Either[OdbError, StreamingExecutionConfig[Pure, Igrins2StaticConfig, Igrins2DynamicConfig]] =
    val static = staticFrom(config)
    Science.instantiate(
      observationId, estimator, static, namespace, config,
      itc.map(_.science.focus.value)
    ).map(s => StreamingExecutionConfig(static, Stream.empty, s.generate))
