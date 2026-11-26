// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package scorpio.longslit

import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScorpioChannelSelection
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.enums.ScorpioFilter
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.enums.SequenceType
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.scorpio.ScorpioChannelConfig
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.core.model.sequence.scorpio.ScorpioFpuMask
import lucuma.core.model.sequence.scorpio.ScorpioStaticConfig
import lucuma.core.model.sequence.scorpio.VisMinExposureTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.definitionError
import lucuma.refined.*

import java.util.UUID

/**
 * SCORPIO long slit sequence generation.  Science only: one atom per pass over
 * the telescope configs, one step per telescope config.  Each channel takes its
 * exposure time and frame count from its time and count exposure time mode.
 */
object LongSlit:

  val NodCycleTitle: NonEmptyString = "Nod Cycle".refined

  val Static: ScorpioStaticConfig =
    ScorpioStaticConfig(ScorpioChannelSelection.VisOnly)

  private def channel(
    observationId: Observation.Id,
    filter:        ScorpioFilter,
    etm:           ExposureTimeMode
  ): Either[OdbError, ScorpioChannelConfig.Vis] =
    ExposureTimeMode
      .timeAndCount
      .getOption(etm)
      .toRight(definitionError(observationId, s"The SCORPIO ${filter.shortName} channel requires a time and count exposure time mode."))
      .filterOrElse(
        _.time >= VisMinExposureTime,
        definitionError(observationId, s"The SCORPIO ${filter.shortName} channel exposure time is below the minimum of ${VisMinExposureTime.toMilliseconds.toLong} ms.")
      )
      .map(tc => ScorpioChannelConfig.Vis(tc.time, tc.count, ScorpioVisReadoutMode.Default))

  def dynamicFrom(
    observationId: Observation.Id,
    config:        Config
  ): Either[OdbError, ScorpioDynamicConfig] =
    def vis(etm: ExposureTimeMode, filter: ScorpioFilter): Either[OdbError, Option[ScorpioChannelConfig.Vis]] =
      channel(observationId, filter, etm).map(_.some)

    for
      g <- vis(config.g, ScorpioFilter.G)
      r <- vis(config.r, ScorpioFilter.R)
      i <- vis(config.i, ScorpioFilter.I)
      z <- vis(config.z, ScorpioFilter.Z)
    yield ScorpioDynamicConfig(g, r, i, z, none, none, none, none, ScorpioFpuMask.LongSlit(config.fpu), ScorpioDisperser.Vph)

  def instantiate(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[ScorpioStaticConfig, ScorpioDynamicConfig],
    namespace:     UUID,
    config:        Config,
    calRole:       Option[CalibrationRole]
  ): Either[OdbError, StreamingExecutionConfig[Pure, ScorpioStaticConfig, ScorpioDynamicConfig]] =
    dynamicFrom(observationId, config).map: dynamic =>
      val cycle = ProtoAtom(
        NodCycleTitle.some,
        config.steps.map(ProtoStep(dynamic, StepConfig.Science, _, calRole.sciClass))
      )

      val science =
        AtomBuilder
          .instantiate(estimator, Static, namespace, SequenceType.Science)
          .buildStream(Stream.emit(cycle).repeatN(config.repeats.value.toLong))

      StreamingExecutionConfig(Static, Stream.empty, science)
