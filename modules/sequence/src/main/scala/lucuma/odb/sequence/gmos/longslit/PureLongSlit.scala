// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.option.*
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GmosNorthDetector.Hamamatsu as HamamatsuNorth
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthStageMode.FollowXy
import lucuma.core.enums.GmosSouthDetector.Hamamatsu as HamamatsuSouth
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthStageMode.FollowXyz
import lucuma.core.enums.MosPreImaging.IsNotMosPreImaging
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.AcqExposureTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.SciExposureTime


/**
 * Core sequence generator for GMOS Long Slit.  It creates the acquisition
 * sequence and the science sequence. Neither sequence is filtered for execution
 * and smart arcs are not yet added.
 *
 * @tparam S static configuration type
 * @tparam D dynamic configuration type
 */
trait PureLongSlit[S, D] {

  def scienceTarget(
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime
  ): ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]]

  def spectroPhotometric(
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime
  ): ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]]

}

object PureLongSlit {

  /**
   * @tparam S static configuration type
   * @tparam D dynamic configuration type
   * @tparam G grating enumeration
   * @tparam L filter enumeration
   * @tparam U FPU enumeration
   */
  private def instantiate[S, D, G, L, U](
    config:      Config[G, L, U],
    static:      S,
    acqFilters:  NonEmptyList[L],
    acqSequence: Acquisition[D, G, L, U],
    sciSequence: Science[D, G, L, U],
    specPhot:    SpectroPhotometric[D, G, L, U]
  ): PureLongSlit[S, D] = {

    def acquisition(
      itc: IntegrationTime
    ): Stream[Pure, ProtoAtom[ProtoStep[D]]] = {
      val acqSteps = acqSequence.compute(
        acqFilters,
        config.fpu,
        AcqExposureTime(itc.exposureTime),
        config.centralWavelength
      )

      Stream(ProtoAtom.of("Acquisition - Initial", acqSteps.ccd2, acqSteps.p10, acqSteps.slit)) ++
        Stream(ProtoAtom.of("Acquisition - Slit", acqSteps.slit)).repeat
    }

    def science(
      seq: ScienceAtomSequenceState[D, G, L, U],
      itc: IntegrationTime
    ): Stream[Pure, ProtoAtom[ProtoStep[D]]] =
      seq
        .stream(config, SciExposureTime(itc.exposureTime))
        .map(a => ProtoAtom(a.description.some, a.steps))
        .take(itc.exposureCount.value)

    new PureLongSlit[S, D] {

      def scienceTarget(
        acquisitionItc: IntegrationTime,
        scienceItc:     IntegrationTime
      ): ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]] =
        ProtoExecutionConfig(static, acquisition(acquisitionItc), science(sciSequence, scienceItc))

      def spectroPhotometric(
        acquisitionItc: IntegrationTime,
        scienceItc:     IntegrationTime
      ): ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]] =
        ProtoExecutionConfig(static, acquisition(acquisitionItc), science(specPhot, scienceItc))

    }

  }

  def gmosNorth(
    config: Config.GmosNorth
  ): PureLongSlit[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
    instantiate(
      config,
      StaticConfig.GmosNorth(
        FollowXy,
        HamamatsuNorth,
        IsNotMosPreImaging,
        None
      ),
      GmosNorthFilter.acquisition,
      Acquisition.GmosNorth,
      Science.GmosNorth,
      SpectroPhotometric.GmosNorth
    )

  def gmosSouth(
    config: Config.GmosSouth
  ): PureLongSlit[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
    instantiate(
      config,
      StaticConfig.GmosSouth(
        FollowXyz,
        HamamatsuSouth,
        IsNotMosPreImaging,
        None
      ),
      GmosSouthFilter.acquisition,
      Acquisition.GmosSouth,
      Science.GmosSouth,
      SpectroPhotometric.GmosSouth
    )
}
