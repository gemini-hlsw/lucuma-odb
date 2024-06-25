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
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.AcqExposureTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.SciExposureTime


/**
 * Generator for GMOS Long Slit.
 *
 * @tparam S static configuration type
 * @tparam D dynamic configuration type
 * @tparam G grating enumeration
 * @tparam L filter enumeration
 * @tparam U FPU enumeration
 */
sealed abstract class PureGenerator[S, D, G, L, U](
  static:      S,
  acqFilters:  NonEmptyList[L],
  acqSequence: Acquisition[D, G, L, U],
  sciSequence: Science[D, G, L, U]
) {

  def generate(
    acquisitionItc: IntegrationTime,
    scienceItc:     IntegrationTime,
    config:         Config[G, L, U]
  ): Either[String, ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]]] = {

    val acq = acqSequence.compute(
      acqFilters,
      config.fpu,
      AcqExposureTime(acquisitionItc.exposureTime),
      config.centralWavelength
    )

    val sci = sciSequence.compute(config, SciExposureTime(scienceItc.exposureTime))

    Option
      .when(scienceItc.exposures.value > 0 && acquisitionItc.exposures.value > 0)(
        ProtoExecutionConfig(
          static,
          Stream(ProtoAtom.of("Acquisition - Initial", acq.ccd2, acq.p10, acq.slit)) ++
            Stream(ProtoAtom.of("Acquisition - Slit", acq.slit)).repeat,
          sci.map(a => ProtoAtom(a.description.some, a.steps))
             .take(scienceItc.exposures.value)
        )
      ).toRight("ITC prescribes 0 exposures.")

  }

}


object PureGenerator {

  object GmosNorth extends PureGenerator(
    StaticConfig.GmosNorth(
      FollowXy,
      HamamatsuNorth,
      IsNotMosPreImaging,
      None
    ),
    GmosNorthFilter.acquisition,
    Acquisition.GmosNorth,
    Science.GmosNorth
  )

  object GmosSouth extends PureGenerator(
    StaticConfig.GmosSouth(
      FollowXyz,
      HamamatsuSouth,
      IsNotMosPreImaging,
      None
    ),
    GmosSouthFilter.acquisition,
    Acquisition.GmosSouth,
    Science.GmosSouth
  )
}
