// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GmosNorthDetector.{Hamamatsu => HamamatsuNorth}
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthStageMode.FollowXy
import lucuma.core.enums.GmosSouthDetector.{Hamamatsu => HamamatsuSouth}
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthStageMode.FollowXyz
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.MosPreImaging.IsNotMosPreImaging
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.syntax.timespan.*
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
 * @tparam F filter enumeration
 * @tparam U FPU enumeration
 */
sealed abstract class Generator[S, D, G, F, U](
  static:      S,
  acqFilters:  NonEmptyList[F],
  acqSequence: Acquisition[D, G, F, U],
  sciSequence: Science[D, G, F, U]
) {

  def generate(
    itc:           IntegrationTime,
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    config:        Config[G, F, U]
  ): Either[String, ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]]] = {

    val acq = acqSequence.compute(
      acqFilters,
      config.fpu,
      Generator.StandinAcquisitionTime,
      config.centralWavelength
    )

    val sci = sciSequence.compute(
      config,
      SciExposureTime(itc.exposureTime),
      sourceProfile,
      imageQuality,
      Generator.Sampling
    )

    Option
      .when(itc.exposures.value > 0)(
        ProtoExecutionConfig(
          static,
          Stream(ProtoAtom.of("Acquisition - Initial", acq.ccd2, acq.p10, acq.slit)) ++
            Stream(ProtoAtom.of("Acquisition - Slit", acq.slit)).repeat,
          sci.map(a => ProtoAtom(a.description.some, a.steps))
        )
      ).toRight("ITC prescribes 0 exposures.")

  }

}


object Generator {

  val Sampling: PosDouble =
    PosDouble.unsafeFrom(2.0)

  // Until we can get results from the ITC for acquisition
  val StandinAcquisitionTime: AcqExposureTime =
    AcqExposureTime(10.secTimeSpan)

  object GmosNorth extends Generator(
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

  object GmosSouth extends Generator(
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
