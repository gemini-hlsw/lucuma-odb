// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import cats.data.NonEmptyList
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ElevationRange
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ImageQuality
import lucuma.core.model.arb.ArbElevationRange.given
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.itc.CloudExtinctionInput
import lucuma.itc.ImageQualityInput
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIntegrationTimeInput {
  import ArbExposureTimeMode.given
  import ArbInstrumentMode.given
  import ArbTargetInput.given

  given Arbitrary[ImageQualityInput] =
    Arbitrary:
      for
        preset <- arbitrary[ImageQuality.Preset].map(ImageQualityInput.preset)
        arcsec <- Gen.choose(0.1, 3.0).map(BigDecimal.apply).map(ImageQualityInput.arcsec)
        r      <- Gen.oneOf(preset, arcsec)
      yield r

  given Arbitrary[CloudExtinctionInput] =
    Arbitrary:
      for
        preset     <- arbitrary[CloudExtinction.Preset].map(CloudExtinctionInput.preset)
        extinction <-
          Gen.choose(0.0, 3.0).map(BigDecimal.apply).map(CloudExtinctionInput.extinction)
        r          <- Gen.oneOf(preset, extinction)
      yield r

  given Arbitrary[ItcConstraintsInput] =
    Arbitrary:
      for
        iq <- arbitrary[ImageQualityInput]
        ce <- arbitrary[CloudExtinctionInput]
        sb <- arbitrary[SkyBackground]
        wv <- arbitrary[WaterVapor]
        er <- arbitrary[ElevationRange]
      yield ItcConstraintsInput(iq, ce, sb, wv, er)

  given Cogen[ItcConstraintsInput] =
    Cogen[
      (
        Either[ImageQuality.Preset, BigDecimal],
        Either[CloudExtinction.Preset, BigDecimal],
        SkyBackground,
        WaterVapor,
        ElevationRange
      )
    ].contramap(x =>
      (x.imageQuality.value,
       x.cloudExtinction.value,
       x.skyBackground,
       x.waterVapor,
       x.elevationRange
      )
    )

  given [A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary:
      arbitrary[List[A]].suchThat(_.nonEmpty).map(NonEmptyList.fromListUnsafe)

  given Arbitrary[SpectroscopyParameters] =
    Arbitrary:
      for
        ex <- arbitrary[ExposureTimeMode]
        cs <- arbitrary[ItcConstraintsInput]
        im <- arbitrary[InstrumentMode]
      yield SpectroscopyParameters(ex, cs, im)

  given Arbitrary[SpectroscopyInput] =
    Arbitrary:
      for
        pars <- arbitrary[SpectroscopyParameters]
        ast  <- arbitrary[NonEmptyList[TargetInput]]
      yield SpectroscopyInput(pars, ast)

  given Cogen[SpectroscopyInput] =
    Cogen[
      (ExposureTimeMode, List[TargetInput], ItcConstraintsInput, InstrumentMode)
    ].contramap: a =>
      (a.exposureTimeMode, a.asterism.toList, a.constraints, a.mode)

  given Arbitrary[ImagingParameters] =
    Arbitrary:
      for {
        ex <- arbitrary[ExposureTimeMode]
        cs <- arbitrary[ItcConstraintsInput]
        im <- arbitrary[InstrumentMode]
      } yield ImagingParameters(ex, cs, im)

  given Arbitrary[ImagingInput] =
    Arbitrary:
      for
        pars <- arbitrary[ImagingParameters]
        ast  <- arbitrary[NonEmptyList[TargetInput]]
      yield ImagingInput(pars, ast)

  given Cogen[ImagingInput] =
    Cogen[
      (
        ExposureTimeMode,
        List[TargetInput],
        ItcConstraintsInput,
        InstrumentMode
      )
    ].contramap: a =>
      (a.exposureTimeMode, a.asterism.toList, a.constraints, a.mode)
}

object ArbIntegrationTimeInput extends ArbIntegrationTimeInput
