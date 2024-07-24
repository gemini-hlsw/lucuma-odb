// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data
package arb

import cats.data.NonEmptyList
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.arb.ArbInstrumentMode
import lucuma.itc.client.arb.ArbIntegrationTimeInput
import lucuma.odb.data.CalibrationRole
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGeneratorParams {

  import ArbEnumerated.given
  import ArbGid.given
  import ArbGmosLongSlitConfig.given
  import ArbInstrumentMode.given

  private def genItc(mo: InstrumentMode): Gen[(ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput)] =
    for {
      sm <- ArbIntegrationTimeInput.genSpectroscopyIntegrationTimeInput(mo)
      im <- ArbIntegrationTimeInput.genImagingIntegrationTimeInput(mo)
    } yield (im, sm)

  val genGmosNorthLongSlit: Gen[GeneratorParams] = {
    given Arbitrary[(ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput)] =
      Arbitrary(arbitrary[InstrumentMode.GmosNorthSpectroscopy].flatMap(genItc))

    for {
      mo  <- arbitrary[InstrumentMode.GmosNorthSpectroscopy]
      s   <- Gen.choose(1, 4)
      itc <- Gen.listOfN(s, arbitrary[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))])
        .map(NonEmptyList.fromListUnsafe)
      cfg <- arbitrary[Config.GmosNorth]
      rol <- arbitrary[Option[CalibrationRole]]
    } yield GeneratorParams(itc, cfg, rol)
  }

  val genGmosSouthLongSlit: Gen[GeneratorParams] = {
    given Arbitrary[(ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput)] =
      Arbitrary(arbitrary[InstrumentMode.GmosSouthSpectroscopy].flatMap(genItc))

    for {
      mo  <- arbitrary[InstrumentMode.GmosSouthSpectroscopy]
      s   <- Gen.choose(1, 4)
      itc <- Gen.listOfN(s, arbitrary[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))])
        .map(NonEmptyList.fromListUnsafe)
      cfg <- arbitrary[Config.GmosSouth]
      rol <- arbitrary[Option[CalibrationRole]]
    } yield GeneratorParams(itc, cfg, rol)
  }

  given Arbitrary[GeneratorParams] =
    Arbitrary {
      Gen.oneOf(
        genGmosNorthLongSlit,
        genGmosSouthLongSlit
      )
    }
}

object ArbGeneratorParams extends ArbGeneratorParams
