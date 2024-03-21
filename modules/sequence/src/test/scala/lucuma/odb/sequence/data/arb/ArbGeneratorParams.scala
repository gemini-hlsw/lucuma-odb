// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data
package arb

import cats.data.NonEmptyList
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbGid
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.arb.ArbInstrumentMode
import lucuma.itc.client.arb.ArbIntegrationTimeInput
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGeneratorParams {

  import ArbGid.given
  import ArbGmosLongSlitConfig.given
  import ArbInstrumentMode.given
  import GeneratorParams.GmosNorthLongSlit
  import GeneratorParams.GmosSouthLongSlit

  given Arbitrary[GmosNorthLongSlit] = {
    given Arbitrary[(ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput)] =
      Arbitrary {
        for {
          mo <- arbitrary[InstrumentMode.GmosNorthSpectroscopy]
          sm <- ArbIntegrationTimeInput.genSpectroscopyIntegrationTimeInput(mo)
          im <- ArbIntegrationTimeInput.genImagingIntegrationTimeInput(mo)
        } yield (im, sm)
      }

    Arbitrary {
      for {
        s   <- Gen.choose(1, 4)
        itc <- Gen.listOfN(s, arbitrary[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))])
          .map(NonEmptyList.fromListUnsafe)
        cfg <- arbitrary[Config.GmosNorth]
      } yield GmosNorthLongSlit(itc, cfg)
    }
  }

  given Arbitrary[GmosSouthLongSlit] = {
    given Arbitrary[(ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput)] =
      Arbitrary {
        for {
          mo <- arbitrary[InstrumentMode.GmosNorthSpectroscopy]
          sm <- ArbIntegrationTimeInput.genSpectroscopyIntegrationTimeInput(mo)
          im <- ArbIntegrationTimeInput.genImagingIntegrationTimeInput(mo)
        } yield (im, sm)
      }

    Arbitrary {
      for {
        s   <- Gen.choose(1, 4)
        itc <- Gen.listOfN(s, arbitrary[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))])
          .map(NonEmptyList.fromListUnsafe)
        cfg <- arbitrary[Config.GmosSouth]
      } yield GmosSouthLongSlit(itc, cfg)
    }
  }
}

object ArbGeneratorParams extends ArbGeneratorParams
