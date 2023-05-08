// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data
package arb

import cats.data.NonEmptyList
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbGid
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.arb.ArbInstrumentMode
import lucuma.itc.client.arb.ArbIntegrationTimeInput
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbGeneratorParams {

  import ArbGid.*
  import ArbGmosLongSlitConfig.given
  import ArbInstrumentMode.given
  import GeneratorParams.GmosNorthLongSlit
  import GeneratorParams.GmosSouthLongSlit

  given Arbitrary[GmosNorthLongSlit] = {
    given Arbitrary[SpectroscopyIntegrationTimeInput] =
      Arbitrary {
        for {
          im <- arbitrary[InstrumentMode.GmosNorthSpectroscopy]
          sm <- ArbIntegrationTimeInput.genSpectroscopyIntegrationTimeInput(im)
        } yield sm
      }

    Arbitrary {
      for {
        s   <- Gen.choose(1, 4)
        itc <- Gen.listOfN(s, arbitrary[(Target.Id, SpectroscopyIntegrationTimeInput)]).map(NonEmptyList.fromListUnsafe)
        cfg <- arbitrary[Config.GmosNorth]
      } yield GmosNorthLongSlit(itc, cfg)
    }
  }

  given Arbitrary[GmosSouthLongSlit] = {
    given Arbitrary[SpectroscopyIntegrationTimeInput] =
      Arbitrary {
        for {
          im <- arbitrary[InstrumentMode.GmosSouthSpectroscopy]
          sm <- ArbIntegrationTimeInput.genSpectroscopyIntegrationTimeInput(im)
        } yield sm
      }

    Arbitrary {
      for {
        s   <- Gen.choose(1, 4)
        itc <- Gen.listOfN(s, arbitrary[(Target.Id, SpectroscopyIntegrationTimeInput)]).map(NonEmptyList.fromListUnsafe)
        cfg <- arbitrary[Config.GmosSouth]
      } yield GmosSouthLongSlit(itc, cfg)
    }
  }
}

object ArbGeneratorParams extends ArbGeneratorParams
