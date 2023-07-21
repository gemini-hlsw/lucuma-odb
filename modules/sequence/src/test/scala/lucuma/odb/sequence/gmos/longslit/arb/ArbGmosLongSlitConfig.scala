// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit
package arb

import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.arb.ArbWavelengthDither
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

object ArbGmosLongSlitConfig {

  import ArbEnumerated.*
  import ArbOffset.given
  import ArbWavelength.given
  import ArbWavelengthDither.given
  given Arbitrary[Config.GmosNorth] =
    Arbitrary {
      for {
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosNorthFpu]
        w <- arbitrary[Wavelength]
        dx <- arbitrary[GmosXBinning]
        x <- arbitrary[Option[GmosXBinning]]
        y <- arbitrary[Option[GmosYBinning]]
        m <- arbitrary[Option[GmosAmpReadMode]]
        n <- arbitrary[Option[GmosAmpGain]]
        r <- arbitrary[Option[GmosRoi]]
        d <- arbitrary[Option[List[WavelengthDither]]]
        s <- arbitrary[Option[List[Q]]]
      } yield Config.GmosNorth(
        g,
        f,
        u,
        w,
        dx,
        x,
        y,
        m,
        n,
        r,
        d,
        s
      )
    }

  given Arbitrary[Config.GmosSouth] =
    Arbitrary {
      for {
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosSouthFpu]
        w <- arbitrary[Wavelength]
        dx <- arbitrary[GmosXBinning]
        x <- arbitrary[Option[GmosXBinning]]
        y <- arbitrary[Option[GmosYBinning]]
        m <- arbitrary[Option[GmosAmpReadMode]]
        n <- arbitrary[Option[GmosAmpGain]]
        r <- arbitrary[Option[GmosRoi]]
        d <- arbitrary[Option[List[WavelengthDither]]]
        s <- arbitrary[Option[List[Q]]]
      } yield Config.GmosSouth(
        g,
        f,
        u,
        w,
        dx,
        x,
        y,
        m,
        n,
        r,
        d,
        s
      )
    }

}
