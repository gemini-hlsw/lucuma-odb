// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
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
import lucuma.core.enums.ImageQuality
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.arb.ArbWavelengthDither
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbGmosLongSlitInput {

  import ArbEnumerated.*
  import ArbOffset.given
  import ArbWavelength.given
  import ArbWavelengthDither.given

  given Arbitrary[GmosLongSlitInput.Create.Common] =
    Arbitrary {
      for {
        w <- arbitrary[Wavelength]
        x <- arbitrary[Option[GmosXBinning]]
        y <- arbitrary[Option[GmosYBinning]]
        m <- arbitrary[Option[GmosAmpReadMode]]
        g <- arbitrary[Option[GmosAmpGain]]
        r <- arbitrary[Option[GmosRoi]]
        d <- arbitrary[Option[List[WavelengthDither]]]
        s <- arbitrary[Option[List[Q]]]
      } yield GmosLongSlitInput.Create.Common(
        w,
        x,
        y,
        m,
        g,
        r,
        d,
        s
      )
    }

  given Arbitrary[GmosLongSlitInput.Create.North] =
    Arbitrary {
      for {
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosNorthFpu]
        c <- arbitrary[GmosLongSlitInput.Create.Common]
      } yield GmosLongSlitInput.Create.North(
        g,
        f,
        u,
        c
      )
    }

  given Arbitrary[GmosLongSlitInput.Create.South] =
    Arbitrary {
      for {
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosSouthFpu]
        c <- arbitrary[GmosLongSlitInput.Create.Common]
      } yield GmosLongSlitInput.Create.South(
        g,
        f,
        u,
        c
      )
    }

}

object ArbGmosLongSlitInput extends ArbGmosLongSlitInput
