// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.arb.ArbWavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosLongSlitInput:
  import ArbEnumerated.given
  import ArbNullable.given
  import ArbOffset.given
  import ArbExposureTimeMode.given
  import ArbWavelength.given
  import ArbWavelengthDither.given

  given Arbitrary[GmosLongSlitInput.Create.Common] =
    Arbitrary:
      for
        w <- arbitrary[Wavelength]
        c <- arbitrary[Option[ExposureTimeMode]]
        x <- arbitrary[Option[GmosXBinning]]
        y <- arbitrary[Option[GmosYBinning]]
        m <- arbitrary[Option[GmosAmpReadMode]]
        g <- arbitrary[Option[GmosAmpGain]]
        r <- arbitrary[Option[GmosRoi]]
        d <- arbitrary[Option[List[WavelengthDither]]]
        s <- arbitrary[Option[List[Q]]]
      yield GmosLongSlitInput.Create.Common(
        w,
        c,
        x,
        y,
        m,
        g,
        r,
        d,
        s
      )

  given Arbitrary[GmosLongSlitInput.NorthAcquisition] =
    Arbitrary:
      for
        f <- arbitrary[Nullable[GmosNorthFilter]]
        a <- arbitrary[Option[ExposureTimeMode]]
      yield GmosLongSlitInput.NorthAcquisition(f, a)

  given Arbitrary[GmosLongSlitInput.Create.North] =
    Arbitrary:
      for
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosNorthFpu]
        c <- arbitrary[GmosLongSlitInput.Create.Common]
        a <- arbitrary[Option[GmosLongSlitInput.NorthAcquisition]]
      yield GmosLongSlitInput.Create.North(
        g,
        f,
        u,
        c,
        a
      )

  given Arbitrary[GmosLongSlitInput.SouthAcquisition] =
    Arbitrary:
      for
        f <- arbitrary[Nullable[GmosSouthFilter]]
        a <- arbitrary[Option[ExposureTimeMode]]
      yield GmosLongSlitInput.SouthAcquisition(f, a)

  given Arbitrary[GmosLongSlitInput.Create.South] =
    Arbitrary:
      for
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosSouthFpu]
        c <- arbitrary[GmosLongSlitInput.Create.Common]
        a <- arbitrary[Option[GmosLongSlitInput.SouthAcquisition]]
      yield GmosLongSlitInput.Create.South(
        g,
        f,
        u,
        c,
        a
      )

object ArbGmosLongSlitInput extends ArbGmosLongSlitInput
