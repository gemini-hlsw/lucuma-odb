// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.arb

import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.service.CalibrationConfigSubset.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object ArbCalibrationConfigSubset:

  given Arbitrary[GmosNConfigs] =
    Arbitrary:
      for
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosNorthFpu]
        w <- arbitrary[Wavelength]
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        r <- arbitrary[GmosAmpReadMode]
        a <- arbitrary[GmosAmpGain]
        o <- arbitrary[GmosRoi]
      yield GmosNConfigs(g, f, u, w, x, y, r, a, o)

  given Arbitrary[GmosSConfigs] =
    Arbitrary:
      for
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosSouthFpu]
        w <- arbitrary[Wavelength]
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        r <- arbitrary[GmosAmpReadMode]
        a <- arbitrary[GmosAmpGain]
        o <- arbitrary[GmosRoi]
      yield GmosSConfigs(g, f, u, w, x, y, r, a, o)

  given Arbitrary[Flamingos2Configs] =
    Arbitrary:
      for
        d <- arbitrary[Flamingos2Disperser]
        f <- arbitrary[Flamingos2Filter]
        u <- arbitrary[Flamingos2Fpu]
      yield Flamingos2Configs(d, f, u)
