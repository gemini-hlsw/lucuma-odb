// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.arb

import cats.syntax.option.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.graphql.input.GmosIfuInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.service.CalibrationConfigSubset.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object ArbCalibrationConfigSubset:

  // Shaped like the inputs toConfigSubset builds: every effective value pinned, nothing else set.
  private val longSlitCommon: Gen[GmosLongSlitInput.Create.Common] =
    for
      w <- arbitrary[Wavelength]
      x <- arbitrary[GmosXBinning]
      y <- arbitrary[GmosYBinning]
      r <- arbitrary[GmosAmpReadMode]
      a <- arbitrary[GmosAmpGain]
      o <- arbitrary[GmosRoi]
    yield GmosLongSlitInput.Create.Common(w, none, x.some, y.some, r.some, a.some, o.some, none, none)

  private val ifuCommon: Gen[GmosIfuInput.Create.Common] =
    for
      w <- arbitrary[Wavelength]
      x <- arbitrary[GmosXBinning]
      y <- arbitrary[GmosYBinning]
      r <- arbitrary[GmosAmpReadMode]
      a <- arbitrary[GmosAmpGain]
      o <- arbitrary[GmosRoi]
    yield GmosIfuInput.Create.Common(w, none, none, x.some, y.some, r.some, a.some, o.some, none, none)

  given Arbitrary[GmosNConfigs] =
    Arbitrary:
      for
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosNorthFpu]
        c <- longSlitCommon
      yield GmosNConfigs(GmosLongSlitInput.Create.North(g, f, u, c, none))

  given Arbitrary[GmosSConfigs] =
    Arbitrary:
      for
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosSouthFpu]
        c <- longSlitCommon
      yield GmosSConfigs(GmosLongSlitInput.Create.South(g, f, u, c, none))

  given Arbitrary[GmosNIfuConfigs] =
    Arbitrary:
      for
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosNorthIfuFpu]
        c <- ifuCommon
      yield GmosNIfuConfigs(GmosIfuInput.Create.North(g, f, u, none, c))

  given Arbitrary[GmosSIfuConfigs] =
    Arbitrary:
      for
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosSouthIfuFpu]
        c <- ifuCommon
      yield GmosSIfuConfigs(GmosIfuInput.Create.South(g, f, u, none, c))

  given Arbitrary[Flamingos2Configs] =
    Arbitrary:
      for
        d <- arbitrary[Flamingos2Disperser]
        f <- arbitrary[Flamingos2Filter]
        u <- arbitrary[Flamingos2Fpu]
      yield Flamingos2Configs(d, f, u)
