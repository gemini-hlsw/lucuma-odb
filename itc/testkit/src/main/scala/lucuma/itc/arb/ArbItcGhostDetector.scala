// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

package arb

import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbItcGhostDetector:
  import ArbEnumerated.given
  import ArbExposureTimeMode.given

  given Arbitrary[ItcGhostDetector] =
    Arbitrary:
      for
        t <- arbitrary[ExposureTimeMode.TimeAndCountMode]
        r <- arbitrary[GhostReadMode]
        b <- arbitrary[GhostBinning]
      yield ItcGhostDetector(t, r, b)

  given Cogen[ItcGhostDetector] =
    Cogen[(ExposureTimeMode, GhostReadMode, GhostBinning)]
      .contramap(a => (a.timeAndCount, a.readMode, a.binning))

object ArbItcGhostDetector extends ArbItcGhostDetector
