// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.model.arb.ArbExposureTimeMode.given
import lucuma.core.model.arb.ArbTelluricType.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbFlamingos2LongSlitInput:

  given Arbitrary[Flamingos2LongSlitInput.Create] =
    Arbitrary:
      for
        g  <- arbitrary[Flamingos2Disperser]
        f  <- arbitrary[Flamingos2Filter]
        u  <- arbitrary[Flamingos2Fpu]
        ac <- arbitrary[Option[ExposureTimeMode]]
        sc <- arbitrary[Option[ExposureTimeMode]]
        r  <- arbitrary[Option[Flamingos2ReadMode]]
        s  <- arbitrary[Option[Flamingos2Reads]]
        d  <- arbitrary[Option[Flamingos2Decker]]
        o  <- arbitrary[Option[Flamingos2ReadoutMode]]
        so <- arbitrary[Option[List[Offset]]]
        tt <- arbitrary[TelluricType]
      yield Flamingos2LongSlitInput.Create(g, f, u, ac, sc, r, s, d, o, so, tt)

  given Arbitrary[Flamingos2LongSlitInput.Edit] =
    Arbitrary:
      for
        g  <- arbitrary[Option[Flamingos2Disperser]]
        f  <- arbitrary[Option[Flamingos2Filter]]
        u  <- arbitrary[Option[Flamingos2Fpu]]
        ac <- arbitrary[Option[ExposureTimeMode]]
        sc <- arbitrary[Option[ExposureTimeMode]]
        r  <- arbitrary[Nullable[Flamingos2ReadMode]]
        s  <- arbitrary[Nullable[Flamingos2Reads]]
        d  <- arbitrary[Nullable[Flamingos2Decker]]
        o  <- arbitrary[Nullable[Flamingos2ReadoutMode]]
        so <- arbitrary[Nullable[List[Offset]]]
        tt <- arbitrary[Option[TelluricType]]
      yield Flamingos2LongSlitInput.Edit(g, f, u, ac, sc, r, s, d, o, so, tt)

object ArbFlamingos2LongSlitInput extends ArbFlamingos2LongSlitInput