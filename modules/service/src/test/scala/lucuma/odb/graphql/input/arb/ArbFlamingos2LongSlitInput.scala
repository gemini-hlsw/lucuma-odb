// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbFlamingos2LongSlitInput:

  given Arbitrary[Flamingos2LongSlitInput.Create] =
    Arbitrary {
      for {
        g <- arbitrary[F2Disperser]
        f <- arbitrary[F2Filter]
        u <- arbitrary[F2Fpu]
        r <- arbitrary[Option[F2ReadMode]]
        s <- arbitrary[Option[F2Reads]]
        d <- arbitrary[Option[F2Decker]]
        o <- arbitrary[Option[F2ReadoutMode]]
      } yield Flamingos2LongSlitInput.Create(g, f, u, r, s, d, o)
    }

  given Arbitrary[Flamingos2LongSlitInput.Edit] =
    Arbitrary {
      for {
        g <- arbitrary[Option[F2Disperser]]
        f <- arbitrary[Option[F2Filter]]
        u <- arbitrary[Option[F2Fpu]]
        r <- arbitrary[Nullable[F2ReadMode]]
        s <- arbitrary[Nullable[F2Reads]]
        d <- arbitrary[Nullable[F2Decker]]
        o <- arbitrary[Nullable[F2ReadoutMode]]
      } yield Flamingos2LongSlitInput.Edit(g, f, u, r, s, d, o)
    }


object ArbFlamingos2LongSlitInput extends ArbFlamingos2LongSlitInput
