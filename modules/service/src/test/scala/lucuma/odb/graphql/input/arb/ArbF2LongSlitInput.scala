// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2Reads
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2WindowCover
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.data.arb.ArbNullable.given
import lucuma.odb.data.Nullable
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbF2LongSlitInput:

  given Arbitrary[F2LongSlitInput.Create] =
    Arbitrary {
      for {
        g <- arbitrary[F2Disperser]
        f <- arbitrary[Option[F2Filter]]
        u <- arbitrary[F2Fpu]
        r <- arbitrary[Option[F2ReadMode]]
        d <- arbitrary[Option[F2Decker]]
        o <- arbitrary[Option[F2ReadoutMode]]
        s <- arbitrary[Option[F2Reads]]
        w <- arbitrary[Option[F2WindowCover]]
        e <- arbitrary[Option[Boolean]]
      } yield F2LongSlitInput.Create(g, f, u, r, d, o, s, w, e)
    }

  given Arbitrary[F2LongSlitInput.Edit] =
    Arbitrary {
      for {
        g <- arbitrary[Option[F2Disperser]]
        f <- arbitrary[Nullable[F2Filter]]
        u <- arbitrary[Option[F2Fpu]]
        r <- arbitrary[Nullable[F2ReadMode]]
        d <- arbitrary[Nullable[F2Decker]]
        o <- arbitrary[Nullable[F2ReadoutMode]]
        s <- arbitrary[Nullable[F2Reads]]
        w <- arbitrary[Nullable[F2WindowCover]]
        e <- arbitrary[Nullable[Boolean]]
      } yield F2LongSlitInput.Edit(g, f, u, r, d, o, s, w, e)
    }


object ArbF2LongSlitInput extends ArbF2LongSlitInput
