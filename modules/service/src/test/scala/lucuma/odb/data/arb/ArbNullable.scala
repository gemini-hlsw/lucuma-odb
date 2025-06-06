// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data.arb

import lucuma.odb.data.Nullable
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

/**
 * Provides arbitrary instances for Nullable values.
 */
object ArbNullable:

  given nullableArbitrary[A: Arbitrary]: Arbitrary[Nullable[A]] =
    Arbitrary {
      Gen.oneOf(
        Gen.const(Nullable.Absent),
        Gen.const(Nullable.Null),
        arbitrary[A].map(Nullable.NonNull(_))
      )
    }
