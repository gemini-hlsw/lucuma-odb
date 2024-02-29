// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import lucuma.core.enums.Half
import lucuma.core.model.Semester
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Year


trait ArbSemester {
  import ArbEnumerated.*

  given Arbitrary[Semester] =
    Arbitrary {
      for {
        y <- Gen.choose(2000, 2025)
        h <- arbitrary[Half]
      } yield Semester(Year.of(y), h)
    }

  given Cogen[Semester] =
    Cogen[(Int, Half)].contramap { a => (
      a.year.getValue,
      a.half
    )}
}

object ArbSemester extends ArbSemester
