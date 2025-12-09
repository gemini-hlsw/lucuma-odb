// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data
package arb

import cats.data.NonEmptyList
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.arb.ArbTelescopeConfig
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbOffsetGenerator:

  import ArbAngle.given
  import ArbOffset.given
  import ArbTelescopeConfig.given

  given Arbitrary[OffsetGenerator.NoGenerator.type] =
    Arbitrary:
      Gen.const(OffsetGenerator.NoGenerator)

  given Arbitrary[OffsetGenerator.Enumerated] =
    Arbitrary:
      for
        tc0 <- arbitrary[TelescopeConfig]
        tcs <- arbitrary[List[TelescopeConfig]]
      yield OffsetGenerator.Enumerated(NonEmptyList(tc0, tcs))

  given Arbitrary[OffsetGenerator.Random] =
    Arbitrary:
      for
        s <- arbitrary[Angle]
        c <- arbitrary[Offset]
      yield OffsetGenerator.Random(s, c)

  given Arbitrary[OffsetGenerator.Spiral] =
    Arbitrary:
      for
        s <- arbitrary[Angle]
        c <- arbitrary[Offset]
      yield OffsetGenerator.Spiral(s, c)

  given Arbitrary[OffsetGenerator.Uniform] =
    Arbitrary:
      for
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
      yield OffsetGenerator.Uniform(a, b)

  given Arbitrary[OffsetGenerator] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[OffsetGenerator.NoGenerator.type],
        arbitrary[OffsetGenerator.Enumerated],
        arbitrary[OffsetGenerator.Random],
        arbitrary[OffsetGenerator.Spiral],
        arbitrary[OffsetGenerator.Uniform]
      )

object ArbOffsetGenerator extends ArbOffsetGenerator
