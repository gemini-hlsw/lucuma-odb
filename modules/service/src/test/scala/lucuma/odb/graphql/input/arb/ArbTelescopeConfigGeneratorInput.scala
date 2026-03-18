// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
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

trait ArbTelescopeConfigGeneratorInput:

  import ArbAngle.given
  import ArbOffset.given
  import ArbTelescopeConfig.given

  given Arbitrary[TelescopeConfigGeneratorInput.NoGeneratorInput.type] =
    Arbitrary:
      Gen.const(TelescopeConfigGeneratorInput.NoGeneratorInput)

  given Arbitrary[TelescopeConfigGeneratorInput.EnumeratedInput] =
    Arbitrary:
      for
        tc0 <- arbitrary[TelescopeConfig]
        tcs <- arbitrary[List[TelescopeConfig]]
      yield TelescopeConfigGeneratorInput.EnumeratedInput(NonEmptyList(tc0, tcs))

  given Arbitrary[TelescopeConfigGeneratorInput.RandomInput] =
    Arbitrary:
      for
        z <- arbitrary[Angle]
        c <- arbitrary[Option[Offset]]
        s <- arbitrary[Option[Long]]
      yield TelescopeConfigGeneratorInput.RandomInput(z, c, s)

  given Arbitrary[TelescopeConfigGeneratorInput.SpiralInput] =
    Arbitrary:
      for
        z <- arbitrary[Angle]
        c <- arbitrary[Option[Offset]]
        s <- arbitrary[Option[Long]]
      yield TelescopeConfigGeneratorInput.SpiralInput(z, c, s)

  given Arbitrary[TelescopeConfigGeneratorInput.UniformInput] =
    Arbitrary:
      for
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
      yield TelescopeConfigGeneratorInput.UniformInput(a, b)

  given Arbitrary[TelescopeConfigGeneratorInput] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[TelescopeConfigGeneratorInput.NoGeneratorInput.type],
        arbitrary[TelescopeConfigGeneratorInput.EnumeratedInput],
        arbitrary[TelescopeConfigGeneratorInput.RandomInput],
        arbitrary[TelescopeConfigGeneratorInput.SpiralInput],
        arbitrary[TelescopeConfigGeneratorInput.UniformInput]
      )

object ArbTelescopeConfigGeneratorInput extends ArbTelescopeConfigGeneratorInput
