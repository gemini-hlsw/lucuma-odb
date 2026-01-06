// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data
package arb

import cats.data.NonEmptyList
import lucuma.core.geom.arb.ArbOffsetGenerator
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.arb.ArbTelescopeConfig
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbTelescopeConfigGenerator extends ArbOffsetGenerator:
  import ArbTelescopeConfig.given

  given Arbitrary[TelescopeConfigGenerator.NoGenerator.type] =
    Arbitrary:
      Gen.const(TelescopeConfigGenerator.NoGenerator)

  given Arbitrary[TelescopeConfigGenerator.Enumerated] =
    Arbitrary:
      for
        tc0 <- arbitrary[TelescopeConfig]
        tcs <- arbitrary[List[TelescopeConfig]]
      yield TelescopeConfigGenerator.Enumerated(NonEmptyList(tc0, tcs))

  given Arbitrary[TelescopeConfigGenerator.Random] =
    Arbitrary:
      for
        g <- genRandomOffsetGenerator
        s <- arbitrary[Long]
      yield TelescopeConfigGenerator.Random(g, s)

  given Arbitrary[TelescopeConfigGenerator.Spiral] =
    Arbitrary:
      for
        g <- genSpiralOffsetGenerator
        s <- arbitrary[Long]
      yield TelescopeConfigGenerator.Spiral(g, s)

  given Arbitrary[TelescopeConfigGenerator.Uniform] =
    Arbitrary:
      for
        g <- genUniformOffsetGenerator
      yield TelescopeConfigGenerator.Uniform(g)

  given Arbitrary[TelescopeConfigGenerator] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[TelescopeConfigGenerator.NoGenerator.type],
        arbitrary[TelescopeConfigGenerator.Enumerated],
        arbitrary[TelescopeConfigGenerator.Random],
        arbitrary[TelescopeConfigGenerator.Spiral],
        arbitrary[TelescopeConfigGenerator.Uniform]
      )

object ArbTelescopeConfigGenerator extends ArbTelescopeConfigGenerator
