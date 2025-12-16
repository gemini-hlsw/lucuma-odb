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
import lucuma.core.geom.OffsetGenerator

trait ArbTelescopeConfigGenerator:
  import ArbOffsetGenerator.given
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

  given Arbitrary[TelescopeConfigGenerator.FromOffsetGenerator] =
    Arbitrary:
      arbitrary[OffsetGenerator].map(TelescopeConfigGenerator.FromOffsetGenerator(_))
    

  given Arbitrary[TelescopeConfigGenerator] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[TelescopeConfigGenerator.NoGenerator.type],
        arbitrary[TelescopeConfigGenerator.Enumerated],
        arbitrary[TelescopeConfigGenerator.FromOffsetGenerator]
      )

object ArbTelescopeConfigGenerator extends ArbTelescopeConfigGenerator
