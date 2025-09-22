// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.arb

import lucuma.core.math.RadialVelocity
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.itc.client.TargetInput
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbTargetInput {
  import ArbRadialVelocity.given
  import ArbSourceProfile.given

  given Arbitrary[TargetInput] = Arbitrary {
    for {
      sp <- arbitrary[SourceProfile]
      rv <- arbitrary[RadialVelocity]
    } yield TargetInput(sp, rv)
  }

  given Cogen[TargetInput] = Cogen[(SourceProfile, RadialVelocity)].contramap { a =>
    (a.sourceProfile, a.radialVelocity)
  }
}

object ArbTargetInput extends ArbTargetInput
