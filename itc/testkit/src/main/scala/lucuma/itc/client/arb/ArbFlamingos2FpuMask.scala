// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbFlamingos2FpuMask:

  import ArbEnumerated.given
  import ArbFlamingos2CustomMask.given

  given Arbitrary[Flamingos2FpuMask] =
    Arbitrary {
      arbitrary[Either[Flamingos2CustomMask, Flamingos2Fpu]].map(Flamingos2FpuMask(_))
    }

  given Cogen[Flamingos2FpuMask] =
    Cogen[Either[Flamingos2CustomMask, Flamingos2Fpu]].contramap(_.fpu)

object ArbFlamingos2FpuMask extends ArbFlamingos2FpuMask
