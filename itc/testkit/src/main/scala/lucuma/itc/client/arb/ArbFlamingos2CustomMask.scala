// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbFlamingos2CustomMask:

  import ArbEnumerated.given

  given Arbitrary[Flamingos2CustomMask] =
    Arbitrary(arbitrary[Flamingos2CustomSlitWidth].map(Flamingos2CustomMask(_)))

  given Cogen[Flamingos2CustomMask] =
    Cogen[Flamingos2CustomSlitWidth].contramap(_.slitWidth)

object ArbFlamingos2CustomMask extends ArbFlamingos2CustomMask
