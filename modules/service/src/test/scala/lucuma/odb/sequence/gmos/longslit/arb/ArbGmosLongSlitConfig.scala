// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit
package arb

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.arb.ArbGmosLongSlitInput
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

object ArbGmosLongSlitConfig {

  import ArbGmosLongSlitInput.given

  given Arbitrary[GmosLongSlitConfig.North] =
    Arbitrary {
      arbitrary[GmosLongSlitInput.Create.North].map(_.toGmosLongSlit)
    }

  given Arbitrary[GmosLongSlitConfig.South] =
    Arbitrary {
      arbitrary[GmosLongSlitInput.Create.South].map(_.toGmosLongSlit)
    }

}
