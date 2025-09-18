// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosFpu {

  import ArbEnumerated.given
  import ArbGmosCustomMask.given
  import GmosFpu.North
  import GmosFpu.South

  given Arbitrary[North] =
    Arbitrary {
      arbitrary[Either[GmosCustomMask, GmosNorthFpu]].map(North(_))
    }

  given Cogen[North] =
    Cogen[Either[GmosCustomMask, GmosNorthFpu]].contramap(_.fpu)

  given Arbitrary[South] =
    Arbitrary {
      arbitrary[Either[GmosCustomMask, GmosSouthFpu]].map(South(_))
    }

  given Cogen[South] =
    Cogen[Either[GmosCustomMask, GmosSouthFpu]].contramap(_.fpu)

}

object ArbGmosFpu extends ArbGmosFpu
