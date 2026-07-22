// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.arb.ArbExposureTimeMode.given
import lucuma.core.model.arb.ArbTelluricType.given
import lucuma.core.model.sequence.arb.ArbSlitTelescopeConfigs.given
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIgrins2LongSlitInput:

  given Arbitrary[Igrins2LongSlitInput.Create] =
    Arbitrary:
      for
        e  <- arbitrary[Option[ExposureTimeMode]]
        sv <- arbitrary[Option[Boolean]]
        tc <- arbitrary[Option[SlitTelescopeConfigs]]
        tt <- arbitrary[TelluricType]
      yield Igrins2LongSlitInput.Create(e, sv, tc, tt)

  given Arbitrary[Igrins2LongSlitInput.Edit] =
    Arbitrary:
      for
        e  <- arbitrary[Option[ExposureTimeMode]]
        sv <- arbitrary[Nullable[Boolean]]
        tc <- arbitrary[Nullable[SlitTelescopeConfigs]]
        tt <- arbitrary[Option[TelluricType]]
      yield Igrins2LongSlitInput.Edit(e, sv, tc, tt)

object ArbIgrins2LongSlitInput extends ArbIgrins2LongSlitInput
