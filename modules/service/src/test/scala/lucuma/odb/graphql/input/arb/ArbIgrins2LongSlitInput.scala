// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.arb.ArbExposureTimeMode.given
import lucuma.core.model.arb.ArbTelluricType.given
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.arb.ArbSlitTelescopeConfigs.given
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIgrins2LongSlitInput:

  given svcCreate: Arbitrary[Igrins2LongSlitInput.Svc.Create] =
    Arbitrary:
      for
        e  <- arbitrary[Option[TimeSpan]]
        tc <- arbitrary[Option[List[TelescopeConfig]]]
      yield Igrins2LongSlitInput.Svc.Create(e, tc)

  given svcEdit: Arbitrary[Igrins2LongSlitInput.Svc.Edit] =
    Arbitrary:
      for
        e  <- arbitrary[Nullable[TimeSpan]]
        tc <- arbitrary[Nullable[List[TelescopeConfig]]]
      yield Igrins2LongSlitInput.Svc.Edit(e, tc)

  given Arbitrary[Igrins2LongSlitInput.Create] =
    Arbitrary:
      for
        e   <- arbitrary[Option[ExposureTimeMode]]
        svc <- arbitrary[Option[Igrins2LongSlitInput.Svc.Create]]
        tc  <- arbitrary[Option[SlitTelescopeConfigs]]
        tt  <- arbitrary[TelluricType]
      yield Igrins2LongSlitInput.Create(e, svc, tc, tt)

  given Arbitrary[Igrins2LongSlitInput.Edit] =
    Arbitrary:
      for
        e   <- arbitrary[Option[ExposureTimeMode]]
        svc <- arbitrary[Nullable[Igrins2LongSlitInput.Svc.Edit]]
        tc  <- arbitrary[Nullable[SlitTelescopeConfigs]]
        tt  <- arbitrary[Option[TelluricType]]
      yield Igrins2LongSlitInput.Edit(e, svc, tc, tt)

object ArbIgrins2LongSlitInput extends ArbIgrins2LongSlitInput
